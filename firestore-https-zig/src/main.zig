const std = @import("std");
const mcp = @import("mcp");
const fs = @import("firestore.zig");

// Global Firestore client
var firestore_client: ?fs.Firestore = null;
var db_running: bool = false;

fn logError(msg: []const u8) void {
    const LogEntry = struct {
        asctime: []const u8,
        name: []const u8 = "root",
        levelname: []const u8 = "ERROR",
        message: []const u8,
    };

    var time_buf: [64]u8 = undefined;
    const ts = std.posix.clock_gettime(.REALTIME) catch std.posix.timespec{ .sec = 0, .nsec = 0 };
    const formatted_time = std.fmt.bufPrint(&time_buf, "{d}", .{ts.sec}) catch "0";

    const entry = LogEntry{
        .asctime = formatted_time,
        .message = msg,
    };
    _ = entry;

    std.debug.print("{{\"asctime\":\"{s}\",\"name\":\"root\",\"levelname\":\"ERROR\",\"message\":\"{s}\"}}\n", .{ formatted_time, msg });
}

// Structured JSON logging to stderr
fn logInfo(msg: []const u8) void {
    const LogEntry = struct {
        asctime: []const u8,
        name: []const u8 = "root",
        levelname: []const u8 = "INFO",
        message: []const u8,
    };

    var time_buf: [64]u8 = undefined;
    const ts = std.posix.clock_gettime(.REALTIME) catch std.posix.timespec{ .sec = 0, .nsec = 0 };
    const formatted_time = std.fmt.bufPrint(&time_buf, "{d}", .{ts.sec}) catch "0";

    const entry = LogEntry{
        .asctime = formatted_time,
        .message = msg,
    };
    _ = entry;

    // Simplified logging to avoid JSON stringify issues in different Zig versions
    std.debug.print("{{\"asctime\":\"{s}\",\"name\":\"root\",\"levelname\":\"INFO\",\"message\":\"{s}\"}}\n", .{ formatted_time, msg });
}

// Custom HTTP Transport for MCP using standard blocking net
const HttpServerTransport = struct {
    allocator: std.mem.Allocator,
    server: std.net.Server,
    current_conn: ?std.net.Server.Connection = null,
    read_buf: std.ArrayListUnmanaged(u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, port: u16) !Self {
        const address = try std.net.Address.parseIp("0.0.0.0", port);
        const server = try address.listen(.{ .reuse_address = true });
        return .{
            .allocator = allocator,
            .server = server,
            .read_buf = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.server.deinit();
        self.read_buf.deinit(self.allocator);
    }

    pub fn transport(self: *Self) mcp.Transport {
        return .{
            .ptr = self,
            .vtable = &.{
                .send = sendVtable,
                .receive = receiveVtable,
                .close = closeVtable,
            },
        };
    }

    fn sendVtable(ptr: *anyopaque, message: []const u8) mcp.Transport.SendError!void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        if (self.current_conn) |conn| {
            defer {
                conn.stream.close();
                self.current_conn = null;
            }

            const header = std.fmt.allocPrint(self.allocator, "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n", .{message.len}) catch return error.OutOfMemory;
            defer self.allocator.free(header);

            conn.stream.writeAll(header) catch return error.WriteError;
            conn.stream.writeAll(message) catch return error.WriteError;
        }
    }

    fn receiveVtable(ptr: *anyopaque) mcp.Transport.ReceiveError!?[]const u8 {
        const self: *Self = @ptrCast(@alignCast(ptr));

        if (self.current_conn) |conn| {
            conn.stream.close();
            self.current_conn = null;
        }

        const conn = self.server.accept() catch return error.ReadError;

        self.read_buf.clearRetainingCapacity();

        var body_start_index: usize = 0;
        var content_length: usize = 0;
        var headers_parsed = false;

        var buf: [4096]u8 = undefined;
        while (true) {
            const n = conn.stream.read(&buf) catch return error.ReadError;
            if (n == 0) break;

            self.read_buf.appendSlice(self.allocator, buf[0..n]) catch return error.OutOfMemory;

            const slice = self.read_buf.items;
            if (!headers_parsed) {
                if (std.mem.indexOf(u8, slice, "\r\n\r\n")) |idx| {
                    body_start_index = idx + 4;
                    if (findContentLength(slice[0..idx])) |cl| {
                        content_length = cl;
                    }
                    headers_parsed = true;
                }
            }

            if (headers_parsed) {
                if (slice.len >= body_start_index + content_length) {
                    break;
                }
            }
        }

        if (!headers_parsed) {
            conn.stream.close();
            return error.ReadError;
        }

        const body = self.read_buf.items[body_start_index..][0..content_length];

        // Check for Notification
        var is_notification = false;
        {
            const parsed = std.json.parseFromSlice(std.json.Value, self.allocator, body, .{}) catch {
                self.current_conn = conn;
                return body;
            };
            defer parsed.deinit();
            if (parsed.value == .object and parsed.value.object.get("id") == null) {
                is_notification = true;
            }
        }

        if (is_notification) {
            const resp = "HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            conn.stream.writeAll(resp) catch {};
            conn.stream.close();
            self.current_conn = null;
        } else {
            self.current_conn = conn;
        }

        return body;
    }

    fn closeVtable(ptr: *anyopaque) void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        self.server.deinit();
    }

    fn findContentLength(headers: []const u8) ?usize {
        var it = std.mem.splitSequence(u8, headers, "\r\n");
        while (it.next()) |line| {
            const key = "Content-Length:";
            if (line.len > key.len) {
                if (std.ascii.eqlIgnoreCase(line[0..key.len], key)) {
                    const val_str = std.mem.trim(u8, line[key.len..], " ");
                    return std.fmt.parseInt(usize, val_str, 10) catch null;
                }
            }
        }
        return null;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const project_id = std.process.getEnvVarOwned(allocator, "GCLOUD_PROJECT") catch blk: {
        break :blk std.process.getEnvVarOwned(allocator, "GOOGLE_CLOUD_PROJECT") catch {
            logError("GCLOUD_PROJECT or GOOGLE_CLOUD_PROJECT environment variable must be set.");
            break :blk try allocator.dupe(u8, "unknown-project");
        };
    };
    defer allocator.free(project_id);

    if (!std.mem.eql(u8, project_id, "unknown-project")) {
        if (fs.Firestore.init(allocator, project_id)) |client| {
            firestore_client = client;
            db_running = true;
            logInfo("Firestore client initialized successfully.");
        } else |_| {
            logError("Failed to initialize Firestore client (Auth failed?)");
            firestore_client = null;
        }
    }

    var server = mcp.Server.init(.{
        .name = "firestore-https-zig",
        .version = "0.1.0",
        .allocator = allocator,
    });
    defer server.deinit();

    // --- Register Tools ---

    // 1. get_products
    try server.addTool(.{
        .name = "get_products",
        .description = "Get a list of all products from the inventory database",
        .inputSchema = .{ .type = "object", .properties = null },
        .handler = handleGetProducts,
    });

    // 2. get_product_by_id
    {
        var builder = mcp.schema.InputSchemaBuilder.init(allocator);
        defer builder.deinit();
        _ = try builder.addString("id", "The ID of the product to get", true);
        const schema_val = try builder.build();

        var req_list = std.ArrayList([]const u8){};
        defer req_list.deinit(allocator);
        if (schema_val.object.get("required")) |req| {
            if (req == .array) {
                for (req.array.items) |item| {
                    if (item == .string) try req_list.append(allocator, item.string);
                }
            }
        }

        try server.addTool(.{
            .name = "get_product_by_id",
            .description = "Get a single product from the inventory database by its ID",
            .inputSchema = .{
                .type = "object",
                .properties = schema_val.object.get("properties"),
                .required = try req_list.toOwnedSlice(allocator),
            },
            .handler = handleGetProductById,
        });
    }

    // 3. seed
    try server.addTool(.{
        .name = "seed",
        .description = "Seed the inventory database with products.",
        .inputSchema = .{ .type = "object", .properties = null },
        .handler = handleSeed,
    });

    // 4. reset
    try server.addTool(.{
        .name = "reset",
        .description = "Clears all products from the inventory database.",
        .inputSchema = .{ .type = "object", .properties = null },
        .handler = handleReset,
    });

    // 5. check_db
    try server.addTool(.{
        .name = "check_db",
        .description = "Checks if the inventory database is running.",
        .inputSchema = .{ .type = "object", .properties = null },
        .handler = handleCheckDb,
    });

    // 6. get_root
    try server.addTool(.{
        .name = "get_root",
        .description = "Get a greeting from the Cymbal Superstore Inventory API.",
        .inputSchema = .{ .type = "object", .properties = null },
        .handler = handleGetRoot,
    });

    var builder = mcp.schema.InputSchemaBuilder.init(allocator);
    defer builder.deinit();
    _ = try builder.addString("param", "Greeting parameter.", false);
    const schema_value = try builder.build();

    try server.addTool(.{
        .name = "greet",
        .description = "Get a greeting from a local http server.",
        .inputSchema = .{
            .type = "object",
            .properties = schema_value,
        },
        .handler = greetHandler,
    });

    logInfo("Server starting on 0.0.0.0:8080...");

    var http_transport = try HttpServerTransport.init(allocator, 8080);
    defer http_transport.deinit();

    try server.runWithTransport(http_transport.transport());
}

fn greetHandler(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    logInfo("Executed greet tool");

    const param = if (args) |a|
        mcp.tools.getString(a, "param") orelse "Stranger"
    else
        "Stranger";

    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    items[0] = .{ .text = .{ .text = param } };

    return mcp.tools.ToolResult{
        .content = items,
        .is_error = false,
    };
}

fn handleGetProducts(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    _ = args;
    if (!db_running or firestore_client == null) {
        return mcp.tools.errorResult(allocator, "Inventory database is not running.");
    }

    const products = firestore_client.?.getProducts() catch {
        logError("Failed to get products");
        return mcp.tools.errorResult(allocator, "Failed to fetch products from Firestore.");
    };

    var list = std.ArrayList(u8){};
    defer list.deinit(allocator);

    try list.append(allocator, '[');
    for (products, 0..) |p, i| {
        if (i > 0) try list.appendSlice(allocator, ",");
        const json_val = p.toJson(allocator) catch return mcp.tools.ToolError.ExecutionFailed;
        try writeJson(json_val, list.writer(allocator));
    }
    try list.append(allocator, ']');

    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    items[0] = .{ .text = .{ .text = try list.toOwnedSlice(allocator) } };

    return mcp.tools.ToolResult{
        .content = items,
        .is_error = false,
    };
}

fn handleGetProductById(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    if (!db_running or firestore_client == null) {
        return mcp.tools.errorResult(allocator, "Inventory database is not running.");
    }

    const id = mcp.tools.getString(args.?, "id") orelse return mcp.tools.errorResult(allocator, "Missing 'id' parameter.");

    const product = firestore_client.?.getProduct(id) catch {
        return mcp.tools.errorResult(allocator, "Error fetching product.");
    };

    if (product) |p| {
        var list = std.ArrayList(u8){};
        const json_val = p.toJson(allocator) catch return mcp.tools.ToolError.ExecutionFailed;
        try writeJson(json_val, list.writer(allocator));

        var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
        items[0] = .{ .text = .{ .text = try list.toOwnedSlice(allocator) } };
        return mcp.tools.ToolResult{ .content = items, .is_error = false };
    } else {
        return mcp.tools.errorResult(allocator, "Product not found.");
    }
}

fn handleSeed(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    _ = args;
    if (!db_running or firestore_client == null) {
        return mcp.tools.errorResult(allocator, "Inventory database is not running.");
    }

    seedDatabase(allocator) catch {
        logError("Seeding failed");
        return mcp.tools.errorResult(allocator, "Seeding failed.");
    };

    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    items[0] = .{ .text = .{ .text = "Database seeded successfully." } };
    return mcp.tools.ToolResult{ .content = items, .is_error = false };
}

fn handleReset(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    _ = args;
    if (!db_running or firestore_client == null) {
        return mcp.tools.errorResult(allocator, "Inventory database is not running.");
    }

    firestore_client.?.deleteAll() catch {
        return mcp.tools.errorResult(allocator, "Reset failed.");
    };

    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    items[0] = .{ .text = .{ .text = "Database reset successfully." } };
    return mcp.tools.ToolResult{ .content = items, .is_error = false };
}

fn handleCheckDb(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    _ = args;
    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    const msg = if (db_running) "Database running: true" else "Database running: false";
    items[0] = .{ .text = .{ .text = try allocator.dupe(u8, msg) } };
    return mcp.tools.ToolResult{ .content = items, .is_error = false };
}

fn handleGetRoot(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    _ = args;
    var items = allocator.alloc(mcp.types.ContentItem, 1) catch return mcp.tools.ToolError.OutOfMemory;
    items[0] = .{ .text = .{ .text = "🍎 Hello! This is the Cymbal Superstore Inventory API." } };
    return mcp.tools.ToolResult{ .content = items, .is_error = false };
}

fn seedDatabase(allocator: std.mem.Allocator) !void {
    const oldProducts = [_][]const u8{
        "Apples",        "Bananas",   "Milk",        "Whole Wheat Bread", "Eggs",         "Cheddar Cheese",
        "Whole Chicken", "Rice",      "Black Beans", "Bottled Water",     "Apple Juice",  "Cola",
        "Coffee Beans",  "Green Tea", "Watermelon",  "Broccoli",          "Jasmine Rice", "Yogurt",
        "Beef",          "Shrimp",    "Walnuts",     "Sunflower Seeds",   "Fresh Basil",  "Cinnamon",
    };

    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.posix.getrandom(std.mem.asBytes(&seed)) catch {
            seed = 0;
        };
        break :blk seed;
    });
    const rand = prng.random();

    for (oldProducts) |name| {
        const p = createRandomProduct(allocator, rand, name, false);
        try firestore_client.?.createProduct(p);
    }

    const recentProducts = [_][]const u8{
        "Parmesan Crisps",        "Pineapple Kombucha",           "Maple Almond Butter",
        "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
        "Smores Cereal",          "Peanut Butter and Jelly Cups",
    };
    for (recentProducts) |name| {
        const p = createRandomProduct(allocator, rand, name, true);
        try firestore_client.?.createProduct(p);
    }

    const oos = [_][]const u8{ "Wasabi Party Mix", "Jalapeno Seasoning" };
    for (oos) |name| {
        var p = createRandomProduct(allocator, rand, name, true);
        p.quantity = 0;
        try firestore_client.?.createProduct(p);
    }
}

fn createRandomProduct(allocator: std.mem.Allocator, rand: std.Random, name: []const u8, is_recent: bool) fs.Product {
    const price = @as(f64, @floatFromInt(rand.intRangeAtMost(i32, 1, 10)));
    const quantity = rand.intRangeAtMost(i64, 1, if (is_recent) 100 else 500);

    var img = std.ArrayList(u8){};
    img.appendSlice(allocator, "product-images/") catch {};
    for (name) |c| {
        if (c != ' ') {
            img.append(allocator, std.ascii.toLower(c)) catch {};
        }
    }
    img.appendSlice(allocator, ".png") catch {};

    const now = std.time.timestamp();
    var ts_val: i64 = 0;

    if (!is_recent) {
        const offset = rand.intRangeAtMost(i64, 0, 31536000) + 7776000;
        ts_val = now - offset;
    } else {
        const offset = rand.intRangeAtMost(i64, 0, 518400) + 1;
        ts_val = now - offset;
    }

    const ts_str = formatIso(allocator, ts_val) catch "2023-01-01T00:00:00Z";
    const now_str = formatIso(allocator, now) catch "2023-01-01T00:00:00Z";

    return fs.Product{
        .name = name,
        .price = price,
        .quantity = quantity,
        .imgfile = img.toOwnedSlice(allocator) catch "",
        .timestamp = ts_str,
        .actualdateadded = now_str,
    };
}

fn formatIso(allocator: std.mem.Allocator, seconds: i64) ![]const u8 {
    const epoch_seconds = @as(u64, @intCast(if (seconds < 0) 0 else seconds));
    const day_seconds = 86400;
    const days = epoch_seconds / day_seconds;
    const rem_seconds = epoch_seconds % day_seconds;

    const hours = rem_seconds / 3600;
    const rem_seconds_2 = rem_seconds % 3600;
    const minutes = rem_seconds_2 / 60;
    const secs = rem_seconds_2 % 60;

    var year: u64 = 1970;
    var d = days;

    while (true) {
        const leap = (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0);
        const days_in_year: u64 = if (leap) 366 else 365;
        if (d < days_in_year) break;
        d -= days_in_year;
        year += 1;
    }

    var month: u64 = 1;
    const leap = (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0);
    const days_in_months = [_]u64{ 31, if (leap) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    for (days_in_months, 0..) |dim, i| {
        if (d < dim) {
            month = i + 1;
            break;
        }
        d -= dim;
    }
    const day = d + 1;

    return std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z", .{ year, month, day, hours, minutes, secs });
}

fn writeJson(val: std.json.Value, writer: anytype) !void {
    switch (val) {
        .null => try writer.writeAll("null"),
        .bool => |b| try writer.print("{}", .{b}),
        .integer => |i| try writer.print("{}", .{i}),
        .float => |f| try writer.print("{}", .{f}),
        .string => |s| try encodeJsonString(s, writer),
        .number_string => |s| try writer.writeAll(s),
        .array => |arr| {
            try writer.writeByte('[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try writer.writeByte(',');
                try writeJson(item, writer);
            }
            try writer.writeByte(']');
        },
        .object => |obj| {
            try writer.writeByte('{');
            var it = obj.iterator();
            var first = true;
            while (it.next()) |entry| {
                if (!first) try writer.writeByte(',');
                first = false;
                try encodeJsonString(entry.key_ptr.*, writer);
                try writer.writeByte(':');
                try writeJson(entry.value_ptr.*, writer);
            }
            try writer.writeByte('}');
        },
    }
}

fn encodeJsonString(s: []const u8, writer: anytype) !void {
    try writer.writeByte('"');
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x08 => try writer.writeAll("\\b"),
            0x0c => try writer.writeAll("\\f"),
            else => {
                if (c < 0x20) {
                    try writer.writeAll("\\u");
                    try writer.print("{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}
