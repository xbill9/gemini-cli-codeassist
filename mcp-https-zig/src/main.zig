const std = @import("std");
const mcp = @import("mcp");

// Structured JSON logging to stderr
fn logInfo(msg: []const u8) void {
    const LogEntry = struct {
        asctime: []const u8,
        name: []const u8 = "root",
        levelname: []const u8 = "INFO",
        message: []const u8,
    };

    var time_buf: [64]u8 = undefined;
    const formatted_time = std.fmt.bufPrint(&time_buf, "{}", .{std.time.timestamp()}) catch "0";

    const entry = LogEntry{
        .asctime = formatted_time,
        .message = msg,
    };

    // Use {f} to print the Formatter returned by std.json.fmt
    std.debug.print("{f}\n", .{std.json.fmt(entry, .{})});
}

// Custom HTTP Transport for MCP
const HttpServerTransport = struct {
    allocator: std.mem.Allocator,
    server: std.net.Server,
    current_conn: ?std.net.Server.Connection = null,
    read_buf: std.ArrayList(u8),
    
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, port: u16) !Self {
        const address = try std.net.Address.parseIp4("0.0.0.0", port);
        const server = try address.listen(.{ .reuse_address = true });
        return .{
            .allocator = allocator,
            .server = server,
            .read_buf = .{}, // Initialize as empty struct
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
            defer conn.stream.close();
            self.current_conn = null;
            
            const header = std.fmt.allocPrint(self.allocator, 
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {d}\r\n\r\n", 
                .{message.len}) catch return error.OutOfMemory;
            defer self.allocator.free(header);
            
            conn.stream.writeAll(header) catch return error.WriteError;
            conn.stream.writeAll(message) catch return error.WriteError;
        }
    }

    fn receiveVtable(ptr: *anyopaque) mcp.Transport.ReceiveError!?[]const u8 {
        const self: *Self = @ptrCast(@alignCast(ptr));
        
        const conn = self.server.accept() catch return error.ReadError;
        self.current_conn = conn;

        self.read_buf.clearRetainingCapacity();
        
        var buf: [1024]u8 = undefined;
        var body_start_index: usize = 0;
        var content_length: usize = 0;
        var headers_parsed = false;
        
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
        
        if (!headers_parsed) return error.ReadError;
        
        return self.read_buf.items[body_start_index..][0..content_length];
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

    // Initialize MCP server
    var server = mcp.Server.init(.{
        .name = "mcp-https-zig",
        .version = "0.1.0",
        .allocator = allocator,
    });
    defer server.deinit();

    // Add "greet" tool
    var builder = mcp.schema.InputSchemaBuilder.init(allocator);
    defer builder.deinit();
    _ = try builder.addString("param", "Greeting parameter.", false);
    const schema_value = try builder.build();

    if (schema_value != .object) {
        logInfo("Error: Schema builder did not produce an object.");
        return error.UnexpectedSchemaType;
    }

    try server.addTool(.{
        .name = "greet",
        .description = "Get a greeting from a local http server.",
        .inputSchema = .{
            .type = "object",
            .properties = schema_value.object.get("properties"),
        },
        .handler = greetHandler,
    });

    logInfo("Server starting on 0.0.0.0:8080 (Custom HTTP Transport)...");

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
    items[0] = .{.text = .{ .text = param }};

    return mcp.tools.ToolResult{
        .content = items,
        .is_error = false,
    };
}
