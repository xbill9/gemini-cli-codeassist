const std = @import("std");

pub const Product = struct {
    id: ?[]const u8 = null,
    name: []const u8,
    price: f64,
    quantity: i64,
    imgfile: []const u8,
    timestamp: []const u8,
    actualdateadded: []const u8,

    pub fn toJson(self: Product, allocator: std.mem.Allocator) !std.json.Value {
        var map = std.json.ObjectMap.init(allocator);
        if (self.id) |id| try map.put("id", .{ .string = id });
        try map.put("name", .{ .string = self.name });
        try map.put("price", .{ .float = self.price });
        try map.put("quantity", .{ .integer = self.quantity });
        try map.put("imgfile", .{ .string = self.imgfile });
        try map.put("timestamp", .{ .string = self.timestamp });
        try map.put("actualdateadded", .{ .string = self.actualdateadded });
        return .{ .object = map };
    }
};

pub const Firestore = struct {
    allocator: std.mem.Allocator,
    project_id: []const u8,
    access_token: []const u8,

    pub fn init(allocator: std.mem.Allocator, project_id: []const u8) !Firestore {
        const token = try getAccessToken(allocator);
        return Firestore{
            .allocator = allocator,
            .project_id = project_id,
            .access_token = token,
        };
    }

    pub fn deinit(self: *Firestore) void {
        self.allocator.free(self.access_token);
    }

    fn getAccessToken(allocator: std.mem.Allocator) ![]const u8 {
        const cmd_result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ "gcloud", "auth", "print-access-token" },
        });
        defer allocator.free(cmd_result.stdout);
        defer allocator.free(cmd_result.stderr);

        if (cmd_result.term.Exited == 0) {
            const token = std.mem.trim(u8, cmd_result.stdout, " \t\r\n");
            if (token.len > 0) {
                return allocator.dupe(u8, token);
            }
        }
        return error.NoCredentialsFound;
    }

    pub fn getProducts(self: *Firestore) ![]Product {
        const url = try std.fmt.allocPrint(self.allocator, "https://firestore.googleapis.com/v1/projects/{s}/databases/(default)/documents/inventory", .{self.project_id});
        defer self.allocator.free(url);

        const body = try self.performRequest("GET", url, null);
        defer self.allocator.free(body);

        var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, body, .{ .ignore_unknown_fields = true });
        defer parsed.deinit();

        var products = std.ArrayList(Product){};

        if (parsed.value == .object) {
            if (parsed.value.object.get("documents")) |docs| {
                if (docs == .array) {
                    for (docs.array.items) |doc| {
                        const p = try parseFirestoreDoc(self.allocator, doc);
                        try products.append(self.allocator, p);
                    }
                }
            }
        }

        return products.toOwnedSlice(self.allocator);
    }

    pub fn getProduct(self: *Firestore, id: []const u8) !?Product {
        const url = try std.fmt.allocPrint(self.allocator, "https://firestore.googleapis.com/v1/projects/{s}/databases/(default)/documents/inventory/{s}", .{ self.project_id, id });
        defer self.allocator.free(url);

        const body = self.performRequest("GET", url, null) catch |err| {
            if (err == error.NotFound) return null;
            return err;
        };
        defer self.allocator.free(body);

        var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, body, .{ .ignore_unknown_fields = true });
        defer parsed.deinit();

        if (parsed.value == .object and parsed.value.object.get("error") != null) {
            return null;
        }

        const p = try parseFirestoreDoc(self.allocator, parsed.value);
        return p;
    }

    pub fn createProduct(self: *Firestore, product: Product) !void {
        var url: []u8 = undefined;
        if (product.id) |id| {
            url = try std.fmt.allocPrint(self.allocator, "https://firestore.googleapis.com/v1/projects/{s}/databases/(default)/documents/inventory/{s}", .{ self.project_id, id });
        } else {
            url = try std.fmt.allocPrint(self.allocator, "https://firestore.googleapis.com/v1/projects/{s}/databases/(default)/documents/inventory", .{self.project_id});
        }
        defer self.allocator.free(url);

        const json_body = try productToFirestoreJson(self.allocator, product);
        defer self.allocator.free(json_body);

        const method = if (product.id != null) "PATCH" else "POST";
        const response = try self.performRequest(method, url, json_body);
        self.allocator.free(response);
    }

    pub fn deleteAll(self: *Firestore) !void {
        const products = try self.getProducts();
        defer self.allocator.free(products);

        for (products) |p| {
            if (p.id) |id| {
                try self.deleteProduct(id);
            }
        }
    }

    pub fn deleteProduct(self: *Firestore, id: []const u8) !void {
        const url = try std.fmt.allocPrint(self.allocator, "https://firestore.googleapis.com/v1/projects/{s}/databases/(default)/documents/inventory/{s}", .{ self.project_id, id });
        defer self.allocator.free(url);

        const res = try self.performRequest("DELETE", url, null);
        self.allocator.free(res);
    }

    fn performRequest(self: *Firestore, method: []const u8, url: []const u8, body: ?[]const u8) ![]u8 {
        var argv = std.ArrayList([]const u8){};
        defer argv.deinit(self.allocator);

        try argv.append(self.allocator, "curl");
        try argv.append(self.allocator, "-s");
        try argv.append(self.allocator, "-X");
        try argv.append(self.allocator, method);

        const auth_header = try std.fmt.allocPrint(self.allocator, "Authorization: Bearer {s}", .{self.access_token});
        defer self.allocator.free(auth_header);
        try argv.append(self.allocator, "-H");
        try argv.append(self.allocator, auth_header);

        if (body != null) {
            try argv.append(self.allocator, "-H");
            try argv.append(self.allocator, "Content-Type: application/json");
            try argv.append(self.allocator, "-d");
            try argv.append(self.allocator, body.?);
        }

        try argv.append(self.allocator, url);

        const result = try std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = argv.items,
        });

        if (result.term.Exited == 0) {
            self.allocator.free(result.stderr);
            return result.stdout;
        } else {
            self.allocator.free(result.stdout);
            self.allocator.free(result.stderr);
            return error.HttpRequestFailed;
        }
    }
};

fn parseFirestoreDoc(allocator: std.mem.Allocator, doc: std.json.Value) !Product {
    if (doc != .object) return error.InvalidFormat;

    const name_path = doc.object.get("name").?.string;
    const doc_id = std.fs.path.basename(name_path);

    const fields = doc.object.get("fields");
    if (fields == null or fields.? != .object) return error.InvalidFormat;
    const f = fields.?.object;

    const name = getString(f, "name");
    const price_val = f.get("price");
    const quantity_val = f.get("quantity");
    const img = getString(f, "imgfile");
    const ts = getString(f, "timestamp");
    const ada = getString(f, "actualdateadded");

    var price: f64 = 0;
    if (price_val) |v| {
        if (v.object.get("integerValue")) |iv| {
            price = try std.fmt.parseFloat(f64, iv.string);
        } else if (v.object.get("doubleValue")) |dv| {
            switch (dv) {
                .float => |fl| price = fl,
                .integer => |i| price = @floatFromInt(i),
                else => {},
            }
        }
    }

    var quantity: i64 = 0;
    if (quantity_val) |v| {
        if (v.object.get("integerValue")) |iv| {
            quantity = try std.fmt.parseInt(i64, iv.string, 10);
        }
    }

    const ts_str = if (ts) |t| t else "";
    const ada_str = if (ada) |a| a else "";

    return Product{
        .id = try allocator.dupe(u8, doc_id),
        .name = try allocator.dupe(u8, name orelse ""),
        .price = price,
        .quantity = quantity,
        .imgfile = try allocator.dupe(u8, img orelse ""),
        .timestamp = try allocator.dupe(u8, ts_str),
        .actualdateadded = try allocator.dupe(u8, ada_str),
    };
}

fn getString(fields: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    if (fields.get(key)) |val| {
        if (val.object.get("stringValue")) |s| return s.string;
        if (val.object.get("timestampValue")) |s| return s.string;
    }
    return null;
}

fn productToFirestoreJson(allocator: std.mem.Allocator, p: Product) ![]u8 {
    var fields = std.json.ObjectMap.init(allocator);
    defer fields.deinit();

    var name_val = std.json.ObjectMap.init(allocator);
    try name_val.put("stringValue", .{ .string = p.name });
    try fields.put("name", .{ .object = name_val });

    var price_val = std.json.ObjectMap.init(allocator);
    try price_val.put("doubleValue", .{ .float = p.price });
    try fields.put("price", .{ .object = price_val });

    var qty_val = std.json.ObjectMap.init(allocator);
    const qty_str = try std.fmt.allocPrint(allocator, "{}", .{p.quantity});
    try qty_val.put("integerValue", .{ .string = qty_str });
    try fields.put("quantity", .{ .object = qty_val });

    var img_val = std.json.ObjectMap.init(allocator);
    try img_val.put("stringValue", .{ .string = p.imgfile });
    try fields.put("imgfile", .{ .object = img_val });

    var ts_val = std.json.ObjectMap.init(allocator);
    try ts_val.put("timestampValue", .{ .string = p.timestamp });
    try fields.put("timestamp", .{ .object = ts_val });

    var ada_val = std.json.ObjectMap.init(allocator);
    try ada_val.put("timestampValue", .{ .string = p.actualdateadded });
    try fields.put("actualdateadded", .{ .object = ada_val });

    var list = std.ArrayList(u8){};
    var writer = list.writer(allocator);
    try writer.writeAll("{\"fields\":");
    try writeJson(.{ .object = fields }, writer);
    try writer.writeByte('}');
    return list.toOwnedSlice(allocator);
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
                    try writer.print("\\u{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}
