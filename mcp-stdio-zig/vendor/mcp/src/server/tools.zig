//! MCP Tools Module
//!
//! Provides the Tool primitive for MCP servers. Tools are executable functions
//! that AI applications can invoke to perform actions such as file operations,
//! API calls, calculations, or any other server-side operations.

const std = @import("std");
const types = @import("../protocol/types.zig");
const schema = @import("../protocol/schema.zig");

/// A tool that can be exposed by an MCP server.
pub const Tool = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    title: ?[]const u8 = null,
    inputSchema: ?types.InputSchema = null,
    icons: ?[]const types.Icon = null,
    annotations: ?ToolAnnotations = null,
    handler: *const fn (allocator: std.mem.Allocator, arguments: ?std.json.Value) ToolError!ToolResult,
    user_data: ?*anyopaque = null,
};

/// Annotations describing tool behavior characteristics for client display and safety.
pub const ToolAnnotations = struct {
    destructive: bool = false,
    readOnly: bool = false,
    idempotent: bool = false,
    requiresConfirmation: bool = false,
    cost: ?[]const u8 = null,
};

/// Result of a tool execution.
pub const ToolResult = struct {
    content: []const types.ContentItem,
    is_error: bool = false,
};

/// Errors that can occur during tool execution.
pub const ToolError = error{
    InvalidArguments,
    ExecutionFailed,
    PermissionDenied,
    ResourceNotFound,
    Timeout,
    OutOfMemory,
    Unknown,
};

/// Builder for creating tools with a fluent API.
pub const ToolBuilder = struct {
    allocator: std.mem.Allocator,
    tool: Tool,
    input_builder: ?schema.InputSchemaBuilder = null,

    const Self = @This();

    /// Creates a new tool builder with the given name.
    pub fn init(allocator: std.mem.Allocator, name: []const u8) Self {
        return .{
            .allocator = allocator,
            .tool = .{
                .name = name,
                .handler = defaultHandler,
            },
        };
    }

    /// Sets the tool description.
    pub fn description(self: *Self, desc: []const u8) *Self {
        self.tool.description = desc;
        return self;
    }

    /// Sets the tool display title.
    pub fn title(self: *Self, t: []const u8) *Self {
        self.tool.title = t;
        return self;
    }

    /// Sets the tool handler function.
    pub fn handler(self: *Self, h: *const fn (std.mem.Allocator, ?std.json.Value) ToolError!ToolResult) *Self {
        self.tool.handler = h;
        return self;
    }

    /// Marks the tool as potentially destructive.
    pub fn destructive(self: *Self) *Self {
        if (self.tool.annotations == null) {
            self.tool.annotations = .{};
        }
        self.tool.annotations.?.destructive = true;
        return self;
    }

    /// Marks the tool as read-only (no side effects).
    pub fn readOnly(self: *Self) *Self {
        if (self.tool.annotations == null) {
            self.tool.annotations = .{};
        }
        self.tool.annotations.?.readOnly = true;
        return self;
    }

    /// Marks the tool as idempotent.
    pub fn idempotent(self: *Self) *Self {
        if (self.tool.annotations == null) {
            self.tool.annotations = .{};
        }
        self.tool.annotations.?.idempotent = true;
        return self;
    }

    /// Marks the tool as requiring user confirmation before execution.
    pub fn requireConfirmation(self: *Self) *Self {
        if (self.tool.annotations == null) {
            self.tool.annotations = .{};
        }
        self.tool.annotations.?.requiresConfirmation = true;
        return self;
    }

    /// Builds and returns the final tool.
    pub fn build(self: *Self) Tool {
        return self.tool;
    }

    fn defaultHandler(_: std.mem.Allocator, _: ?std.json.Value) ToolError!ToolResult {
        return .{ .content = &.{} };
    }
};

/// Creates a tool result containing a single text content item.
pub fn textResult(allocator: std.mem.Allocator, text: []const u8) !ToolResult {
    const content = try allocator.alloc(types.ContentItem, 1);
    content[0] = .{ .text = .{ .text = text } };
    return .{ .content = content };
}

/// Creates an error result containing a message.
pub fn errorResult(allocator: std.mem.Allocator, message: []const u8) !ToolResult {
    const content = try allocator.alloc(types.ContentItem, 1);
    content[0] = .{ .text = .{ .text = message } };
    return .{
        .content = content,
        .is_error = true,
    };
}

/// Creates a tool result containing an image.
pub fn imageResult(allocator: std.mem.Allocator, data: []const u8, mimeType: []const u8) !ToolResult {
    const content = try allocator.alloc(types.ContentItem, 1);
    content[0] = .{ .image = .{ .data = data, .mimeType = mimeType } };
    return .{ .content = content };
}

/// Extracts a string argument from tool arguments by key.
pub fn getString(args: ?std.json.Value, key: []const u8) ?[]const u8 {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                if (val == .string) {
                    return val.string;
                }
            }
        }
    }
    return null;
}

/// Extracts an integer argument from tool arguments by key.
pub fn getInteger(args: ?std.json.Value, key: []const u8) ?i64 {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                if (val == .integer) {
                    return val.integer;
                }
            }
        }
    }
    return null;
}

/// Extracts a float argument from tool arguments by key.
pub fn getFloat(args: ?std.json.Value, key: []const u8) ?f64 {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                return switch (val) {
                    .float => val.float,
                    .integer => @floatFromInt(val.integer),
                    else => null,
                };
            }
        }
    }
    return null;
}

/// Extracts a boolean argument from tool arguments by key.
pub fn getBoolean(args: ?std.json.Value, key: []const u8) ?bool {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                if (val == .bool) {
                    return val.bool;
                }
            }
        }
    }
    return null;
}

/// Extracts an array argument from tool arguments by key.
pub fn getArray(args: ?std.json.Value, key: []const u8) ?std.json.Array {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                if (val == .array) {
                    return val.array;
                }
            }
        }
    }
    return null;
}

/// Extracts an object argument from tool arguments by key.
pub fn getObject(args: ?std.json.Value, key: []const u8) ?std.json.ObjectMap {
    if (args) |a| {
        if (a == .object) {
            if (a.object.get(key)) |val| {
                if (val == .object) {
                    return val.object;
                }
            }
        }
    }
    return null;
}

/// Validates a tool name according to MCP naming conventions.
/// Names must be alphanumeric with underscores, starting with a letter.
pub fn isValidToolName(name: []const u8) bool {
    if (name.len == 0 or name.len > 64) return false;
    if (!std.ascii.isAlphabetic(name[0])) return false;
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    }
    return true;
}

test "ToolBuilder" {
    const allocator = std.testing.allocator;

    var builder = ToolBuilder.init(allocator, "test_tool");
    const tool = builder
        .description("A test tool")
        .title("Test Tool")
        .readOnly()
        .build();

    try std.testing.expectEqualStrings("test_tool", tool.name);
    try std.testing.expectEqualStrings("A test tool", tool.description.?);
    try std.testing.expect(tool.annotations.?.readOnly);
}

test "isValidToolName" {
    try std.testing.expect(isValidToolName("my_tool"));
    try std.testing.expect(isValidToolName("getThing"));
    try std.testing.expect(isValidToolName("calculate_sum_123"));

    try std.testing.expect(!isValidToolName(""));
    try std.testing.expect(!isValidToolName("123tool"));
    try std.testing.expect(!isValidToolName("my-tool"));
    try std.testing.expect(!isValidToolName("my tool"));
}

test "argument extraction" {
    const allocator = std.testing.allocator;

    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();

    try obj.put("name", .{ .string = "test" });
    try obj.put("count", .{ .integer = 42 });
    try obj.put("enabled", .{ .bool = true });
    try obj.put("value", .{ .float = 3.14 });

    const value = std.json.Value{ .object = obj };

    try std.testing.expectEqualStrings("test", getString(value, "name").?);
    try std.testing.expectEqual(@as(i64, 42), getInteger(value, "count").?);
    try std.testing.expect(getBoolean(value, "enabled").?);
    try std.testing.expectApproxEqAbs(@as(f64, 3.14), getFloat(value, "value").?, 0.001);

    try std.testing.expect(getString(value, "missing") == null);
}
