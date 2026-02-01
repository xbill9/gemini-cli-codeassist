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
    const formatted_time = std.fmt.bufPrint(&time_buf, "{}", .{0}) catch "0";

    const entry = LogEntry{
        .asctime = formatted_time,
        .message = msg,
    };

    std.debug.print("{f}\n", .{std.json.fmt(entry, .{})});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize MCP server
    var server = mcp.Server.init(.{
        .name = "mcp-stdio-zig",
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
        .description = "Get a greeting from a local stdio server.",
        .inputSchema = .{
            .type = "object",
            .properties = schema_value.object.get("properties"),
        },
        .handler = greetHandler,
    });

    logInfo("Server starting...");

    // Run the server using stdio transport
    try server.run(.{ .stdio = {} });
}

fn greetHandler(allocator: std.mem.Allocator, args: ?std.json.Value) mcp.tools.ToolError!mcp.tools.ToolResult {
    logInfo("Executed greet tool");

    // Extract "param" from arguments
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
