const std = @import("std");
pub fn main() !void {
    const LogEntry = struct {
        asctime: []const u8,
        name: []const u8 = "root",
        levelname: []const u8 = "INFO",
        message: []const u8,
    };
    const entry = LogEntry{
        .asctime = "123456",
        .message = "test",
    };
    try std.json.stringify(entry, .{}, std.io.getStdErr().writer());
}