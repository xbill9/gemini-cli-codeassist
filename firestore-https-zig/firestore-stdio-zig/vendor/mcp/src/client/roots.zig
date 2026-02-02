//! MCP Roots Module
//!
//! Provides types and utilities for filesystem roots. Roots define the
//! boundaries within which a server may operate on the client's filesystem,
//! enabling secure and scoped file access.

const std = @import("std");
const types = @import("../protocol/types.zig");

/// A filesystem root that can be exposed to servers.
pub const Root = types.Root;

/// Result of listing available roots.
pub const RootsListResult = struct {
    roots: []const Root,
};

/// Creates a file root from a filesystem path.
pub fn fileRoot(path: []const u8, name: ?[]const u8) Root {
    var uri_buf: [1024]u8 = undefined;
    const uri = std.fmt.bufPrint(&uri_buf, "file://{s}", .{path}) catch path;
    return .{ .uri = uri, .name = name };
}

/// Validates that a URI is a valid root URI (file:// scheme).
pub fn isValidRootUri(uri: []const u8) bool {
    return std.mem.startsWith(u8, uri, "file://");
}

test "fileRoot" {
    const root = fileRoot("/home/user/project", "Project");
    try std.testing.expect(std.mem.startsWith(u8, root.uri, "file://"));
    try std.testing.expectEqualStrings("Project", root.name.?);
}

test "isValidRootUri" {
    try std.testing.expect(isValidRootUri("file:///home/user"));
    try std.testing.expect(!isValidRootUri("http://example.com"));
}
