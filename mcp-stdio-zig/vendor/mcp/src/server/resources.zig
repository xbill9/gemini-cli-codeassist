//! MCP Resources Module
//!
//! Provides the Resource primitive for MCP servers. Resources are read-only
//! data sources that provide contextual information to AI applications,
//! such as files, database records, or API responses.

const std = @import("std");
const types = @import("../protocol/types.zig");

/// A resource exposed by an MCP server.
pub const Resource = struct {
    uri: []const u8,
    name: []const u8,
    description: ?[]const u8 = null,
    mimeType: ?[]const u8 = null,
    icons: ?[]const types.Icon = null,
    size: ?u64 = null,
    handler: *const fn (allocator: std.mem.Allocator, uri: []const u8) ResourceError!ResourceContent,
    user_data: ?*anyopaque = null,
};

/// Content returned when reading a resource.
pub const ResourceContent = struct {
    uri: []const u8,
    mimeType: ?[]const u8 = null,
    text: ?[]const u8 = null,
    blob: ?[]const u8 = null,
};

/// Resource template for dynamic resources with URI parameters.
pub const ResourceTemplate = struct {
    uriTemplate: []const u8,
    name: []const u8,
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    mimeType: ?[]const u8 = null,
    icons: ?[]const types.Icon = null,
};

/// Errors that can occur during resource operations.
pub const ResourceError = error{
    NotFound,
    AccessDenied,
    ReadFailed,
    InvalidUri,
    OutOfMemory,
    Unknown,
};

/// Builder for creating resources with a fluent API.
pub const ResourceBuilder = struct {
    allocator: std.mem.Allocator,
    resource: Resource,

    /// Creates a new resource builder with the given URI and name.
    pub fn init(allocator: std.mem.Allocator, uri: []const u8, name: []const u8) ResourceBuilder {
        return .{
            .allocator = allocator,
            .resource = .{ .uri = uri, .name = name, .handler = defaultHandler },
        };
    }

    /// Sets the resource description.
    pub fn description(self: *ResourceBuilder, desc: []const u8) *ResourceBuilder {
        self.resource.description = desc;
        return self;
    }

    /// Sets the resource MIME type.
    pub fn mimeType(self: *ResourceBuilder, mime: []const u8) *ResourceBuilder {
        self.resource.mimeType = mime;
        return self;
    }

    /// Sets the resource handler function.
    pub fn handler(self: *ResourceBuilder, h: *const fn (std.mem.Allocator, []const u8) ResourceError!ResourceContent) *ResourceBuilder {
        self.resource.handler = h;
        return self;
    }

    /// Builds and returns the final resource.
    pub fn build(self: *ResourceBuilder) Resource {
        return self.resource;
    }

    fn defaultHandler(_: std.mem.Allocator, uri: []const u8) ResourceError!ResourceContent {
        return .{ .uri = uri };
    }
};

/// Extracts the scheme portion from a URI (e.g., "file" from "file:///path").
pub fn getUriScheme(uri: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, uri, "://")) |index| return uri[0..index];
    return null;
}

/// Extracts the path portion from a URI (e.g., "/path" from "file:///path").
pub fn getUriPath(uri: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, uri, "://")) |index| return uri[index + 3 ..];
    return null;
}

/// Detects the MIME type based on file extension.
pub fn detectMimeType(path: []const u8) []const u8 {
    const ext = std.fs.path.extension(path);
    if (std.mem.eql(u8, ext, ".txt")) return "text/plain";
    if (std.mem.eql(u8, ext, ".json")) return "application/json";
    if (std.mem.eql(u8, ext, ".html")) return "text/html";
    if (std.mem.eql(u8, ext, ".md")) return "text/markdown";
    if (std.mem.eql(u8, ext, ".png")) return "image/png";
    if (std.mem.eql(u8, ext, ".jpg")) return "image/jpeg";
    return "application/octet-stream";
}

test "ResourceBuilder" {
    const allocator = std.testing.allocator;
    var builder = ResourceBuilder.init(allocator, "file:///test.txt", "Test");
    const resource = builder.description("A test").mimeType("text/plain").build();
    try std.testing.expectEqualStrings("file:///test.txt", resource.uri);
}

test "getUriScheme" {
    try std.testing.expectEqualStrings("file", getUriScheme("file:///path").?);
}
