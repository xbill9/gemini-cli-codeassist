//! MCP Type Definitions
//!
//! Contains all type definitions used throughout the MCP protocol including
//! capability structures, tool definitions, resources, prompts, content types,
//! and other protocol primitives.

const std = @import("std");

/// Request ID can be a string or integer, used to match responses to requests.
pub const RequestId = union(enum) {
    string: []const u8,
    integer: i64,

    pub fn format(
        self: RequestId,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .integer => |i| try writer.print("{d}", .{i}),
        }
    }

    /// Compares two request IDs for equality.
    pub fn eql(self: RequestId, other: RequestId) bool {
        return switch (self) {
            .string => |s| switch (other) {
                .string => |os| std.mem.eql(u8, s, os),
                .integer => false,
            },
            .integer => |i| switch (other) {
                .string => false,
                .integer => |oi| i == oi,
            },
        };
    }
};

/// Token for tracking progress of long-running operations.
pub const ProgressToken = union(enum) {
    string: []const u8,
    integer: i64,
};

/// Cursor for paginating through large result sets.
pub const Cursor = []const u8;

/// Information about an MCP implementation (client or server).
pub const Implementation = struct {
    name: []const u8,
    version: []const u8,
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    icons: ?[]const Icon = null,
    websiteUrl: ?[]const u8 = null,
};

/// Icon definition for visual representation of servers, tools, or resources.
pub const Icon = struct {
    src: []const u8,
    mimeType: ?[]const u8 = null,
    sizes: ?[]const []const u8 = null,
};

/// Server capabilities advertised during initialization.
pub const ServerCapabilities = struct {
    logging: ?LoggingCapability = null,
    prompts: ?PromptsCapability = null,
    resources: ?ResourcesCapability = null,
    tools: ?ToolsCapability = null,
    completions: ?CompletionsCapability = null,
    experimental: ?std.json.Value = null,
};

pub const LoggingCapability = struct {};

pub const PromptsCapability = struct {
    listChanged: bool = false,
};

pub const ResourcesCapability = struct {
    subscribe: bool = false,
    listChanged: bool = false,
};

pub const ToolsCapability = struct {
    listChanged: bool = false,
};

pub const CompletionsCapability = struct {};

/// Client capabilities advertised during initialization.
pub const ClientCapabilities = struct {
    roots: ?RootsCapability = null,
    sampling: ?SamplingCapability = null,
    elicitation: ?ElicitationCapability = null,
    experimental: ?std.json.Value = null,
};

pub const RootsCapability = struct {
    listChanged: bool = false,
};

pub const SamplingCapability = struct {};

pub const ElicitationCapability = struct {
    form: ?struct {} = null,
    url: ?struct {} = null,
};

/// Definition of a tool exposed by a server.
pub const ToolDefinition = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    title: ?[]const u8 = null,
    inputSchema: InputSchema,
    icons: ?[]const Icon = null,
    annotations: ?ToolAnnotations = null,
};

/// JSON Schema for tool input parameters.
pub const InputSchema = struct {
    type: []const u8 = "object",
    properties: ?std.json.Value = null,
    required: ?[]const []const u8 = null,
    description: ?[]const u8 = null,
};

/// Annotations describing tool behavior characteristics.
pub const ToolAnnotations = struct {
    destructive: ?bool = null,
    readOnly: ?bool = null,
    idempotent: ?bool = null,
    requiresConfirmation: ?bool = null,
    cost: ?[]const u8 = null,
};

/// Definition of a resource exposed by a server.
pub const ResourceDefinition = struct {
    uri: []const u8,
    name: []const u8,
    description: ?[]const u8 = null,
    mimeType: ?[]const u8 = null,
    icons: ?[]const Icon = null,
    size: ?u64 = null,
};

/// Resource template for dynamic resources with URI parameters.
pub const ResourceTemplate = struct {
    uriTemplate: []const u8,
    name: []const u8,
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    mimeType: ?[]const u8 = null,
    icons: ?[]const Icon = null,
};

/// Content of a resource read operation.
pub const ResourceContent = struct {
    uri: []const u8,
    mimeType: ?[]const u8 = null,
    text: ?[]const u8 = null,
    blob: ?[]const u8 = null,
};

/// Definition of a prompt template.
pub const PromptDefinition = struct {
    name: []const u8,
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    arguments: ?[]const PromptArgument = null,
    icons: ?[]const Icon = null,
};

/// Argument specification for a prompt.
pub const PromptArgument = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    required: bool = false,
};

/// Message in a prompt response.
pub const PromptMessage = struct {
    role: []const u8,
    content: ContentItem,
};

/// Content item that can be text, image, or embedded resource.
pub const ContentItem = union(enum) {
    text: TextContent,
    image: ImageContent,
    resource: EmbeddedResource,

    /// Returns the text content if this is a text item, null otherwise.
    pub fn asText(self: ContentItem) ?[]const u8 {
        return switch (self) {
            .text => |t| t.text,
            else => null,
        };
    }
};

pub const TextContent = struct {
    type: []const u8 = "text",
    text: []const u8,
};

pub const ImageContent = struct {
    type: []const u8 = "image",
    data: []const u8,
    mimeType: []const u8,
};

pub const EmbeddedResource = struct {
    type: []const u8 = "resource",
    resource: ResourceContent,
};

/// Message for sampling (LLM completion) requests.
pub const SamplingMessage = struct {
    role: []const u8,
    content: ContentItem,
};

/// Model preferences for sampling requests.
pub const ModelPreferences = struct {
    hints: ?[]const ModelHint = null,
    costPriority: ?f64 = null,
    speedPriority: ?f64 = null,
    intelligencePriority: ?f64 = null,
};

/// Hint for model selection during sampling.
pub const ModelHint = struct {
    name: ?[]const u8 = null,
};

/// Filesystem root exposed by a client.
pub const Root = struct {
    uri: []const u8,
    name: ?[]const u8 = null,
};

/// Reference for argument completion.
pub const CompletionRef = union(enum) {
    prompt: PromptRef,
    resource: ResourceRef,

    pub const PromptRef = struct {
        type: []const u8 = "ref/prompt",
        name: []const u8,
    };

    pub const ResourceRef = struct {
        type: []const u8 = "ref/resource",
        uri: []const u8,
    };
};

/// Argument being completed.
pub const CompletionArgument = struct {
    name: []const u8,
    value: []const u8,
};

/// Result of argument completion.
pub const CompletionResult = struct {
    values: []const []const u8,
    total: ?u64 = null,
    hasMore: bool = false,
};

/// JSON-RPC error object.
pub const JsonRpcError = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value = null,
};

test "RequestId equality" {
    const id1 = RequestId{ .integer = 42 };
    const id2 = RequestId{ .integer = 42 };
    const id3 = RequestId{ .string = "abc" };

    try std.testing.expect(id1.eql(id2));
    try std.testing.expect(!id1.eql(id3));
}

test "ContentItem text access" {
    const content = ContentItem{ .text = .{ .text = "Hello" } };
    try std.testing.expectEqualStrings("Hello", content.asText().?);

    const image = ContentItem{ .image = .{ .data = "base64", .mimeType = "image/png" } };
    try std.testing.expect(image.asText() == null);
}
