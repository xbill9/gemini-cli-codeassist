//! MCP Protocol Layer
//!
//! Defines core MCP protocol structures, message types, and method constants.
//! Built on JSON-RPC 2.0, this module provides the foundation for MCP communication
//! including initialization, capability negotiation, and all standard MCP methods.

const std = @import("std");
const jsonrpc = @import("jsonrpc.zig");
const types = @import("types.zig");

pub const JsonRpc = jsonrpc;
pub const Types = types;

/// Current MCP protocol version supported by this library.
pub const PROTOCOL_VERSION = "2025-11-25";

/// Legacy alias for compatibility.
pub const VERSION = PROTOCOL_VERSION;

/// List of all MCP protocol versions this library can communicate with.
pub const SUPPORTED_VERSIONS = [_][]const u8{
    "2025-11-25",
    "2025-06-18",
    "2025-03-26",
    "2024-11-05",
};

/// JSON-RPC version used by MCP.
pub const JSONRPC_VERSION = "2.0";

/// All MCP method names as defined in the protocol specification.
pub const Method = enum {
    initialize,
    @"notifications/initialized",
    ping,
    @"tools/list",
    @"tools/call",
    @"notifications/tools/list_changed",
    @"resources/list",
    @"resources/read",
    @"resources/subscribe",
    @"resources/unsubscribe",
    @"resources/templates/list",
    @"notifications/resources/list_changed",
    @"notifications/resources/updated",
    @"prompts/list",
    @"prompts/get",
    @"notifications/prompts/list_changed",
    @"logging/setLevel",
    @"notifications/message",
    @"sampling/createMessage",
    @"elicitation/create",
    @"roots/list",
    @"notifications/roots/list_changed",
    @"completion/complete",
    @"notifications/progress",
    @"notifications/cancelled",

    /// Returns the string representation of the method name.
    pub fn toString(self: Method) []const u8 {
        return @tagName(self);
    }

    /// Parses a method name string into the corresponding enum value.
    pub fn fromString(str: []const u8) ?Method {
        inline for (std.meta.fields(Method)) |field| {
            if (std.mem.eql(u8, field.name, str)) {
                return @enumFromInt(field.value);
            }
        }
        return null;
    }
};

/// Parameters for the initialize request.
pub const InitializeParams = struct {
    protocolVersion: []const u8,
    capabilities: types.ClientCapabilities,
    clientInfo: types.Implementation,
};

/// Result of a successful initialize request.
pub const InitializeResult = struct {
    protocolVersion: []const u8,
    capabilities: types.ServerCapabilities,
    serverInfo: types.Implementation,
    instructions: ?[]const u8 = null,
};

/// Result of listing available tools.
pub const ToolsListResult = struct {
    tools: []const types.ToolDefinition,
    nextCursor: ?[]const u8 = null,
};

/// Parameters for calling a tool.
pub const ToolCallParams = struct {
    name: []const u8,
    arguments: ?std.json.Value = null,
};

/// Result of a tool call.
pub const ToolCallResult = struct {
    content: []const types.ContentItem,
    isError: bool = false,
};

/// Result of listing available resources.
pub const ResourcesListResult = struct {
    resources: []const types.ResourceDefinition,
    nextCursor: ?[]const u8 = null,
};

/// Parameters for reading a resource.
pub const ResourcesReadParams = struct {
    uri: []const u8,
};

/// Result of reading a resource.
pub const ResourcesReadResult = struct {
    contents: []const types.ResourceContent,
};

/// Result of listing resource templates.
pub const ResourceTemplatesListResult = struct {
    resourceTemplates: []const types.ResourceTemplate,
    nextCursor: ?[]const u8 = null,
};

/// Result of listing available prompts.
pub const PromptsListResult = struct {
    prompts: []const types.PromptDefinition,
    nextCursor: ?[]const u8 = null,
};

/// Parameters for fetching a prompt.
pub const PromptsGetParams = struct {
    name: []const u8,
    arguments: ?std.json.Value = null,
};

/// Result of fetching a prompt.
pub const PromptsGetResult = struct {
    description: ?[]const u8 = null,
    messages: []const types.PromptMessage,
};

/// Parameters for creating a sampling message (LLM completion request).
pub const SamplingCreateMessageParams = struct {
    messages: []const types.SamplingMessage,
    modelPreferences: ?types.ModelPreferences = null,
    systemPrompt: ?[]const u8 = null,
    includeContext: ?[]const u8 = null,
    temperature: ?f64 = null,
    maxTokens: u32,
    stopSequences: ?[]const []const u8 = null,
    metadata: ?std.json.Value = null,
};

/// Result of a sampling message request.
pub const SamplingCreateMessageResult = struct {
    role: []const u8,
    content: types.ContentItem,
    model: []const u8,
    stopReason: ?[]const u8 = null,
};

/// Parameters for creating an elicitation request.
pub const ElicitationCreateParams = struct {
    message: []const u8,
    schema: std.json.Value,
};

/// Result of an elicitation request.
pub const ElicitationCreateResult = struct {
    action: []const u8,
    content: ?std.json.Value = null,
};

/// Log severity levels following syslog conventions.
pub const LogLevel = enum {
    debug,
    info,
    notice,
    warning,
    @"error",
    critical,
    alert,
    emergency,

    pub fn toString(self: LogLevel) []const u8 {
        return @tagName(self);
    }
};

/// Parameters for setting the log level.
pub const SetLogLevelParams = struct {
    level: []const u8,
};

/// Log message notification payload.
pub const LogMessageNotification = struct {
    level: []const u8,
    logger: ?[]const u8 = null,
    data: std.json.Value,
};

/// Progress notification payload for long-running operations.
pub const ProgressNotification = struct {
    progressToken: types.ProgressToken,
    progress: f64,
    total: ?f64 = null,
    message: ?[]const u8 = null,
};

/// Cancellation notification payload.
pub const CancelledNotification = struct {
    requestId: types.RequestId,
    reason: ?[]const u8 = null,
};

/// Result of listing filesystem roots.
pub const RootsListResult = struct {
    roots: []const types.Root,
};

/// Parameters for argument completion.
pub const CompletionCompleteParams = struct {
    ref: types.CompletionRef,
    argument: types.CompletionArgument,
};

/// Result of argument completion.
pub const CompletionCompleteResult = struct {
    completion: types.CompletionResult,
};

/// Builds an initialize request message.
pub fn buildInitializeRequest(
    id: types.RequestId,
    params: InitializeParams,
) jsonrpc.Request {
    return jsonrpc.Request{
        .id = id,
        .method = Method.initialize.toString(),
        .params = serializeParams(params),
    };
}

/// Builds an initialize response message.
pub fn buildInitializeResponse(
    id: types.RequestId,
    result: InitializeResult,
) jsonrpc.Response {
    return jsonrpc.Response{
        .id = id,
        .result = serializeResult(result),
    };
}

/// Builds a tools/list request message.
pub fn buildToolsListRequest(id: types.RequestId) jsonrpc.Request {
    return jsonrpc.Request{
        .id = id,
        .method = Method.@"tools/list".toString(),
        .params = null,
    };
}

/// Builds a tools/call request message.
pub fn buildToolCallRequest(
    id: types.RequestId,
    params: ToolCallParams,
) jsonrpc.Request {
    return jsonrpc.Request{
        .id = id,
        .method = Method.@"tools/call".toString(),
        .params = serializeParams(params),
    };
}

fn serializeParams(value: anytype) ?std.json.Value {
    _ = value;
    return null;
}

fn serializeResult(value: anytype) ?std.json.Value {
    _ = value;
    return null;
}

test "Method enum" {
    try std.testing.expectEqualStrings("initialize", Method.initialize.toString());
    try std.testing.expectEqualStrings("tools/list", Method.@"tools/list".toString());
}

test "LogLevel enum" {
    try std.testing.expectEqualStrings("debug", LogLevel.debug.toString());
    try std.testing.expectEqualStrings("error", LogLevel.@"error".toString());
}
