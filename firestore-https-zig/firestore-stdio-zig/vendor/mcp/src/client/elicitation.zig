//! MCP Elicitation Module
//!
//! Provides types and utilities for server-initiated elicitation requests.
//! Elicitation allows servers to request structured information from users
//! through the client, enabling interactive workflows.

const std = @import("std");

/// Request from server to elicit information from the user.
pub const ElicitationRequest = struct {
    message: []const u8,
    schema: std.json.Value,
};

/// Response to an elicitation request.
pub const ElicitationResponse = struct {
    action: Action,
    content: ?std.json.Value = null,

    pub const Action = enum { accept, decline, cancel };
};

/// Handler function type for processing elicitation requests.
pub const ElicitationHandler = *const fn (
    allocator: std.mem.Allocator,
    request: ElicitationRequest,
) ElicitationError!ElicitationResponse;

/// Errors that can occur during elicitation.
pub const ElicitationError = error{
    UserCancelled,
    Timeout,
    InvalidSchema,
    OutOfMemory,
    Unknown,
};

/// Creates an accept response with the given content.
pub fn accept(content: std.json.Value) ElicitationResponse {
    return .{ .action = .accept, .content = content };
}

/// Creates a decline response indicating the user refused.
pub fn decline() ElicitationResponse {
    return .{ .action = .decline };
}

/// Creates a cancel response indicating the operation was cancelled.
pub fn cancel() ElicitationResponse {
    return .{ .action = .cancel };
}

test "accept response" {
    const resp = accept(.{ .string = "test" });
    try std.testing.expectEqual(ElicitationResponse.Action.accept, resp.action);
}

test "decline response" {
    const resp = decline();
    try std.testing.expectEqual(ElicitationResponse.Action.decline, resp.action);
}
