//! MCP Sampling Module
//!
//! Provides types and utilities for LLM sampling requests. Sampling allows
//! servers to request language model completions from the client, enabling
//! AI-powered workflows while keeping the model under client control.

const std = @import("std");
const types = @import("../protocol/types.zig");

/// Request from server to client for an LLM completion.
pub const SamplingRequest = struct {
    messages: []const types.SamplingMessage,
    modelPreferences: ?types.ModelPreferences = null,
    systemPrompt: ?[]const u8 = null,
    temperature: ?f64 = null,
    maxTokens: u32,
    stopSequences: ?[]const []const u8 = null,
    metadata: ?std.json.Value = null,
};

/// Response containing the LLM completion.
pub const SamplingResponse = struct {
    role: []const u8,
    content: types.ContentItem,
    model: []const u8,
    stopReason: ?[]const u8 = null,
};

/// Handler function type for processing sampling requests.
pub const SamplingHandler = *const fn (
    allocator: std.mem.Allocator,
    request: SamplingRequest,
) SamplingError!SamplingResponse;

/// Errors that can occur during sampling.
pub const SamplingError = error{
    RequestDenied,
    ModelUnavailable,
    TokenLimitExceeded,
    Timeout,
    OutOfMemory,
    Unknown,
};

/// Builds a sampling request with messages and token limit.
pub fn buildRequest(messages: []const types.SamplingMessage, maxTokens: u32) SamplingRequest {
    return .{ .messages = messages, .maxTokens = maxTokens };
}

/// Creates a text message for use in sampling requests.
pub fn textMessage(role: []const u8, text: []const u8) types.SamplingMessage {
    return .{ .role = role, .content = .{ .text = .{ .text = text } } };
}

test "buildRequest" {
    const msgs = &[_]types.SamplingMessage{textMessage("user", "Hello")};
    const req = buildRequest(msgs, 100);
    try std.testing.expectEqual(@as(u32, 100), req.maxTokens);
}
