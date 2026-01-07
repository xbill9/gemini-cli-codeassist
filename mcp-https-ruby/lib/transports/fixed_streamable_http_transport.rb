# frozen_string_literal: true

require 'mcp'
require 'json'

# Custom transport to fix response format for SSE
class FixedStreamableHTTPTransport < MCP::Server::Transports::StreamableHTTPTransport
  private

  def send_response_to_stream(stream, response, _session_id)
    message = JSON.parse(response)
    send_to_stream(stream, message)
    [202, {}, []]
  end
end
