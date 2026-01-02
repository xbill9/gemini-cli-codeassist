# frozen_string_literal: true

require 'mcp'
require 'logger'
require 'rack'
require 'rackup'
require 'json'
require 'puma'

# Set up logging to stderr
LOGGER = Logger.new($stderr)
LOGGER.level = Logger::INFO

# Configure MCP
MCP.configure do |config|
  config.protocol_version = '2024-11-05'
end

# Define the greet tool
class GreetTool < MCP::Tool
  description 'Get a greeting from a local server.'
  input_schema(
    type: 'object',
    properties: {
      message: {
        type: 'string',
        description: 'The message to repeat.'
      }
    },
    required: ['message']
  )

  class << self
    def call(message:)
      LOGGER.info "GreetTool called with message: #{message}"
      MCP::Tool::Response.new(
        [
          {
            type: 'text',
            text: message
          }
        ]
      )
    end
  end
end

# Custom transport to fix response format for SSE
class FixedStreamableHTTPTransport < MCP::Server::Transports::StreamableHTTPTransport
  private

  def send_response_to_stream(stream, response, _session_id)
    message = JSON.parse(response)
    send_to_stream(stream, message)
    [202, {}, []]
  end
end

# Initialize MCP Server
server = MCP::Server.new(
  name: 'hello-world-server',
  version: '1.0.0',
  tools: [GreetTool]
)

# Create the Fixed Streamable HTTP transport
transport = FixedStreamableHTTPTransport.new(server)
server.transport = transport

# Create the Rack application
app = proc do |env|
  request = Rack::Request.new(env)
  response = transport.handle_request(request)
  response
end

# Run the server using streaming HTTP transport
if __FILE__ == $PROGRAM_NAME
  begin
    port = ENV.fetch('PORT', 8080).to_i
    LOGGER.info "Starting MCP server: #{server.name} (v#{server.version}) on HTTP port #{port}"
    Rackup::Handler.get('puma').run(app, Port: port, Host: '0.0.0.0')
  rescue Interrupt
    LOGGER.info 'Shutting down MCP server...'
    transport.close
  rescue StandardError => e
    LOGGER.error "Server error: #{e.message}"
    LOGGER.error e.backtrace.join("\n")
    exit 1
  end
end
