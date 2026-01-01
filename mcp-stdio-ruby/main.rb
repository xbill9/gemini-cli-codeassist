# frozen_string_literal: true

require 'mcp'
require 'logger'

# Set up logging to stderr
# We use stderr because stdout is used for MCP protocol communication
LOGGER = Logger.new($stderr)
LOGGER.level = Logger::INFO

# Define the greet tool
class GreetTool < MCP::Tool
  description 'Get a greeting from a local stdio server.'
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

# Initialize MCP Server
server = MCP::Server.new(
  name: 'hello-world-server',
  version: '1.0.0',
  tools: [GreetTool]
)

# Run the server using stdio transport
if __FILE__ == $PROGRAM_NAME
  begin
    LOGGER.info "Starting MCP server: #{server.name} (v#{server.version})"
    transport = MCP::Server::Transports::StdioTransport.new(server)
    transport.open
  rescue Interrupt
    LOGGER.info "Shutting down MCP server..."
  rescue StandardError => e
    LOGGER.error "Server error: #{e.message}"
    LOGGER.error e.backtrace.join("\n")
    exit 1
  end
end
