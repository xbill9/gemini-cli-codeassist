#!/usr/bin/env ruby
# frozen_string_literal: true

require 'mcp'
require 'logger'
require_relative 'lib/greet_tool'

# Set up logging to stderr
# We use stderr because stdout is used for MCP protocol communication
LOGGER = Logger.new($stderr)
LOGGER.level = Logger::INFO

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
    LOGGER.info 'Shutting down MCP server...'
  rescue StandardError => e
    LOGGER.error "Server error: #{e.message}"
    LOGGER.error e.backtrace.join("\n")
    exit 1
  end
end
