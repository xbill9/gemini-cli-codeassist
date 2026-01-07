#!/usr/bin/env ruby
# frozen_string_literal: true

require 'mcp'
require 'logger'
require 'json'
require_relative 'lib/greet_tool'
require_relative 'lib/get_products_tool'
require_relative 'lib/get_product_by_id_tool'
require_relative 'lib/seed_tool'
require_relative 'lib/reset_tool'
require_relative 'lib/check_db_tool'
require_relative 'lib/get_root_tool'
require_relative 'lib/firestore_client'

# Set up logging to stderr
# We use stderr because stdout is used for MCP protocol communication
LOGGER = Logger.new($stderr)
LOGGER.formatter = proc do |severity, datetime, progname, msg|
  "#{({
    severity: severity,
    timestamp: datetime.iso8601,
    progname: progname,
    message: msg
  }.to_json)}\n"
end
LOGGER.level = Logger::INFO

# Initialize Firestore Client
FirestoreClient.instance

# Initialize MCP Server
server = MCP::Server.new(
  name: 'inventory-server',
  version: '0.1.1',
  tools: [
    GreetTool,
    GetProductsTool,
    GetProductByIdTool,
    SeedTool,
    ResetTool,
    CheckDbTool,
    GetRootTool
  ]
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
