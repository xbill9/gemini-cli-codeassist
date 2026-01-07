# frozen_string_literal: true

# Redirect stdout to stderr to ensure all output goes to stderr immediately
$stdout.reopen($stderr)

require 'mcp'
require 'rack'
require 'rackup'
require 'json'
require 'puma'
require 'dotenv/load'

require_relative 'lib/mcp_server/config'
require_relative 'lib/mcp_server/database_helper'
require_relative 'lib/mcp_server/tools/get_products'
require_relative 'lib/mcp_server/tools/get_product_by_id'
require_relative 'lib/mcp_server/tools/seed'
require_relative 'lib/mcp_server/tools/reset'
require_relative 'lib/mcp_server/tools/get_root'
require_relative 'lib/mcp_server/tools/check_db'

# Initialize Configuration and Firestore
MCPServer::Config.init_firestore
LOGGER = MCPServer::Config.logger

# Configure MCP
MCP.configure do |config|
  config.protocol_version = '2024-11-05'
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

# Middleware for JSON request logging
class JsonRequestLogger
  def initialize(app, logger)
    @app = app
    @logger = logger
  end

  def call(env)
    status, headers, body = @app.call(env)
    @logger.info(
      type: 'request',
      method: env['REQUEST_METHOD'],
      path: env['PATH_INFO'],
      status: status,
      remote_addr: env['REMOTE_ADDR']
    )
    [status, headers, body]
  end
end

# Initialize MCP Server
server = MCP::Server.new(
  name: 'inventory-server',
  version: '1.0.0',
  tools: [
    MCPServer::Tools::GetProducts,
    MCPServer::Tools::GetProductById,
    MCPServer::Tools::Seed,
    MCPServer::Tools::Reset,
    MCPServer::Tools::GetRoot,
    MCPServer::Tools::CheckDb
  ]
)

# Create the Fixed Streamable HTTP transport
transport = FixedStreamableHTTPTransport.new(server)
server.transport = transport

# Create the Rack application
base_app = proc do |env|
  request = Rack::Request.new(env)
  transport.handle_request(request)
end

# Wrap with JSON logger
app = JsonRequestLogger.new(base_app, LOGGER)

# Run the server using streaming HTTP transport
if __FILE__ == $PROGRAM_NAME
  begin
    port = ENV.fetch('PORT', 8080).to_i
    LOGGER.info "Starting MCP server: #{server.name} (v#{server.version}) on HTTP port #{port}"
    Rackup::Handler.get('puma').run(app, Port: port, Host: '0.0.0.0', Quiet: true)
  rescue Interrupt
    LOGGER.info 'Shutting down MCP server...'
    transport.close
  rescue StandardError => e
    LOGGER.error(message: "Server error: #{e.message}", backtrace: e.backtrace)
    exit 1
  end
end
