# frozen_string_literal: true

require 'mcp'
require_relative 'app_logger'
require_relative 'greet_tool'

module McpStdioRuby
  # Encapsulates the MCP Server logic
  class Server
    attr_reader :server

    def initialize
      @server = MCP::Server.new(
        name: 'hello-world-server',
        version: '1.0.0',
        tools: [GreetTool]
      )
    end

    def start
      AppLogger.logger.info "Starting MCP server: #{server.name} (v#{server.version})"
      run_transport
    rescue Interrupt
      AppLogger.logger.info 'Shutting down MCP server...'
    rescue StandardError => e
      handle_error(e)
    end

    private

    def run_transport
      MCP::Server::Transports::StdioTransport.new(server).open
    end

    def handle_error(err)
      AppLogger.logger.error "Server error: #{err.message}"
      AppLogger.logger.error err.backtrace.join("\n")
      exit 1
    end
  end
end
