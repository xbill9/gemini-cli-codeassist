# frozen_string_literal: true

require 'mcp'
require_relative '../config'

module MCPServer
  module Tools
    # Tool to check if the database is running.
    class CheckDb < MCP::Tool
      description 'Checks if the inventory database is running.'
      input_schema(type: 'object', properties: {})

      def self.call(*)
        MCP::Tool::Response.new(
          [{ type: 'text', text: "Database running: #{MCPServer::Config.db_running?}" }]
        )
      end
    end
  end
end
