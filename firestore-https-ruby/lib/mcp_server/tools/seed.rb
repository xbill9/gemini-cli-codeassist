# frozen_string_literal: true

require 'mcp'
require_relative '../config'
require_relative '../database_helper'

module MCPServer
  module Tools
    # Tool to seed the inventory database.
    class Seed < MCP::Tool
      description 'Seed the inventory database with products.'
      input_schema(type: 'object', properties: {})

      def self.call(*)
        unless MCPServer::Config.db_running?
          return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], is_error: true)
        end

        MCPServer::DatabaseHelper.seed_database
        MCP::Tool::Response.new(
          [{ type: 'text', text: 'Database seeded successfully.' }]
        )
      end
    end
  end
end
