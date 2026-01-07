# frozen_string_literal: true

require 'mcp'
require_relative '../config'
require_relative '../database_helper'

module MCPServer
  module Tools
    # Tool to reset the inventory database.
    class Reset < MCP::Tool
      description 'Clears all products from the inventory database.'
      input_schema(type: 'object', properties: {})

      def self.call(*)
        unless MCPServer::Config.db_running?
          return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], is_error: true)
        end

        MCPServer::DatabaseHelper.clean_firestore_collection
        MCP::Tool::Response.new(
          [{ type: 'text', text: 'Database reset successfully.' }]
        )
      end
    end
  end
end
