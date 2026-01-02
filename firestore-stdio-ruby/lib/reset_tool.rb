# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'

class ResetTool < MCP::Tool
  description "Clears all products from the inventory database."
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      client = FirestoreClient.instance
      unless client.db_running
        return MCP::Tool::Response.new(
          [{ type: 'text', text: "Inventory database is not running." }]
        )
      end

      client.reset
      MCP::Tool::Response.new(
        [{ type: 'text', text: "Database reset successfully." }]
      )
    end
  end
end
