# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'

class SeedTool < MCP::Tool
  description "Seed the inventory database with products."
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      client = FirestoreClient.instance
      unless client.db_running
        return MCP::Tool::Response.new(
          [{ type: 'text', text: "Inventory database is not running." }]
        )
      end

      client.seed
      MCP::Tool::Response.new(
        [{ type: 'text', text: "Database seeded successfully." }]
      )
    end
  end
end
