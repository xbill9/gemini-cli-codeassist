# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'

class CheckDbTool < MCP::Tool
  description "Checks if the inventory database is running."
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      client = FirestoreClient.instance
      running = client.db_running
      MCP::Tool::Response.new(
        [{ type: 'text', text: "Database running: #{running}" }]
      )
    end
  end
end
