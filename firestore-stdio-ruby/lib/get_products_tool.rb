# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'
require 'json'

class GetProductsTool < MCP::Tool
  description "Get a list of all products from the inventory database"
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      client = FirestoreClient.instance
      unless client.db_running
        # Assuming is_error is supported. If not, the client will just receive the text.
        return MCP::Tool::Response.new(
          [{ type: 'text', text: "Inventory database is not running." }]
        )
      end

      products = client.get_products
      MCP::Tool::Response.new(
        [{ type: 'text', text: JSON.pretty_generate(products) }]
      )
    end
  end
end
