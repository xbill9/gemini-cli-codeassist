# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'
require 'json'

class GetProductByIdTool < MCP::Tool
  description "Get a single product from the inventory database by its ID"
  input_schema(
    type: 'object',
    properties: {
      id: {
        type: 'string',
        description: 'The ID of the product to get'
      }
    },
    required: ['id']
  )

  class << self
    def call(id:)
      client = FirestoreClient.instance
      unless client.db_running
        return MCP::Tool::Response.new(
          [{ type: 'text', text: "Inventory database is not running." }]
        )
      end

      product = client.get_product_by_id(id)
      if product.nil?
        return MCP::Tool::Response.new(
          [{ type: 'text', text: "Product not found." }]
        )
      end

      MCP::Tool::Response.new(
        [{ type: 'text', text: JSON.pretty_generate(product) }]
      )
    end
  end
end
