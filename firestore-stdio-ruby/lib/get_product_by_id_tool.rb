# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'
require 'json'

# Tool to retrieve a specific product by its ID from Firestore.
class GetProductByIdTool < MCP::Tool
  tool_name 'get_product_by_id'
  description 'Get a single product from the inventory database by its ID'
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
      return db_not_running_response unless client.db_running

      product = client.product_by_id(id)
      return product_not_found_response if product.nil?

      MCP::Tool::Response.new([{ type: 'text', text: JSON.pretty_generate(product) }])
    end

    private

    def db_not_running_response
      MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }])
    end

    def product_not_found_response
      MCP::Tool::Response.new([{ type: 'text', text: 'Product not found.' }])
    end
  end
end
