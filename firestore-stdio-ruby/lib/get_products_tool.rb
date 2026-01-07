# frozen_string_literal: true

require 'mcp'
require_relative 'firestore_client'
require 'json'

# Tool to retrieve all products from Firestore.
class GetProductsTool < MCP::Tool
  tool_name 'get_products'
  description 'Get a list of all products from the inventory database'
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      client = FirestoreClient.instance
      return db_not_running_response unless client.db_running

      products = format_products(client.products)
      MCP::Tool::Response.new([{ type: 'text', text: products.to_json }])
    rescue StandardError => e
      error_response(e)
    end

    private

    def format_products(products)
      products.map do |p|
        p[:timestamp] = p[:timestamp].to_s if p[:timestamp]
        p[:actualdateadded] = p[:actualdateadded].to_s if p[:actualdateadded]
        p
      end
    end

    def db_not_running_response
      MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }])
    end

    def error_response(error)
      MCP::Tool::Response.new(
        [{ type: 'text', text: "Error getting products: #{error.message}" }],
        is_error: true
      )
    end
  end
end
