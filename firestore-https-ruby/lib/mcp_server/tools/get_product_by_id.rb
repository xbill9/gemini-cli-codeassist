# frozen_string_literal: true

require 'mcp'
require 'json'
require_relative '../config'
require_relative '../database_helper'

module MCPServer
  module Tools
    # Tool to retrieve a specific product by its ID.
    class GetProductById < MCP::Tool
      description 'Get a single product from the inventory database by its ID'
      input_schema(
        type: 'object',
        properties: {
          id: { type: 'string', description: 'The ID of the product to get' }
        },
        required: ['id']
      )

      def self.call(id:)
        unless MCPServer::Config.db_running?
          return MCP::Tool::Response.new([{ type: 'text', text: 'Database not running.' }], is_error: true)
        end

        doc = MCPServer::Config.firestore.col('inventory').doc(id).get
        unless doc.exists?
          return MCP::Tool::Response.new([{ type: 'text', text: 'Product not found.' }], is_error: true)
        end

        product = MCPServer::DatabaseHelper.doc_to_product(doc)
        MCP::Tool::Response.new([{ type: 'text', text: JSON.pretty_generate(product) }])
      end
    end
  end
end
