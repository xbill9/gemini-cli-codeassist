# frozen_string_literal: true

require 'mcp'
require 'json'
require_relative '../config'
require_relative '../database_helper'

module MCPServer
  module Tools
    # Tool to retrieve all products from the inventory.
    class GetProducts < MCP::Tool
      description 'Get a list of all products from the inventory database'
      input_schema(type: 'object', properties: {})

      def self.call(*)
        unless MCPServer::Config.db_running?
          return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], is_error: true)
        end

        firestore = MCPServer::Config.firestore
        products = firestore.col('inventory').get.map { |doc| MCPServer::DatabaseHelper.doc_to_product(doc) }
        MCP::Tool::Response.new(
          [{ type: 'text', text: JSON.pretty_generate(products) }]
        )
      end
    end
  end
end
