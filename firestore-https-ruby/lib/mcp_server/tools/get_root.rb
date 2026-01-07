# frozen_string_literal: true

require 'mcp'

module MCPServer
  module Tools
    # Tool to get the root greeting.
    class GetRoot < MCP::Tool
      description 'Get a greeting from the Cymbal Superstore Inventory API.'
      input_schema(type: 'object', properties: {})

      def self.call(*)
        MCP::Tool::Response.new(
          [{ type: 'text', text: '🍎 Hello! This is the Cymbal Superstore Inventory API.' }]
        )
      end
    end
  end
end
