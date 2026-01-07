# frozen_string_literal: true

require 'mcp'

# Tool to get a greeting from the API.
class GetRootTool < MCP::Tool
  tool_name 'get_root'
  description 'Get a greeting from the Cymbal Superstore Inventory API.'
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      MCP::Tool::Response.new(
        [{ type: 'text', text: '🍎 Hello! This is the Cymbal Superstore Inventory API.' }]
      )
    end
  end
end
