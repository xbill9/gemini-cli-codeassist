# frozen_string_literal: true

require 'mcp'

class GetRootTool < MCP::Tool
  description "Get a greeting from the Cymbal Superstore Inventory API."
  input_schema(type: 'object', properties: {})

  class << self
    def call(*)
      MCP::Tool::Response.new(
        [{ type: 'text', text: "🍎 Hello! This is the Cymbal Superstore Inventory API." }]
      )
    end
  end
end
