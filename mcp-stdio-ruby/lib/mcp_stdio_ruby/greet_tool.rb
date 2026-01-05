# frozen_string_literal: true

require 'mcp'
require_relative 'app_logger'

module McpStdioRuby
  # Define the greet tool
  class GreetTool < MCP::Tool
    tool_name 'greet'
    description 'Gives a friendly greeting.'
    input_schema(
      type: 'object',
      properties: {
        name: {
          type: 'string',
          description: 'The name to greet.'
        }
      },
      required: ['name']
    )

    class << self
      def call(name:)
        AppLogger.logger.info "GreetTool called with name: #{name}"
        MCP::Tool::Response.new(
          [
            {
              type: 'text',
              text: "Hello, #{name}!"
            }
          ]
        )
      end
    end
  end
end
