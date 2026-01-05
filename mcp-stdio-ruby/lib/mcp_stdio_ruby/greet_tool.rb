# frozen_string_literal: true

require 'mcp'
require_relative 'app_logger'

module McpStdioRuby
  # Define the greet tool
  class GreetTool < MCP::Tool
    description 'Get a greeting from a local stdio server.'
    input_schema(
      type: 'object',
      properties: {
        message: {
          type: 'string',
          description: 'The message to repeat.'
        }
      },
      required: ['message']
    )

    class << self
      def call(message:)
        AppLogger.logger.info "GreetTool called with message: #{message}"
        MCP::Tool::Response.new(
          [
            {
              type: 'text',
              text: message
            }
          ]
        )
      end
    end
  end
end
