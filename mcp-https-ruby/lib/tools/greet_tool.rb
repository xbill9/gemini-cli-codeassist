# frozen_string_literal: true

require 'mcp'
require 'logger'

# Define the greet tool
class GreetTool < MCP::Tool
  tool_name 'greet'
  description 'Get a greeting from a local server.'
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
      logger.info "GreetTool called with message: #{message}"
      MCP::Tool::Response.new(
        [
          {
            type: 'text',
            text: message
          }
        ]
      )
    end

    private

    def logger
      @logger ||= defined?(LOGGER) ? LOGGER : Logger.new($stderr)
    end
  end
end
