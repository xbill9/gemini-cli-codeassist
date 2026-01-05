#!/usr/bin/env ruby
# frozen_string_literal: true

require_relative 'lib/mcp_stdio_ruby'

# Run the server using stdio transport
McpStdioRuby::Server.new.start if __FILE__ == $PROGRAM_NAME
