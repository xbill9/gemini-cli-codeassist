# frozen_string_literal: true

require 'logger'

module McpStdioRuby
  # Provides a centralized logger for the application.
  module AppLogger
    class << self
      def logger
        @logger ||= Logger.new($stderr).tap do |l|
          l.level = Logger::INFO
        end
      end

      attr_writer :logger
    end
  end
end
