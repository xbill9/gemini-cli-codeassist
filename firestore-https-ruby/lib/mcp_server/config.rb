# frozen_string_literal: true

require 'logger'
require 'time'
require 'json'
require 'google/cloud/firestore'

module MCPServer
  # Configuration and initialization for the MCP server.
  module Config
    LOGGER = Logger.new($stderr)
    LOGGER.level = Logger::INFO
    LOGGER.formatter = proc do |severity, datetime, _progname, msg|
      log_entry = {
        timestamp: datetime.iso8601,
        level: severity
      }
      if msg.is_a?(Hash)
        log_entry.merge!(msg)
      else
        log_entry[:message] = msg.to_s
      end
      "#{log_entry.to_json}\n"
    end

    def self.logger
      LOGGER
    end

    def self.firestore
      @firestore
    end

    def self.db_running?
      @db_running
    end

    def self.init_firestore
      @firestore = Google::Cloud::Firestore.new
      @db_running = true
      logger.info 'Firestore client initialized'
    rescue StandardError => e
      logger.error "Failed to initialize Firestore: #{e.message}"
      @db_running = false
    end
  end
end
