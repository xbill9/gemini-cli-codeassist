# frozen_string_literal: true

require 'mcp'
require_relative '../main'

# rubocop:disable RSpec/DescribeClass
RSpec.describe 'Inventory Server' do
  # rubocop:enable RSpec/DescribeClass
  describe GetRootTool do
    it 'returns a greeting message', :aggregate_failures do
      response = described_class.call
      expect(response).to be_a(MCP::Tool::Response)
      expect(response.content.first[:text]).to include('Cymbal Superstore Inventory API')
    end
  end

  describe CheckDbTool do
    it 'returns the database status', :aggregate_failures do
      response = described_class.call
      expect(response).to be_a(MCP::Tool::Response)
      expect(response.content.first[:text]).to match(/Database running: (true|false)/)
    end
  end
end
