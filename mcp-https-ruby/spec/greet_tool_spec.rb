# frozen_string_literal: true

require 'mcp'
require_relative '../lib/tools/greet_tool'

RSpec.describe GreetTool do
  it 'returns the provided message', :aggregate_failures do
    response = described_class.call(message: 'Hello, World!')
    expect(response).to be_a(MCP::Tool::Response)
    expect(response.content.first[:text]).to eq('Hello, World!')
  end
end
