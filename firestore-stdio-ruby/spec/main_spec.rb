# frozen_string_literal: true

require 'mcp'
require_relative '../main'

RSpec.describe GreetTool do
  it 'returns the provided message' do
    response = GreetTool.call(message: 'Hello, World!')
    expect(response).to be_a(MCP::Tool::Response)
    expect(response.content.first[:text]).to eq('Hello, World!')
  end
end
