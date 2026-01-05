# frozen_string_literal: true

require 'spec_helper'

RSpec.describe McpStdioRuby::GreetTool do
  it 'returns the provided message' do
    response = described_class.call(message: 'Hello, World!')
    expect(response).to be_a(MCP::Tool::Response)
    expect(response.content.first[:text]).to eq('Hello, World!')
  end
end
