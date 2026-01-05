# frozen_string_literal: true

require 'spec_helper'

RSpec.describe McpStdioRuby::GreetTool do
  it 'returns a greeting message' do
    response = described_class.call(name: 'World')
    expect(response).to be_a(MCP::Tool::Response)
    expect(response.content.first[:text]).to eq('Hello, World!')
  end
end
