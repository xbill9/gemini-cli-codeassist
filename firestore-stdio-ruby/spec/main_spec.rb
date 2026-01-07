# frozen_string_literal: true

require 'mcp'
require_relative '../main'
require 'json'

RSpec.describe GreetTool do
  it 'returns the provided message' do
    response = GreetTool.call(message: 'Hello, World!')
    expect(response).to be_a(MCP::Tool::Response)
    expect(response.content.first[:text]).to eq('Hello, World!')
  end
end

RSpec.describe GetProductsTool do
  let(:client) { instance_double(FirestoreClient) }

  before do
    allow(FirestoreClient).to receive(:instance).and_return(client)
  end

  it 'returns products when db is running' do
    allow(client).to receive(:db_running).and_return(true)
    allow(client).to receive(:products).and_return([
                                                     { id: '1', name: 'Product 1', price: 10.0,
                                                       timestamp: '2023-01-01' }
                                                   ])

    response = GetProductsTool.call
    expect(response).to be_a(MCP::Tool::Response)
    products = JSON.parse(response.content.first[:text])
    expect(products.first['name']).to eq('Product 1')
  end

  it 'returns an error message when db is not running' do
    allow(client).to receive(:db_running).and_return(false)

    response = GetProductsTool.call
    expect(response.content.first[:text]).to eq('Inventory database is not running.')
  end
end

RSpec.describe GetProductByIdTool do
  let(:client) { instance_double(FirestoreClient) }

  before do
    allow(FirestoreClient).to receive(:instance).and_return(client)
  end

  it 'returns a product when it exists' do
    allow(client).to receive(:db_running).and_return(true)
    allow(client).to receive(:product_by_id).with('1').and_return(
      { id: '1', name: 'Product 1', price: 10.0 }
    )

    response = GetProductByIdTool.call(id: '1')
    expect(response).to be_a(MCP::Tool::Response)
    product = JSON.parse(response.content.first[:text])
    expect(product['name']).to eq('Product 1')
  end

  it 'returns not found when product does not exist' do
    allow(client).to receive(:db_running).and_return(true)
    allow(client).to receive(:product_by_id).with('99').and_return(nil)

    response = GetProductByIdTool.call(id: '99')
    expect(response.content.first[:text]).to eq('Product not found.')
  end
end
