# frozen_string_literal: true

require 'mcp'
require 'logger'
require 'time'
require 'rack'
require 'rackup'
require 'json'
require 'puma'
require 'google/cloud/firestore'
require 'dotenv/load'

# Redirect stdout to stderr to ensure all output goes to stderr
$stdout.reopen($stderr)

# Set up logging to stderr with JSON format
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

# Initialize Firestore
begin
  FIRESTORE = Google::Cloud::Firestore.new
  DB_RUNNING = true
  LOGGER.info 'Firestore client initialized'
rescue StandardError => e
  LOGGER.error "Failed to initialize Firestore: #{e.message}"
  DB_RUNNING = false
end

# Configure MCP
MCP.configure do |config|
  config.protocol_version = '2024-11-05'
end

# Helper class for database operations
class DatabaseHelper
  def self.doc_to_product(doc)
    data = doc.data
    {
      id: doc.document_id,
      name: data[:name],
      price: data[:price],
      quantity: data[:quantity],
      imgfile: data[:imgfile],
      timestamp: data[:timestamp],
      actualdateadded: data[:actualdateadded]
    }
  end

  def self.add_or_update_firestore(product)
    query = FIRESTORE.col('inventory').where('name', '==', product[:name]).get

    if query.empty?
      FIRESTORE.col('inventory').add(product)
    else
      query.each do |doc|
        doc.ref.update(product)
      end
    end
  end

  def self.seed_database
    seed_old_products
    seed_recent_products
    seed_out_of_stock_products
  end

  # rubocop:disable Metrics/MethodLength
  def self.seed_old_products
    names = [
      'Apples', 'Bananas', 'Milk', 'Whole Wheat Bread', 'Eggs', 'Cheddar Cheese',
      'Whole Chicken', 'Rice', 'Black Beans', 'Bottled Water', 'Apple Juice',
      'Cola', 'Coffee Beans', 'Green Tea', 'Watermelon', 'Broccoli',
      'Jasmine Rice', 'Yogurt', 'Beef', 'Shrimp', 'Walnuts',
      'Sunflower Seeds', 'Fresh Basil', 'Cinnamon'
    ]

    names.each do |name|
      product = {
        name: name,
        price: rand(1..10),
        quantity: rand(1..500),
        imgfile: "product-images/#{name.gsub(/\s+/, '').downcase}.png",
        timestamp: Time.now - rand(0..31_536_000) - 7_776_000,
        actualdateadded: Time.now
      }
      LOGGER.info "⬆️ Adding (or updating) product in firestore: #{product[:name]}"
      add_or_update_firestore(product)
    end
  end

  def self.seed_recent_products
    names = [
      'Parmesan Crisps', 'Pineapple Kombucha', 'Maple Almond Butter',
      'Mint Chocolate Cookies', 'White Chocolate Caramel Corn', 'Acai Smoothie Packs',
      'Smores Cereal', 'Peanut Butter and Jelly Cups'
    ]

    names.each do |name|
      product = {
        name: name,
        price: rand(1..10),
        quantity: rand(1..100),
        imgfile: "product-images/#{name.gsub(/\s+/, '').downcase}.png",
        timestamp: Time.now - rand(0..518_400),
        actualdateadded: Time.now
      }
      LOGGER.info "🆕 Adding (or updating) product in firestore: #{product[:name]}"
      add_or_update_firestore(product)
    end
  end

  def self.seed_out_of_stock_products
    names = ['Wasabi Party Mix', 'Jalapeno Seasoning']

    names.each do |name|
      product = {
        name: name,
        price: rand(1..10),
        quantity: 0,
        imgfile: "product-images/#{name.gsub(/\s+/, '').downcase}.png",
        timestamp: Time.now - rand(0..518_400),
        actualdateadded: Time.now
      }
      LOGGER.info "😱 Adding (or updating) out of stock product in firestore: #{product[:name]}"
      add_or_update_firestore(product)
    end
  end

  # rubocop:disable Metrics/AbcSize
  def self.clean_firestore_collection
    LOGGER.info 'Cleaning Firestore collection...'
    snapshot = FIRESTORE.col('inventory').get
    return if snapshot.empty?

    batch = FIRESTORE.batch
    snapshot.each_with_index do |doc, index|
      batch.delete(doc.ref)
      if ((index + 1) % 400).zero?
        batch.commit
        batch = FIRESTORE.batch
      end
    end
    batch.commit if (snapshot.size % 400).positive?
    LOGGER.info 'Firestore collection cleaned.'
  end
  # rubocop:enable Metrics/MethodLength, Metrics/AbcSize
end

# Tool to get all products
class GetProductsTool < MCP::Tool
  description 'Get a list of all products from the inventory database'
  input_schema(type: 'object', properties: {})

  def self.call(*)
    unless DB_RUNNING
      return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], isError: true)
    end

    products = FIRESTORE.col('inventory').get.map { |doc| DatabaseHelper.doc_to_product(doc) }
    MCP::Tool::Response.new(
      [{ type: 'text', text: JSON.pretty_generate(products) }]
    )
  end
end

# Tool to get a product by ID
class GetProductByIdTool < MCP::Tool
  description 'Get a single product from the inventory database by its ID'
  input_schema(
    type: 'object',
    properties: {
      id: { type: 'string', description: 'The ID of the product to get' }
    },
    required: ['id']
  )

  def self.call(id:)
    unless DB_RUNNING
      return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], isError: true)
    end

    doc = FIRESTORE.col('inventory').doc(id).get
    return MCP::Tool::Response.new([{ type: 'text', text: 'Product not found.' }], isError: true) unless doc.exists?

    product = DatabaseHelper.doc_to_product(doc)
    MCP::Tool::Response.new(
      [{ type: 'text', text: JSON.pretty_generate(product) }]
    )
  end
end

# Tool to seed the database
class SeedTool < MCP::Tool
  description 'Seed the inventory database with products.'
  input_schema(type: 'object', properties: {})

  def self.call(*)
    unless DB_RUNNING
      return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], isError: true)
    end

    DatabaseHelper.seed_database
    MCP::Tool::Response.new(
      [{ type: 'text', text: 'Database seeded successfully.' }]
    )
  end
end

# Tool to reset the database
class ResetTool < MCP::Tool
  description 'Clears all products from the inventory database.'
  input_schema(type: 'object', properties: {})

  def self.call(*)
    unless DB_RUNNING
      return MCP::Tool::Response.new([{ type: 'text', text: 'Inventory database is not running.' }], isError: true)
    end

    DatabaseHelper.clean_firestore_collection
    MCP::Tool::Response.new(
      [{ type: 'text', text: 'Database reset successfully.' }]
    )
  end
end

# Tool to get root greeting
class GetRootTool < MCP::Tool
  description 'Get a greeting from the Cymbal Superstore Inventory API.'
  input_schema(type: 'object', properties: {})

  def self.call(*)
    MCP::Tool::Response.new(
      [{ type: 'text', text: '🍎 Hello! This is the Cymbal Superstore Inventory API.' }]
    )
  end
end

# Tool to check database status
class CheckDbTool < MCP::Tool
  description 'Checks if the inventory database is running.'
  input_schema(type: 'object', properties: {})

  def self.call(*)
    MCP::Tool::Response.new(
      [{ type: 'text', text: "Database running: #{DB_RUNNING}" }]
    )
  end
end

# Custom transport to fix response format for SSE
class FixedStreamableHTTPTransport < MCP::Server::Transports::StreamableHTTPTransport
  private

  def send_response_to_stream(stream, response, _session_id)
    message = JSON.parse(response)
    send_to_stream(stream, message)
    [202, {}, []]
  end
end

# Middleware for JSON request logging
class JsonRequestLogger
  def initialize(app, logger)
    @app = app
    @logger = logger
  end

  def call(env)
    status, headers, body = @app.call(env)
    # Only log if it's not a health check or similar if needed, but for now log all
    @logger.info(
      type: 'request',
      method: env['REQUEST_METHOD'],
      path: env['PATH_INFO'],
      status: status,
      remote_addr: env['REMOTE_ADDR']
    )
    [status, headers, body]
  end
end

# Initialize MCP Server
server = MCP::Server.new(
  name: 'inventory-server',
  version: '1.0.0',
  tools: [GetProductsTool, GetProductByIdTool, SeedTool, ResetTool, GetRootTool, CheckDbTool]
)

# Create the Fixed Streamable HTTP transport
transport = FixedStreamableHTTPTransport.new(server)
server.transport = transport

# Create the Rack application
base_app = proc do |env|
  request = Rack::Request.new(env)
  transport.handle_request(request)
end

# Wrap with JSON logger
app = JsonRequestLogger.new(base_app, LOGGER)

# Run the server using streaming HTTP transport
if __FILE__ == $PROGRAM_NAME
  begin
    port = ENV.fetch('PORT', 8080).to_i
    LOGGER.info "Starting MCP server: #{server.name} (v#{server.version}) on HTTP port #{port}"
    Rackup::Handler.get('puma').run(app, Port: port, Host: '0.0.0.0', Quiet: true)
  rescue Interrupt
    LOGGER.info 'Shutting down MCP server...'
    transport.close
  rescue StandardError => e
    LOGGER.error(message: "Server error: #{e.message}", backtrace: e.backtrace)
    exit 1
  end
end
