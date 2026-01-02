# frozen_string_literal: true

require 'google/cloud/firestore'
require 'dotenv/load'

class FirestoreClient
  attr_reader :client, :db_running

  def initialize
    begin
      # Assuming environment variables for auth are set or will be handled by library defaults
      @client = Google::Cloud::Firestore.new
      @db_running = true
      LOGGER.info 'Firestore client initialized' if defined?(LOGGER)
    rescue StandardError => e
      LOGGER.error "Failed to initialize Firestore: #{e.message}" if defined?(LOGGER)
      @db_running = false
      @client = nil
    end
  end

  def self.instance
    @instance ||= new
  end

  def get_products
    return [] unless @db_running

    products = @client.col('inventory').get
    products.map { |doc| doc_to_product(doc) }
  end

  def get_product_by_id(id)
    return nil unless @db_running

    doc = @client.col('inventory').doc(id).get
    return nil unless doc.exists?

    doc_to_product(doc)
  end

  def seed
    return unless @db_running

    init_firestore_collection
  end

  def reset
    return unless @db_running

    clean_firestore_collection
  end

  private

  def doc_to_product(doc)
    data = doc.data
    # data is a Hash with symbol keys if configured, or string keys?
    # Google::Cloud::Firestore returns hash with Symbol keys by default in recent versions or depends on config.
    # Let's assume Symbol keys for now, but handle String keys if needed or check docs.
    # Actually, let's just use data directly.
    
    {
      id: doc.document_id,
      name: data[:name],
      price: data[:price],
      quantity: data[:quantity],
      imgfile: data[:imgfile],
      timestamp: data[:timestamp], # Firestore Timestamp object
      actualdateadded: data[:actualdateadded]
    }
  end

  def init_firestore_collection
    old_products = [
      "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
      "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
      "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
      "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
      "Sunflower Seeds", "Fresh Basil", "Cinnamon"
    ]

    old_products.each do |product_name|
      old_product = {
        name: product_name,
        price: rand(1..10),
        quantity: rand(1..500),
        imgfile: "product-images/#{product_name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(7776000000..31536000000), # Approx logic from TS
        actualdateadded: Time.now
      }
      LOGGER.info "⬆️ Adding (or updating) product in firestore: #{old_product[:name]}" if defined?(LOGGER)
      add_or_update_firestore(old_product)
    end

    recent_products = [
      "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
      "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
      "Smores Cereal", "Peanut Butter and Jelly Cups"
    ]

    recent_products.each do |product_name|
      recent = {
        name: product_name,
        price: rand(1..10),
        quantity: rand(1..100),
        imgfile: "product-images/#{product_name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(1..518400000),
        actualdateadded: Time.now
      }
      LOGGER.info "🆕 Adding (or updating) product in firestore: #{recent[:name]}" if defined?(LOGGER)
      add_or_update_firestore(recent)
    end

    recent_products_out_of_stock = ["Wasabi Party Mix", "Jalapeno Seasoning"]
    recent_products_out_of_stock.each do |product_name|
      oos_product = {
        name: product_name,
        price: rand(1..10),
        quantity: 0,
        imgfile: "product-images/#{product_name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(1..518400000),
        actualdateadded: Time.now
      }
      LOGGER.info "😱 Adding (or updating) out of stock product in firestore: #{oos_product[:name]}" if defined?(LOGGER)
      add_or_update_firestore(oos_product)
    end
  end

  def add_or_update_firestore(product)
    query = @client.col('inventory').where('name', '=', product[:name]).get

    if query.empty?
      @client.col('inventory').add(product)
    else
      query.each do |doc|
        doc.ref.update(product)
      end
    end
  end

  def clean_firestore_collection
    LOGGER.info "Cleaning Firestore collection..." if defined?(LOGGER)
    snapshot = @client.col('inventory').get
    unless snapshot.empty?
      # Batch delete is limited to 500 writes, but here we iterate.
      # Ideally use batch.
      batch = @client.batch
      count = 0
      snapshot.each do |doc|
        batch.delete(doc.ref)
        count += 1
        if count >= 400 # Commit every 400 to be safe
             batch.commit
             batch = @client.batch
             count = 0
        end
      end
      batch.commit if count > 0
    end
    LOGGER.info "Firestore collection cleaned." if defined?(LOGGER)
  end
end
