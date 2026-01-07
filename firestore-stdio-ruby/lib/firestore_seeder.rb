# frozen_string_literal: true

require_relative 'inventory_data'

# Module for seeding Firestore collection with sample data.
module FirestoreSeeder
  include InventoryData

  def seed
    return unless @db_running

    seed_old_products
    seed_recent_products
    seed_out_of_stock_products
  end

  private

  def seed_old_products
    OLD_PRODUCTS.each do |name|
      data = {
        name: name, price: rand(1..10), quantity: rand(1..500),
        imgfile: "product-images/#{name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(7_776_000_000..31_536_000_000), actualdateadded: Time.now
      }
      LOGGER.info "⬆️ Adding: #{name}" if defined?(LOGGER)
      add_or_update_firestore(data)
    end
  end

  def seed_recent_products
    RECENT_PRODUCTS.each do |name|
      data = {
        name: name, price: rand(1..10), quantity: rand(1..100),
        imgfile: "product-images/#{name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(1..518_400_000), actualdateadded: Time.now
      }
      LOGGER.info "🆕 Adding: #{name}" if defined?(LOGGER)
      add_or_update_firestore(data)
    end
  end

  def seed_out_of_stock_products
    RECENT_PRODUCTS_OUT_OF_STOCK.each do |name|
      data = {
        name: name, price: rand(1..10), quantity: 0,
        imgfile: "product-images/#{name.gsub(/\s/, '').downcase}.png",
        timestamp: Time.now - rand(1..518_400_000), actualdateadded: Time.now
      }
      LOGGER.info "😱 Adding OOS: #{name}" if defined?(LOGGER)
      add_or_update_firestore(data)
    end
  end

  def add_or_update_firestore(product)
    query = @client.col('inventory').where('name', '=', product[:name]).get

    if query.empty?
      @client.col('inventory').add(product)
    else
      query.each { |doc| doc.ref.update(product) }
    end
  end
end
