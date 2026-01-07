# frozen_string_literal: true

require_relative 'config'

module MCPServer
  # Helper class for database operations.
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
      firestore = MCPServer::Config.firestore
      query = firestore.col('inventory').where('name', '==', product[:name]).get

      if query.empty?
        firestore.col('inventory').add(product)
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
        MCPServer::Config.logger.info "⬆️ Adding (or updating) product in firestore: #{product[:name]}"
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
        MCPServer::Config.logger.info "🆕 Adding (or updating) product in firestore: #{product[:name]}"
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
        MCPServer::Config.logger.info "😱 Adding (or updating) out of stock product in firestore: #{product[:name]}"
        add_or_update_firestore(product)
      end
    end
    # rubocop:enable Metrics/MethodLength

    # rubocop:disable Metrics/AbcSize, Metrics/MethodLength
    def self.clean_firestore_collection
      MCPServer::Config.logger.info 'Cleaning Firestore collection...'
      firestore = MCPServer::Config.firestore
      snapshot = firestore.col('inventory').get
      return if snapshot.empty?

      batch = firestore.batch
      snapshot.each_with_index do |doc, index|
        batch.delete(doc.ref)
        if ((index + 1) % 400).zero?
          batch.commit
          batch = firestore.batch
        end
      end
      batch.commit if (snapshot.size % 400).positive?
      MCPServer::Config.logger.info 'Firestore collection cleaned.'
    end
    # rubocop:enable Metrics/AbcSize, Metrics/MethodLength
  end
end
