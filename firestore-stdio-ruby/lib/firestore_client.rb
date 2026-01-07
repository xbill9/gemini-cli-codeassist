# frozen_string_literal: true

require 'google/cloud/firestore'
require 'dotenv/load'
require_relative 'inventory_data'
require_relative 'firestore_seeder'

# Client for interacting with Google Cloud Firestore for inventory management.
class FirestoreClient
  include InventoryData
  include FirestoreSeeder

  attr_reader :client, :db_running

  def initialize
    # Assuming environment variables for auth are set or will be handled by library defaults
    @client = Google::Cloud::Firestore.new
    @db_running = true
    LOGGER.info 'Firestore client initialized' if defined?(LOGGER)
  rescue StandardError => e
    LOGGER.error "Failed to initialize Firestore: #{e.message}" if defined?(LOGGER)
    @db_running = false
    @client = nil
  end

  def self.instance
    @instance ||= new
  end

  def products
    return [] unless @db_running

    products = @client.col('inventory').get
    products.map { |doc| doc_to_product(doc) }
  end

  def product_by_id(id)
    return nil unless @db_running

    doc = @client.col('inventory').doc(id).get
    return nil unless doc.exists?

    doc_to_product(doc)
  end

  def reset
    return unless @db_running

    clean_firestore_collection
  end

  private

  def doc_to_product(doc)
    data = doc.data

    {
      id: doc.document_id, name: data[:name], price: data[:price],
      quantity: data[:quantity], imgfile: data[:imgfile],
      timestamp: data[:timestamp], actualdateadded: data[:actualdateadded]
    }
  end

  def clean_firestore_collection
    LOGGER.info 'Cleaning Firestore collection...' if defined?(LOGGER)
    snapshot = @client.col('inventory').get
    batch_delete(snapshot) unless snapshot.empty?
    LOGGER.info 'Firestore collection cleaned.' if defined?(LOGGER)
  end

  def batch_delete(snapshot)
    batch = @client.batch
    snapshot.each_with_index do |doc, i|
      batch.delete(doc.ref)
      if ((i + 1) % 400).zero?
        batch.commit
        batch = @client.batch
      end
    end
    batch.commit unless (snapshot.size % 400).zero?
  end
end
