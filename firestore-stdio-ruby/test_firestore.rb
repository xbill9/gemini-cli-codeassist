# frozen_string_literal: true

require 'google/cloud/firestore'
begin
  firestore = Google::Cloud::Firestore.new(project_id: 'comglitn')
  puts 'Trying to access collection...'
  firestore.col('inventory').get
  puts 'Success!'
rescue StandardError => e
  puts "Error: #{e.class}: #{e.message}"
  puts e.backtrace.first(10).join("\n")
end
