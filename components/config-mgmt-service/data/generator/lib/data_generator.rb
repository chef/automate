require 'faker'

require_relative 'data_generator/elasticsearch_insert_client'
require_relative 'data_generator/generate_static_functional_data'

module DataGenerator
  # TODO: when adding multi-server support, vary this per server
  ENTERPRISE_DOMAIN_NAME = Faker::Internet.domain_name
end
