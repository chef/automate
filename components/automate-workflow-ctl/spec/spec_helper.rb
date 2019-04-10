
# Ensure that ctl-command relative includes can be found.
$LOAD_PATH.unshift(File.expand_path('../lib', __FILE__))

require 'rspec'

require 'json'
require 'ctl-helpers/delivery_config'

# Wrapper method for tests which require reading /etc/delivery/delivery.rb
def with_delivery_rb(content = "")
  delivery_rb = Tempfile.new("rspec-delivery-rb")
  File.write(delivery_rb, content)
  allow(CtlHelpers::DeliveryConfig).to receive(:delivery_config_path).and_return(delivery_rb.path)
  yield
ensure
  delivery_rb.close
  delivery_rb.unlink
end

def with_attributes_json(attributes_file)
  attributes_json = Tempfile.new(File.basename(attributes_file.path))
  allow(File).to receive(:open).with(attributes_file.path, 'w').
    and_yield(attributes_json)
  attributes_file.write
  attributes_json.rewind
  yield JSON.parse(attributes_json.read)
ensure
  attributes_json.close
  attributes_json.unlink
end

RSpec.configure do |config|
  config.filter_run focus: true
  config.run_all_when_everything_filtered = true
end
