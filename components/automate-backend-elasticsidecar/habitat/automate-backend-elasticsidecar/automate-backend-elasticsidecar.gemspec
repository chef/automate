lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require_relative "lib/automate/backend/elasticsidecar/version"

Gem::Specification.new do |spec|
  spec.name          = "automate-backend-elasticsidecar"
  spec.version       = Automate::Backend::Elasticsidecar::VERSION
  spec.authors       = ["Faizan Fulara"]
  spec.email         = ["ffulara@progress.com"]
  spec.description   = %q{Package to control backend-elasticsidecar}
  spec.summary       = spec.description
  spec.licenses      = "Apache-2.0"

  spec.require_paths = ["lib"]

  spec.required_ruby_version = ">= 2.6.0"
  
  spec.add_runtime_dependency 'toml-rb'
  spec.add_runtime_dependency 'mixlib-shellout'
  spec.add_runtime_dependency 'pry'

end
