lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'automate/backend/ctl/version'

Gem::Specification.new do |spec|
  spec.name          = "automate-backend-ctl"
  spec.version       = AutomateBackend::Ctl::VERSION
  spec.authors       = ["Jeremy J. Miller"]
  spec.email         = ["jm@chef.io"]
  spec.description   = %q{Commands to control Chef Automate Backend}
  spec.summary       = spec.description
  spec.licenses      = "Apache-2.0"

  spec.files         = %w{LICENSE README.md} + Dir.glob("{bin,doc,helpers,liblugins,spec}/**/*")
  spec.bindir        = "gem_bin"
  spec.executables   = "automate-backend-ctl"
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.required_ruby_version = ">= 2.4.0"

  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
  spec.add_development_dependency "pry"
  spec.add_development_dependency "rb-readline"

  spec.add_runtime_dependency 'httparty'
  spec.add_runtime_dependency 'json'
  spec.add_runtime_dependency 'mixlib-log'
  spec.add_runtime_dependency 'thor'
  spec.add_runtime_dependency 'toml-rb'
  spec.add_runtime_dependency 'knife-ec-backup'
end
