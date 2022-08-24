# -*- encoding: utf-8 -*-
# frozen_string_literal: true

lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "assert/version"

Gem::Specification.new do |gem|
  gem.name        = "assert"
  gem.version     = Assert::VERSION
  gem.authors     = ["Kelly Redding", "Collin Redding"]
  gem.email       = ["kelly@kellyredding.com", "collin.redding@me.com"]
  gem.summary     = "Assertion style testing framework."
  gem.description = "Assertion style testing framework."
  gem.homepage    = "http://github.com/redding/assert"
  gem.license     = "MIT"

  gem.files = `git ls-files | grep "^[^.]"`.split($INPUT_RECORD_SEPARATOR)

  gem.executables   = gem.files.grep(%r{^bin/}).map{ |f| File.basename(f) }
  gem.test_files    = gem.files.grep(%r{^(test|spec|features)/})
  gem.require_paths = ["lib"]

  gem.required_ruby_version = ">= 2.5"

  gem.add_development_dependency("much-style-guide", ["~> 0.6.7"])

  gem.add_dependency("much-factory",   ["~> 0.2.3"])
  gem.add_dependency("much-not-given", ["~> 0.1.3"])
  gem.add_dependency("much-stub",      ["~> 0.1.10"])
end
