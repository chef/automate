# -*- encoding: utf-8 -*-
# stub: assert 2.19.8 ruby lib

Gem::Specification.new do |s|
  s.name = "assert".freeze
  s.version = "2.19.8"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kelly Redding".freeze, "Collin Redding".freeze]
  s.date = "2022-01-23"
  s.description = "Assertion style testing framework.".freeze
  s.email = ["kelly@kellyredding.com".freeze, "collin.redding@me.com".freeze]
  s.executables = ["assert".freeze]
  s.files = ["bin/assert".freeze]
  s.homepage = "http://github.com/redding/assert".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Assertion style testing framework.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<much-style-guide>.freeze, ["~> 0.6.7"])
    s.add_runtime_dependency(%q<much-factory>.freeze, ["~> 0.2.3"])
    s.add_runtime_dependency(%q<much-not-given>.freeze, ["~> 0.1.3"])
    s.add_runtime_dependency(%q<much-stub>.freeze, ["~> 0.1.10"])
  else
    s.add_dependency(%q<much-style-guide>.freeze, ["~> 0.6.7"])
    s.add_dependency(%q<much-factory>.freeze, ["~> 0.2.3"])
    s.add_dependency(%q<much-not-given>.freeze, ["~> 0.1.3"])
    s.add_dependency(%q<much-stub>.freeze, ["~> 0.1.10"])
  end
end
