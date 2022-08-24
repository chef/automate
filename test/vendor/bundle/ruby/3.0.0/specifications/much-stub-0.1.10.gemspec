# -*- encoding: utf-8 -*-
# stub: much-stub 0.1.10 ruby lib

Gem::Specification.new do |s|
  s.name = "much-stub".freeze
  s.version = "0.1.10"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kelly Redding".freeze, "Collin Redding".freeze]
  s.date = "2022-01-23"
  s.description = "Stubbing API for replacing method calls on objects in test runs.".freeze
  s.email = ["kelly@kellyredding.com".freeze, "collin.redding@me.com".freeze]
  s.homepage = "https://github.com/redding/much-stub".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Stubbing API for replacing method calls on objects in test runs.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<assert>.freeze, ["~> 2.19.7"])
    s.add_development_dependency(%q<much-style-guide>.freeze, ["~> 0.6.7"])
  else
    s.add_dependency(%q<assert>.freeze, ["~> 2.19.7"])
    s.add_dependency(%q<much-style-guide>.freeze, ["~> 0.6.7"])
  end
end
