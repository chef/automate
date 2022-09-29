# -*- encoding: utf-8 -*-
# stub: much-not-given 0.1.3 ruby lib

Gem::Specification.new do |s|
  s.name = "much-not-given".freeze
  s.version = "0.1.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kelly Redding".freeze, "Collin Redding".freeze]
  s.date = "2021-10-29"
  s.description = "Add \"not given\" default values to your objects.".freeze
  s.email = ["kelly@kellyredding.com".freeze, "collin.redding@me.com".freeze]
  s.homepage = "https://github.com/redding/much-not-given".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Add \"not given\" default values to your objects.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<assert>.freeze, ["~> 2.19.2"])
    s.add_development_dependency(%q<much-style-guide>.freeze, ["~> 0.6.0"])
    s.add_runtime_dependency(%q<much-mixin>.freeze, ["~> 0.2.4"])
  else
    s.add_dependency(%q<assert>.freeze, ["~> 2.19.2"])
    s.add_dependency(%q<much-style-guide>.freeze, ["~> 0.6.0"])
    s.add_dependency(%q<much-mixin>.freeze, ["~> 0.2.4"])
  end
end
