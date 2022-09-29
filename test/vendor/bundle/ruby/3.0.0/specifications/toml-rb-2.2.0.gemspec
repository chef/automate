# -*- encoding: utf-8 -*-
# stub: toml-rb 2.2.0 ruby lib

Gem::Specification.new do |s|
  s.name = "toml-rb".freeze
  s.version = "2.2.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Emiliano Mancuso".freeze, "Lucas Tolchinsky".freeze]
  s.date = "2022-07-15"
  s.description = "A Toml parser using Citrus parsing library. ".freeze
  s.email = ["emiliano.mancuso@gmail.com".freeze, "lucas.tolchinsky@gmail.com".freeze]
  s.homepage = "https://github.com/emancu/toml-rb".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Toml parser in ruby, for ruby.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<citrus>.freeze, ["~> 3.0", "> 3.0"])
    s.add_development_dependency(%q<minitest>.freeze, ["~> 5.7"])
    s.add_development_dependency(%q<standard>.freeze, ["~> 1.4"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
  else
    s.add_dependency(%q<citrus>.freeze, ["~> 3.0", "> 3.0"])
    s.add_dependency(%q<minitest>.freeze, ["~> 5.7"])
    s.add_dependency(%q<standard>.freeze, ["~> 1.4"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
  end
end
