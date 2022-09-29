# -*- encoding: utf-8 -*-
# stub: citrus 3.0.2 ruby lib

Gem::Specification.new do |s|
  s.name = "citrus".freeze
  s.version = "3.0.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Michael Jackson".freeze]
  s.date = "2015-02-19"
  s.description = "Parsing Expressions for Ruby".freeze
  s.email = "mjijackson@gmail.com".freeze
  s.extra_rdoc_files = ["README.md".freeze, "CHANGES".freeze]
  s.files = ["CHANGES".freeze, "README.md".freeze]
  s.homepage = "http://mjackson.github.io/citrus".freeze
  s.rdoc_options = ["--line-numbers".freeze, "--inline-source".freeze, "--title".freeze, "Citrus".freeze, "--main".freeze, "Citrus".freeze]
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Parsing Expressions for Ruby".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
  else
    s.add_dependency(%q<rake>.freeze, [">= 0"])
  end
end
