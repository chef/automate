# -*- encoding: utf-8 -*-
# stub: little-plugger 1.1.4 ruby lib

Gem::Specification.new do |s|
  s.name = "little-plugger".freeze
  s.version = "1.1.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Tim Pease".freeze]
  s.date = "2015-08-28"
  s.description = "LittlePlugger is a module that provides Gem based plugin management.\nBy extending your own class or module with LittlePlugger you can easily\nmanage the loading and initializing of plugins provided by other gems.".freeze
  s.email = "tim.pease@gmail.com".freeze
  s.extra_rdoc_files = ["History.txt".freeze, "README.rdoc".freeze]
  s.files = ["History.txt".freeze, "README.rdoc".freeze]
  s.homepage = "http://gemcutter.org/gems/little-plugger".freeze
  s.rdoc_options = ["--main".freeze, "README.rdoc".freeze]
  s.rubygems_version = "3.3.19".freeze
  s.summary = "LittlePlugger is a module that provides Gem based plugin management.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<rspec>.freeze, ["~> 3.3"])
  else
    s.add_dependency(%q<rspec>.freeze, ["~> 3.3"])
  end
end
