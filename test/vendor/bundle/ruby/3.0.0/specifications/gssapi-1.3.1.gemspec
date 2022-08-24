# -*- encoding: utf-8 -*-
# stub: gssapi 1.3.1 ruby lib

Gem::Specification.new do |s|
  s.name = "gssapi".freeze
  s.version = "1.3.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Dan Wanek".freeze]
  s.date = "2020-11-24"
  s.description = "    A FFI wrapper around the system GSSAPI library. Please make sure and read the\n    Yard docs or standard GSSAPI documentation if you have any questions.\n\n    There is also a class called GSSAPI::Simple that wraps many of the common features\n    used for GSSAPI.\n".freeze
  s.email = "dan.wanek@gmail.com".freeze
  s.extra_rdoc_files = ["README.md".freeze, "COPYING".freeze, "Changelog.md".freeze]
  s.files = ["COPYING".freeze, "Changelog.md".freeze, "README.md".freeze]
  s.homepage = "http://github.com/zenchild/gssapi".freeze
  s.licenses = ["MIT".freeze]
  s.rdoc_options = ["-x".freeze, "test/".freeze, "-x".freeze, "examples/".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 1.8.7".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "A FFI wrapper around the system GSSAPI library.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<ffi>.freeze, [">= 1.0.1"])
    s.add_development_dependency(%q<pry-byebug>.freeze, [">= 0"])
  else
    s.add_dependency(%q<ffi>.freeze, [">= 1.0.1"])
    s.add_dependency(%q<pry-byebug>.freeze, [">= 0"])
  end
end
