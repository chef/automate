# -*- encoding: utf-8 -*-
# stub: winrm-elevated 1.2.3 ruby lib

Gem::Specification.new do |s|
  s.name = "winrm-elevated".freeze
  s.version = "1.2.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Shawn Neal".freeze]
  s.date = "2020-11-11"
  s.description = "Ruby library for running commands via WinRM as elevated through a scheduled task".freeze
  s.email = ["sneal@sneal.net".freeze]
  s.extra_rdoc_files = ["README.md".freeze, "LICENSE".freeze]
  s.files = ["LICENSE".freeze, "README.md".freeze]
  s.homepage = "https://github.com/WinRb/winrm-elevated".freeze
  s.licenses = ["Apache-2.0".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Ruby library for running commands as elevated".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<erubi>.freeze, ["~> 1.8"])
    s.add_runtime_dependency(%q<winrm>.freeze, ["~> 2.0"])
    s.add_runtime_dependency(%q<winrm-fs>.freeze, ["~> 1.0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 10.3", "< 13"])
    s.add_development_dependency(%q<rspec>.freeze, ["~> 3.2"])
    s.add_development_dependency(%q<rubocop>.freeze, ["~> 0.51.0"])
  else
    s.add_dependency(%q<erubi>.freeze, ["~> 1.8"])
    s.add_dependency(%q<winrm>.freeze, ["~> 2.0"])
    s.add_dependency(%q<winrm-fs>.freeze, ["~> 1.0"])
    s.add_dependency(%q<rake>.freeze, [">= 10.3", "< 13"])
    s.add_dependency(%q<rspec>.freeze, ["~> 3.2"])
    s.add_dependency(%q<rubocop>.freeze, ["~> 0.51.0"])
  end
end
