# -*- encoding: utf-8 -*-
# stub: docker-api 2.2.0 ruby lib

Gem::Specification.new do |s|
  s.name = "docker-api".freeze
  s.version = "2.2.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Swipely, Inc.".freeze]
  s.date = "2021-07-07"
  s.description = "A simple REST client for the Docker Remote API".freeze
  s.email = "tomhulihan@swipely.com bright@swipely.com toddlunter@swipely.com".freeze
  s.homepage = "https://github.com/swipely/docker-api".freeze
  s.licenses = ["MIT".freeze]
  s.rubygems_version = "3.3.19".freeze
  s.summary = "A simple REST client for the Docker Remote API".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<excon>.freeze, [">= 0.47.0"])
    s.add_runtime_dependency(%q<multi_json>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<rspec>.freeze, ["~> 3.0"])
    s.add_development_dependency(%q<rspec-its>.freeze, [">= 0"])
    s.add_development_dependency(%q<cane>.freeze, [">= 0"])
    s.add_development_dependency(%q<pry>.freeze, [">= 0"])
    s.add_development_dependency(%q<single_cov>.freeze, [">= 0"])
    s.add_development_dependency(%q<webmock>.freeze, [">= 0"])
    s.add_development_dependency(%q<parallel>.freeze, [">= 0"])
  else
    s.add_dependency(%q<excon>.freeze, [">= 0.47.0"])
    s.add_dependency(%q<multi_json>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<rspec>.freeze, ["~> 3.0"])
    s.add_dependency(%q<rspec-its>.freeze, [">= 0"])
    s.add_dependency(%q<cane>.freeze, [">= 0"])
    s.add_dependency(%q<pry>.freeze, [">= 0"])
    s.add_dependency(%q<single_cov>.freeze, [">= 0"])
    s.add_dependency(%q<webmock>.freeze, [">= 0"])
    s.add_dependency(%q<parallel>.freeze, [">= 0"])
  end
end
