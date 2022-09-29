# -*- encoding: utf-8 -*-
# stub: train 3.10.1 ruby lib

Gem::Specification.new do |s|
  s.name = "train".freeze
  s.version = "3.10.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "bug_tracker_uri" => "https://github.com/inspec/train/issues", "changelog_uri" => "https://github.com/inspec/train/blob/master/CHANGELOG.md", "homepage_uri" => "https://github.com/inspec/train", "source_code_uri" => "https://github.com/inspec/train" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Chef InSpec Team".freeze]
  s.date = "2022-06-29"
  s.description = "Transport interface to talk to different backends.".freeze
  s.email = ["inspec@chef.io".freeze]
  s.licenses = ["Apache-2.0".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.7".freeze)
  s.rubygems_version = "3.3.19".freeze
  s.summary = "Transport interface to talk to different backends.".freeze

  s.installed_by_version = "3.3.19" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<train-core>.freeze, ["= 3.10.1"])
    s.add_runtime_dependency(%q<train-winrm>.freeze, ["~> 0.2"])
    s.add_runtime_dependency(%q<activesupport>.freeze, [">= 6.0.3.1"])
    s.add_runtime_dependency(%q<inifile>.freeze, ["~> 3.0"])
    s.add_runtime_dependency(%q<azure_graph_rbac>.freeze, ["~> 0.16"])
    s.add_runtime_dependency(%q<azure_mgmt_key_vault>.freeze, ["~> 0.17"])
    s.add_runtime_dependency(%q<azure_mgmt_resources>.freeze, ["~> 0.15"])
    s.add_runtime_dependency(%q<azure_mgmt_security>.freeze, ["~> 0.18"])
    s.add_runtime_dependency(%q<azure_mgmt_storage>.freeze, ["~> 0.18"])
    s.add_runtime_dependency(%q<docker-api>.freeze, [">= 1.26", "< 3.0"])
    s.add_runtime_dependency(%q<google-api-client>.freeze, [">= 0.23.9", "<= 0.52.0"])
    s.add_runtime_dependency(%q<googleauth>.freeze, [">= 0.6.6", "<= 0.14.0"])
  else
    s.add_dependency(%q<train-core>.freeze, ["= 3.10.1"])
    s.add_dependency(%q<train-winrm>.freeze, ["~> 0.2"])
    s.add_dependency(%q<activesupport>.freeze, [">= 6.0.3.1"])
    s.add_dependency(%q<inifile>.freeze, ["~> 3.0"])
    s.add_dependency(%q<azure_graph_rbac>.freeze, ["~> 0.16"])
    s.add_dependency(%q<azure_mgmt_key_vault>.freeze, ["~> 0.17"])
    s.add_dependency(%q<azure_mgmt_resources>.freeze, ["~> 0.15"])
    s.add_dependency(%q<azure_mgmt_security>.freeze, ["~> 0.18"])
    s.add_dependency(%q<azure_mgmt_storage>.freeze, ["~> 0.18"])
    s.add_dependency(%q<docker-api>.freeze, [">= 1.26", "< 3.0"])
    s.add_dependency(%q<google-api-client>.freeze, [">= 0.23.9", "<= 0.52.0"])
    s.add_dependency(%q<googleauth>.freeze, [">= 0.6.6", "<= 0.14.0"])
  end
end
