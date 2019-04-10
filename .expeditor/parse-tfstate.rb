#!/usr/bin/env ruby

require 'json'

tfstate = JSON.parse(ARGF.read)

output = []

tfstate["modules"].each do |mod|
  next if mod["resources"].empty?

  if (%w{ title description type fqdn } - mod["outputs"].keys).empty?
    fqdn = [mod.dig("outputs", "fqdn", "value")].flatten.first

    next if fqdn.nil?

    output << {
      id: mod["path"].last,
      title: mod.dig("outputs", "title", "value"),
      description: mod.dig("outputs", "description", "value"),
      type: mod.dig("outputs", "type", "value"),
      tags: mod.dig("outputs", "tags", "value") || [],
      fqdn: fqdn,
    }
  end
end

# Manually add the new Automate Landing Page
output << {
  id: "landing-page",
  title: "Automate Landing Page",
  description: "The landing page where documentation and marketing material is available.",
  type: "server",
  tags: [],
  fqdn: "a2-docs-#{ENV['DNS_SUFFIX']}.cd.chef.co",
}

puts output.to_json
