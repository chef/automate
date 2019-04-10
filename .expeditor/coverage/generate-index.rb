#!/usr/bin/env ruby

# This script fetches the full list of S3 'folders' containing code coverage for the last 3 months and then
# generates a simple index page from that.

require 'date'
require 'json'
require 'erb'
require 'net/http'
require 'json'

date = (DateTime.now - 90).strftime("%Y-%m-%d")
folders = `aws --profile chef-cd --output json s3api list-objects-v2 --bucket 'a2-code-coverage.cd.chef.co' --prefix 'automate-ui/' --delimiter "/" --start-after "automate-ui/#{date}"`
@dates = JSON.parse(folders)["CommonPrefixes"].map {|k| k["Prefix"].sub("automate-ui", "").gsub("/", "")}.reverse
@now = DateTime.now.strftime("%Y-%m-%d %H:%M")

@top_level_coverage = {}
@dates.each do |date|
  next if date == "current"
  url = "https://a2-code-coverage.cd.chef.co/automate-ui/#{date}/coverage-summary.json"
  uri = URI(url)
  response = Net::HTTP.get_response(uri)
  if response.code.to_i >= 400
    @top_level_coverage[date] = "null"
  else
    body = JSON.parse(response.body)
    @top_level_coverage[date] = body["total"]["lines"]["pct"]
  end
end

template = File.read('./template.erb')

result = ERB.new(template).result(binding)

File.open('index.html', 'w+') do |f|
  f.write result
end

`aws --profile chef-cd s3 cp index.html s3://a2-code-coverage.cd.chef.co/automate-ui/index.html`
