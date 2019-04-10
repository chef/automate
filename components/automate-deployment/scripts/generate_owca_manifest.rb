#!/usr/bin/env ruby

# -- generate_owca_manifest.rb --
# Generate a manifest for use in OWCA.
#
# Currently (2018-09-26), features that OWCA depends on (e.g S3 backup and
# restore) require that we use versions of the database services that we do not
# yet ship to customers. This script downloads the currently-used manifest and
# replaces the latest database (psql, es) packages that are published to the
# habitat depot. The manifest generated is to be used by Chef and AWS for
# testing OWCA2.
#
# https://gist.github.com/sdelano/8a08c6f5e266548242a303e2c54cd309

require 'json'
require 'net/http'
require 'openssl'

BLDR_API_HOST='bldr.habitat.sh'
HTTP_USER_AGENT='Chef OWCA Manifest Generator'
PACKAGES_HOST='packages.chef.io'

if ARGV[0].nil?
  puts <<EOF
ERROR: Missing argument CHANNEL

Usage: #{$0} CHANNEL

Generate a manifest for OWCA based on the manifest in CHANNEL.
EOF
  exit 1
end

channel = ARGV[0]
unless %w{dev acceptance current}.include?(channel)
  puts "ERROR: CHANNEL must be one of: dev, acceptance, current"
  exit 1
end

# get the latest manifest from packages.chef.io
# returns a hash representation of the manifest
def get_latest_manifest(channel)
  http = Net::HTTP.new(PACKAGES_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new("/manifests/#{channel}/automate/latest.json",
                           {'User-Agent' => HTTP_USER_AGENT})
  resp = http.request(req)
  JSON.parse(resp.body)
end

# get the latest database package from unstable on bldr.habitat.sh
# returns a hash representation of:
# {
#  "name": "automate-elasticsearch",
#  "origin": "chef",
#  "release": "20180911201649",
#  "version": "6.2.2"
# }
def get_latest_database_package(db_name)
  http = Net::HTTP.new(BLDR_API_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new("/v1/depot/channels/chef/unstable/pkgs/#{db_name}/latest",
                           {'User-Agent' => HTTP_USER_AGENT})
  resp = http.request(req)
  JSON.parse(resp.body)["ident"]
end

# modifies the manifest and replaces the existing package with
# the one represented in ident
def replace_package_in_manifest(manifest, ident)
  pkg_key   = "#{ident['origin']}/#{ident['name']}/"
  pkg_ident = "#{ident['origin']}/#{ident['name']}/#{ident['version']}/#{ident['release']}"
  manifest['packages'].delete_if do |pkg|
    pkg.start_with?(pkg_key)
  end
  manifest['packages'] << pkg_ident
end

manifest = get_latest_manifest(channel)

%w{automate-postgresql automate-elasticsearch}.each do |db|
  pkg = get_latest_database_package(db)
  replace_package_in_manifest(manifest, pkg)
end

puts JSON.pretty_generate(manifest)
