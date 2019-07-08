#!/usr/bin/env ruby

require "json"
require "net/http"
require "openssl"
require "yaml"

MANIFEST_HOST = "packages.chef.io".freeze
MANIFEST_URL = "/manifests/dev/automate/latest.json".freeze

def get_dev_manifest_sha
  http = Net::HTTP.new(MANIFEST_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new(MANIFEST_URL)
  response = http.request(req)
  latest_release = JSON.parse(response.body)
  latest_release["git_sha"]
end

current_rev = `git rev-parse HEAD`.chomp!
master_rev = `git rev-parse origin/master`.chomp!
dev_rev = get_dev_manifest_sha

# check if the dev_rev is actually from this tree.  we only expect to
# see this until we can get a build through dev.
`git cat-file -e #{dev_rev}`
dev_rev_invalid = !$?.success?

STDERR.puts("===== CHANGED COMPONENTS =====")
STDERR.puts("     CURRENT REV: #{current_rev}")
STDERR.puts("      MASTER REV: #{master_rev}")
STDERR.puts("DEV MANIFEST REV: #{dev_rev}")
if dev_rev_invalid
  STDERR.puts("\nWARNING: Invalid dev rev, did the git history just change?")
elsif master_rev != dev_rev
  STDERR.puts("\nWARNING: Dev manifest behind master (build in progress?)")
end
STDERR.puts("==============================")

if current_rev == dev_rev || dev_rev_invalid
  puts 'HEAD~1...HEAD'
else
  puts "#{dev_rev}...HEAD"
end
