#!/usr/bin/env ruby
 
require "json"
require "net/http"
require "openssl"
require "yaml"
require "open3"  
 
MANIFEST_HOST = "packages.chef.io".freeze
MANIFEST_URL = "/manifests/dev/automate/latest_semver.json".freeze
 
# Hard-coded list of valid commands
GIT_COMMAND = ['git', 'cat-file', '-e'].freeze
 
def get_dev_manifest_sha
  http = Net::HTTP.new(MANIFEST_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new(MANIFEST_URL)
  response = http.request(req)
  latest_release = JSON.parse(response.body)
  latest_release["git_sha"]
end
 
# Function to validate if the input is a proper SHA-1 hash
def valid_sha1?(sha)
  sha.match?(/\A[0-9a-f]{40}\z/)
end
 
current_rev = `git rev-parse HEAD`.chomp!
master_rev = `git rev-parse origin/master`.chomp!
dev_rev = if ENV['GIT_DIFF_BASE']
            ENV['GIT_DIFF_BASE']
          else
            get_dev_manifest_sha
          end
 
# Validate dev_rev before using it
if valid_sha1?(dev_rev)
  STDERR.puts("DEBUG: Executing hard-coded command with dev_rev: #{dev_rev}")
 
  # Use the hard-coded git command and append dev_rev if it's valid
  command = GIT_COMMAND + [dev_rev]
  
  # Execute the command and capture output
  Open3.popen3(*command) do |stdin, stdout, stderr, wait_thr|
    dev_rev_invalid = !wait_thr.value.success?
    stdout_str = stdout.read
    stderr_str = stderr.read
    
    if dev_rev_invalid
      STDERR.puts("DEBUG: git cat-file failed for dev_rev: #{dev_rev}")
      STDERR.puts("ERROR: #{stderr_str.strip}")
    else
      STDERR.puts("DEBUG: git cat-file succeeded for dev_rev: #{dev_rev}")
    end
  end
else
  STDERR.puts("WARNING: Invalid dev_rev SHA-1: #{dev_rev}")
  dev_rev_invalid = true
end
 
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