#!/usr/bin/env ruby

# By default, this script creates a manifest.json file that contains all the packages in the dev channel

require 'date'
require 'net/http'
require 'json'
require 'openssl'
require 'open3'

module Net
  class HTTP::Purge < HTTPRequest
    METHOD='PURGE'
    REQUEST_HAS_BODY = false
    RESPONSE_HAS_BODY = true
  end
end

BLDR_API_HOST="bldr.habitat.sh"
BLDR_API_USER_AGENT="Chef Expeditor"

# Packages that are present in products.meta but we wish to
# exclude from the manifest (probably because they are not yet published to the
# depot).
#
# We make this list explicit so that we can make manifest generation fail when
# we fail to get expected package data from the hab depot.
SKIP_PACKAGES = []


# When true, ALLOW_LOCAL_PACKAGES creates the manifest using locally
# created hartifacts if it can not otherwise find the package uploaded
# to the depot.
ALLOW_LOCAL_PACKAGES=(ENV["ALLOW_LOCAL_PACKAGES"] == "true")
LOCAL_PACKAGE_PATH="results/"
LOCAL_PACKAGE_ORIGIN=ENV["HAB_ORIGIN"] || "chef"
FASTLY_PURGE=(ENV["EXPEDITOR_GROUP_ID"].to_s != "")

def channel_for_origin(origin)
  case origin
  when "chef"
    "dev"
  else
    "stable"
  end
end

def get_latest(channel, origin, name)
  # -- FIXME: PINNED DATABASES - sdelano 2018/07/19 --
  #
  # We currently believe it's unsafe to upgrade the database packages in an existing
  # A2 installation because of unconfigurable shutdown behavior for Habitat services.
  # Until we have a new supervisor that can safely stop PostgreSQL and ElasticSearch,
  # we need to avoid upgrading the databases and risking potential data corruption and
  # stale service state.
  #
  # This will pin the services that we package and start with the deployment service,
  # while still allowing the clients of these databases to upgrade their client
  # libraries if any fixes are shipped there.
  pinned_databases = {
    "automate-postgresql"    => {"version" => "9.6.11", "release" => "20190409151101"},
    "automate-elasticsearch" => {"version" => "6.2.2", "release" => "20190123133819"}
  }

  # IF YOU UPDATE THESE PINS YOU MUST ALSO UPDATE THE core/hab PIN IN components/automate-deployment/habitat/plan.sh
  #
  pinned_hab_components = {
    "hab"          => { "origin" => "core", "name" => "hab",          "version" => "0.69.0", "release" => "20181127182011"},
    "hab-sup"      => { "origin" => "core", "name" => "hab-sup",      "version" => "0.69.0", "release" => "20181127183841"},
    "hab-launcher" => { "origin" => "core", "name" => "hab-launcher", "version" => "9106",   "release" => "20181126205526"}
  }

  if pinned_databases.keys.include?(name)
    return pinned_databases[name]
  end

  if pinned_hab_components.include?(name)
    return pinned_hab_components[name]
  end

  latest_path = "/v1/depot/channels/#{origin}/#{channel}/pkgs/#{name}/latest"

  if FASTLY_PURGE
    purge_fastly_path(BLDR_API_HOST, latest_path)
  end

  http = Net::HTTP.new(BLDR_API_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new(latest_path, {'User-Agent' => BLDR_API_USER_AGENT})
  response = http.request(req)
  case response
  when Net::HTTPNotFound
    if ALLOW_LOCAL_PACKAGES
      puts "  Could not find '#{origin}/#{name}' in Habitat Depot, searching local packages in '#{LOCAL_PACKAGE_PATH}'"
      get_local_package(origin, name)
    else
      raise "Could not find package '#{origin}/#{name}' in channel '#{channel}'.  Has this package been built and uploaded to the Habitat Depot?"
    end
  else
    latest_release = JSON.parse(response.body)
    latest_release["ident"]
  end
end

def purge_fastly_path(host, url)
  puts "  Purging #{url} from Fastly"
  http = Net::HTTP.new(BLDR_API_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Purge.new(url)
  http.request(req)
end

def get_local_package(origin, name)
  # We are just going to depend on glob ordering here rather than explicitly sorting
  candidates_real_origin = Dir.glob("#{LOCAL_PACKAGE_PATH}/#{origin}-#{name}-*.hart")
  if candidates_real_origin.length > 0
    puts "  Using #{candidates_real_origin.first}"
    return ident_from_package(origin, name, origin, candidates_real_origin.first)
  else
    puts "  No candidates for #{origin}/#{name}, trying '#{LOCAL_PACKAGE_ORIGIN}/#{name}'"
  end

  candidates_override_origin= Dir.glob("#{LOCAL_PACKAGE_PATH}/#{LOCAL_PACKAGE_ORIGIN}-#{name}-*.hart")
  if candidates_override_origin.length > 0
    puts "  Using #{candidates_override_origin.first}"
    return ident_from_package(origin, name, LOCAL_PACKAGE_ORIGIN, candidates_override_origin.first)
  end

  raise "Could not find package '#{name} in '#{LOCAL_PACKAGE_PATH}'"
end


def ident_from_package(desired_origin, desired_name, found_origin, package_path)
  ident = {}
  ident["origin"] = desired_origin
  ident["name"] = desired_name

  hart_name = File.basename(package_path)
  if match_data = /^#{found_origin}-#{desired_name}-(.*)-(\d{14})-x86_64-linux.hart$/.match(hart_name)
    ident["version"] = match_data[1]
    ident["release"] = match_data[2]
    ident
  else
    raise "Could not parse ident from filename #{hart_name}"
  end
end

def get_hab_deps_latest()
  ret = {}
  ["hab", "hab-sup", "hab-launcher"].each do |name|
    d = get_latest("stable", "core", name)
    ret[name] = "#{d["origin"]}/#{d["name"]}/#{d["version"]}/#{d["release"]}"
  end
  ret
end

puts "Creating release manifest for Automate"
puts "Important Environment Variables"
puts "-------------------------------"
puts "ALLOW_LOCAL_PACKAGES=#{ENV["ALLOW_LOCAL_PACKAGES"]}"
puts "HAB_ORIGIN=#{ENV["HAB_ORIGIN"]}"
puts "VERSION=#{ENV["VERSION"]}"
puts "EXPEDITOR_PKG_IDENTS=#{ENV["EXPEDITOR_PKG_IDENTS"]}"
puts "-------------------------------"
puts "Environment Variable Keys"
puts "-------------------------------"
puts "#{ENV.keys}"
puts "-------------------------------"

version = ENV["VERSION"] || DateTime.now.strftime("%Y%m%d%H%M%S")
filename = ENV["VERSION"] || "manifest"

manifest = {}

# The version of the manifest schema - might need to be bumped in the future
manifest["schema_version"] = "1"

# The version of the manifest - the "engineering" version
manifest["build"] = version

# Grab the version of various Habitat components from the deployment-service

hab_deps = get_hab_deps_latest
manifest["hab"] = []
manifest["hab"] << hab_deps["hab"]
manifest["hab"] << hab_deps["hab-sup"]
manifest["hab"] << hab_deps["hab-launcher"]


# Grab the version of hab in the build environment. Comes out in the
# form of 'hab 0.54.0/20180221020527'
hab_version = /(\d+\.\d+\.\d+\/\d{14})/.match(`hab --version`.strip)[0]
manifest["hab_build"] = "core/hab/#{hab_version}"

# Grab the git SHA
out, err, status = Open3.capture3("git show-ref HEAD --hash")
raise "Failed to get git_sha: exitcode=#{status.exitstatus} stderr=#{err}" if status.exitstatus != 0
manifest["git_sha"] = out.strip

products_meta = File.open("products.meta") do |f|
  JSON.parse(f.read)
end

manifest["packages"] = []
products_meta["packages"].each do |pkg_path|
  next if SKIP_PACKAGES.include?(pkg_path)

  package_ident = pkg_path.split("/")
  pkg_origin = package_ident[0]
  pkg_name = package_ident[1]

  latest_release = get_latest(channel_for_origin(pkg_origin), pkg_origin, pkg_name)

  pkg_version = latest_release["version"]
  pkg_release = latest_release["release"]

  puts "  Adding package #{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
  manifest["packages"] << "#{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
end

products_meta["deleted_packages"].each do |pkg|
  puts "  Adding last stable release of deleted package #{pkg}"
  manifest["packages"] << "#{pkg}"
end

# Add extra packages to manifest that deployment-service doesn't need to manage
# but we still want versioned with each release.
%w{
  chef/automate-chef-io
}.each do |extra_package|
  package_ident = extra_package.split("/")
  pkg_origin = package_ident[0]
  pkg_name = package_ident[1]

  latest_release = get_latest(channel_for_origin(pkg_origin), pkg_origin, pkg_name)

  pkg_version = latest_release["version"]
  pkg_release = latest_release["release"]

  puts "  Adding package #{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
  manifest["packages"] << "#{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
end

manifest["packages"].uniq!
# Sort the packages for easier diff-ing
manifest["packages"].sort!


File.open("#{filename}.json", "w") { |file| file.write(JSON.pretty_generate(manifest)) }
