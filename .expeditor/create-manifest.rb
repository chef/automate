#!/usr/bin/env ruby

# By default, this script creates a manifest.json file that contains all the packages in the dev channel

require 'date'
require 'net/http'
require 'json'
require 'openssl'
require 'open3'
require 'logger'

# Packages that are present in products.meta but we wish to
# exclude from the manifest (probably because they are not yet published to the
# depot).
#
# We make this list explicit so that we can make manifest generation fail when
# we fail to get expected package data from the hab depot.
SKIP_PACKAGES = []

class PackageQuerier
  class Ident
    attr_accessor :origin, :name, :version, :release, :source
    def initialize(data={})
      @origin= data["origin"]
      @name = data["name"]
      @version = data["version"]
      @release = data["release"]
      @source = data["source"] || "unknown source"
    end

    def ident
      "#{origin}/#{name}/#{version}/#{release}"
    end

    def pretty
      "#{ident} from #{source}"
    end

    def to_s
      ident
    end

    def ==(other)
      other != nil && ident == other.ident
    end
  end

  class ChainQuerier
    def initialize(queriers=[])
      @queriers=queriers
    end

    def get_latest(channel, origin, name)
      @queriers.each do |q|
        ret = q.get_latest(channel, origin, name)
        if ret != nil
          return ret
        end
      end
    end
  end

  class LoggedComparisonQuerier
    def initialize(main_querier, comparison_querier, logger)
      @main=main_querier
      @compare=comparison_querier
      @logger=logger
    end

    def get_latest(channel, origin, name)
      ret = @main.get_latest(channel, origin, name)
      if ret != nil
        compare_result = @compare.get_latest(channel, origin, name)
        if compare_result != ret
          @logger.warn("Found mismatch between sources:")
          @logger.warn("   #{ret.pretty}")
          @logger.warn("   #{compare_result.pretty}")
          @logger.warn("Using #{ret}")
        end
      end
      ret
    end
  end

  class PinQuerier
    def initialize(pins={})
      @pins = pins
    end

    def get_latest(_channel, _origin, name)
      ident = @pins[name]
      if ident != nil
        ident["source"] = "pin"
        Ident.new(ident)
      else
        nil
      end
    end
  end

  class DiskQuerier
    def initialize(dir, origin)
      @dir=dir
      @fallback_origin=origin
    end

    def get_latest(_channel, origin, name)
      ret = get_latest_from_disk(origin, origin, name)
      if ret == nil
        get_latest_from_disk(@fallback_origin, origin, name)
      else
        ret
      end
    end

    def get_latest_from_disk(lookup_origin, desired_origin, name)
      # We are just going to depend on glob ordering here rather than
      # explicitly sorting
      candidates = Dir.glob("#{@dir}/#{lookup_origin}-#{name}-*.hart")
      if candidates.length > 0
        ident_from_package(desired_origin, name, lookup_origin, candidates.first)
      else
        nil
      end
    end

    def ident_from_package(desired_origin, desired_name, found_origin, package_path)
      ident = {}
      ident["source"] = "local file #{package_path}"
      ident["origin"] = desired_origin
      ident["name"] = desired_name

      hart_name = File.basename(package_path)
      if match_data = /^#{found_origin}-#{desired_name}-(.*)-(\d{14})-x86_64-linux.hart$/.match(hart_name)
        ident["version"] = match_data[1]
        ident["release"] = match_data[2]
        Ident.new(ident)
      else
        raise "Could not parse ident from filename #{hart_name}"
      end
    end
  end

  class ExpeditorEnvQuerier
    def initialize
      @idents=collect_package_idents_from_env
    end

    def get_latest(_channel, origin, name)
      @idents["#{origin}/#{name}"]
    end

    def collect_package_idents_from_env
      idents = {}
      ENV.keys.each do |key|
        if key.start_with?("EXPEDITOR_PKG_IDENTS_")
          ident=ENV[key]
          origin, name, version, release = ident.split("/")
          idents["#{origin}/#{name}"] = Ident.new(
            {
              "origin" => origin,
              "name" => name,
              "version" => version,
              "release" => release,
              "source" => "expeditor environment"
            })
        end
      end
      idents
    end
  end

  class DepotQuerier
    BLDR_API_HOST="bldr.habitat.sh"
    BLDR_API_USER_AGENT="Chef Expeditor"

    def initialize(host=BLDR_API_HOST)
      @host=host
    end

    def get_latest(channel, origin, name)
      http = Net::HTTP.new(@host, 443)
      http.use_ssl = true
      http.verify_mode = OpenSSL::SSL::VERIFY_PEER
      req = Net::HTTP::Get.new("/v1/depot/channels/#{origin}/#{channel}/pkgs/#{name}/latest", {'User-Agent' => BLDR_API_USER_AGENT})
      response = http.request(req)
      case response
      when Net::HTTPNotFound
        return nil
      else
        latest_release = JSON.parse(response.body)
        ident = latest_release["ident"]
        ident["source"] = "Habitat Depot"
        Ident.new(ident)
      end
    end
  end
end

def get_hab_deps_latest(package_querier)
  ret = {}
  ["hab", "hab-sup", "hab-launcher"].each do |name|
    d = package_querier.get_latest("stable", "core", name)
    ret[name] = d.ident
  end
  ret
end

def channel_for_origin(origin)
  case origin
  when "chef"
    "dev"
  else
    "stable"
  end
end

def log_added_ident(log, ident)
  log.info "Adding package #{ident.pretty}"
end

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
  "automate-postgresql"    => {"origin" => "chef", "name" => "automate-postgresql",    "version" => "9.6.11", "release" => "20190409151101"},
  "automate-elasticsearch" => {"origin" => "chef", "name" => "automate-elasticsearch", "version" => "6.2.2",  "release" => "20190123133819"}
}

# IF YOU UPDATE THESE PINS YOU MUST ALSO UPDATE THE core/hab PIN IN
# components/automate-deployment/habitat/plan.sh
#
pinned_hab_components = {
    "hab"          => { "origin" => "core", "name" => "hab",          "version" => "0.69.0", "release" => "20181127182011"},
    "hab-sup"      => { "origin" => "core", "name" => "hab-sup",      "version" => "0.69.0", "release" => "20181127183841"},
    "hab-launcher" => { "origin" => "core", "name" => "hab-launcher", "version" => "9106",   "release" => "20181126205526"}
}

pins = pinned_databases.merge(pinned_hab_components)

# When true, ALLOW_LOCAL_PACKAGES creates the manifest using locally
# created hartifacts if it can not otherwise find the package uploaded
# to the depot.
use_local_packages=(ENV["ALLOW_LOCAL_PACKAGES"] == "true")
local_package_origin = ENV["HAB_ORIGIN"] || "chef"
local_package_directory = "results"

use_environment_idents=(ENV["EXPEDITOR_NAME"].to_s != "")
version = ENV["VERSION"] || DateTime.now.strftime("%Y%m%d%H%M%S")
filename = if ENV["VERSION"]
             "#{ENV["VERSION"]}.json"
           else
             "manifest.json"
           end

log = Logger.new(STDOUT)
log.formatter = proc do |severity, datetime, progname, msg|
  "#{datetime} #{severity}: #{msg}\n"
end
log.info "Creating release manifest for Automate"
log.info "--------------------------------------"
log.info "Configuration:"
log.info "  use_local_packages=#{use_local_packages}"
log.info "  use_environment_idents=#{use_environment_idents}"
log.info "  local_package_origin=#{local_package_origin}"
log.info "  local_package_directory=#{local_package_directory}"
log.info "  filename=#{filename}"
log.info "  version=#{version}"
log.info "-------------------------------"

package_queriers = [PackageQuerier::PinQuerier.new(pins)]
if use_environment_idents
  package_queriers << PackageQuerier::LoggedComparisonQuerier.new(PackageQuerier::ExpeditorEnvQuerier.new, PackageQuerier::DepotQuerier.new, log)
end
package_queriers << PackageQuerier::DepotQuerier.new
# NOTE(ssd) 2019-08-30: This comes after DepotQuerier, becuase we only
# really need to look at local packages in the case of adding a new
# service that hasn't been uploaded to the depot. For other newly
# built packages, we depend on the deployment-service's override
# origin.
if use_local_packages
  package_queriers << PackageQuerier::DiskQuerier.new(local_package_directory, local_package_origin)
end
package_querier = PackageQuerier::ChainQuerier.new(package_queriers)

manifest = {}

# The version of the manifest schema - might need to be bumped in the future
manifest["schema_version"] = "1"

# The version of the manifest - the "engineering" version
manifest["build"] = version

# Grab the version of various Habitat components from the deployment-service
hab_deps = get_hab_deps_latest(package_querier)
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

  latest_release = package_querier.get_latest(channel_for_origin(pkg_origin), pkg_origin, pkg_name)
  log_added_ident(log, latest_release)
  manifest["packages"] << latest_release.ident
end

products_meta["deleted_packages"].each do |pkg|
  log.info "Adding last stable release of deleted package #{pkg}"
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

  latest_release = package_querier.get_latest(channel_for_origin(pkg_origin), pkg_origin, pkg_name)
  log_added_ident(log, latest_release)
  manifest["packages"] << latest_release.ident
end

manifest["packages"].uniq!
# Sort the packages for easier diff-ing
manifest["packages"].sort!


File.open("#{filename}", "w") { |file| file.write(JSON.pretty_generate(manifest)) }
