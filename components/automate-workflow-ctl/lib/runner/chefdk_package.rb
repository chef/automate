require 'open-uri'
require 'mixlib/install'

require 'runner/exceptions'

module Runner
  # This class populates info needed to download a chefdk package and downloads it.
  class ChefDKPackage
    ENTERPRISE_LINUX = ['centos', 'redhat', 'oracle'].freeze

    def initialize(version, os_name, os_release)
      @version = version || :latest
      @os_name = os_name
      @os_release = os_release
    end

    def download
      options = populate_artifact_options(@version)

      artifact = fetch_artifact_package_info(options)

      filepath = "/tmp/#{artifact.url.split("/")[-1]}"

      File.open(filepath, 'w+') do |file|
        begin
          file << open(artifact.url).read
        rescue OpenURI::HTTPError => error
          raise Runner::Exceptions::ChefDKHTTPError
        end
      end

      filepath
    end

    def populate_artifact_options(version_to_download)
      os_info = find_os_info
      options = {
        channel: :stable,
        product_name: 'chefdk',
        product_version: version_to_download,
        platform: os_info[:name],
        platform_version: os_info[:release],
        architecture: 'x86_64'
      }
    end

    def fetch_artifact_package_info(options)
      artifact_info = Mixlib::Install.new(options).artifact_info

      # mixlib returns an empty array in the 404 case
      unless artifact_info.is_a?(Mixlib::Install::ArtifactInfo)
        # if the user requested a version that does not exist,
        # return a relevant error.
        if @version == :latest
          raise Runner::Exceptions::ChefDKHTTPError
        else
          raise Runner::Exceptions::ChefDK404
        end
      end
      artifact_info
    end

    def find_os_info
      case @os_name
      when 'ubuntu'
        {name: 'ubuntu', release: @os_release.strip}
      when 'debian'
        {name: 'debian', release: @os_release.split('.')[0]}
      when 'suse' # train returns suse for sles
        {name: 'sles', release: @os_release}
      when *ENTERPRISE_LINUX
        {name: 'el', release: @os_release.split('.')[0]}
      when 'mac_os_x'
        {name: 'mac_os_x', release: @os_release.rpartition('.')[0]}
      else
        raise Runner::Exceptions::UnsupportedTargetOS.new(@os_name)
      end
    end

  end
end
