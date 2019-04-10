require "ctl-helpers/ohai"
require "pathname"

module CtlHelpers
  module DiskUtil
    class << self
      def mount_for_path(path)
        mounts = CtlHelpers::Ohai.ohai[:filesystem2][:by_mountpoint].keys

        # Ascend all known mount points until we find a match or end up at the
        # root.
        Pathname.new(path).ascend do |path_to_check|
          return path_to_check.to_s if mounts.include?(path_to_check.to_s)
        end
      end

      def info_for_mount(mount)
        CtlHelpers::Ohai.ohai[:filesystem2][:by_mountpoint][mount]
      end

      def block_info_for_device(device)
        if CtlHelpers::Ohai.ohai[:block_device]
          CtlHelpers::Ohai.ohai[:block_device][File.basename(device)]
        else
          {}
        end
      end
    end
  end
end
