require "spec_helper"
require "ctl-helpers/disk_util"

describe CtlHelpers::DiskUtil do
  before do
    allow(CtlHelpers::Ohai).to receive(:ohai).and_return(ohai)
  end

  let(:ohai) do
    {
      filesystem2: {
        by_mountpoint: {
          "/" => { "fs_type" => "aufs", "mount_options" => %w{rw dio dirperm1}, "devices" => ["/dev/sdb"] },
          "/dev" => { "fs_type" => "tmpfs", "mount_options" => ["rw", "nosuid", "mode=755"], "devices" => ["tmpfs"] },
          "/mnt/files" => { "fs_type" => "ext3", "mount_options" => %w{ro noatime}, "devices" => ["/dev/sda1"] },
        },
      },
      block_device: {
        "sdb" => { "rotational" => "1" },
        "tmpfs" => { "rotational" => "0" },
        "sda1" => { "rotational" => "0" },
      },
    }
  end

  describe "#self.mount_for_path" do
    context "when the path is directly mounted" do
      it "returns the path" do
        expect(described_class.mount_for_path("/mnt/files")).to eq("/mnt/files")
      end
    end

    context "when the path is not directly mounted" do
      it "returns the root path" do
        expect(described_class.mount_for_path("/var/opt/delivery")).to eq("/")
      end
    end
  end

  describe "#self.info_for_mount" do
    it "returns the device info for the mount" do
      described_class.info_for_mount("/mnt/files") do |info|
        expect(info["fs_type"]).to eq("ext3")
        expect(info["mount_options"]).to eq(%w{ro noatime})
        expect(info["devices"]).to eq(["/dev/sda1"])
      end
    end
  end

  describe "#self.block_info_for_device" do
    it "returns the block info for the device" do
      described_class.block_info_for_device("/dev/sdb") do |info|
        expect(info["rotational"]).to eq("1")
      end

      described_class.block_info_for_device("tmpfs") do |info|
        expect(info["rotational"]).to eq("0")
      end
    end
  end
end
