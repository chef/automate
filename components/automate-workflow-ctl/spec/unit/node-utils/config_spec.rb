require "spec_helper"
require "node-utils/config"

describe NodeUtils::Config do
  let(:filename) { "/etc/delivery/delivery-running.json" }
  let(:config_file) do
    <<EOF
{
  "delivery": {
    "insights": {
      "node_missing_threshold_mins": 42
    }
  },
  "fake_key": "fake_value"
}
EOF
  end
  let(:config_hash) { JSON.parse(config_file) }
  let(:threshold_mins) { 42 }

  describe "delivery_running" do
    it "raises an error when delivery-running.json doesn't exist" do
      expect(File).to receive(:exist?).with(filename).and_return(false)
      expect { NodeUtils::Config.delivery_running() }.to raise_error(/must configure/)
    end

    it "parses and returns the JSON contents of delivery-running.json" do
      expect(File).to receive(:exist?).with(filename).and_return(true)
      expect(File).to receive(:read).with(filename).and_return(config_file)
      delivery_running = NodeUtils::Config.delivery_running()
      expect(delivery_running).to be_a(Hash)
      expect(delivery_running["fake_key"]).to eq("fake_value")
    end
  end

  describe "missing_threshold_mins" do
    it "should return the value stored in the config" do
      expect(NodeUtils::Config).to receive(:delivery_running).and_return(config_hash)
      expect(NodeUtils::Config.missing_threshold_mins()).to eq(threshold_mins)
    end
  end
end
