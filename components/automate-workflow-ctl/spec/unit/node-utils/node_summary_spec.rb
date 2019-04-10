require "spec_helper"
require "node-utils/node_summary"

describe NodeUtils::NodeSummary do

  let(:agentless_node_1) do
    {
      "name"    => "policyfile-fixture-2-delivered.cd.chef.co",
      "checkin" => "2017-11-15T21:32:09+00:00",
      "uuid"    => "0cf07cf2-fd2e-4659-9c6a-e381be0ab118",
      "status"  => "scanned",
    }
  end

  let(:agentless_node_2) do
    {
      "name"    => "a2-acceptance.cd.chef.co",
      "checkin" => "2017-11-03T21:21:32+00:00",
      "uuid"    => "1381f53d-ba7c-43e4-877e-95d415948dbd",
      "status"  => "scanned",
    }
  end

  let(:agentless_nodes) do
    [ agentless_node_1, agentless_node_2 ]
  end

  let(:viz_node_1) do
    {
      "uuid" => "a8fb7d66-8bfb-43f4-b308-ff5ed1f0e7d1",
      "chef_version" => "12.19.36",
      "checkin" => "2017-06-07T19:53:37.000Z",
      "@timestamp" => "2017-06-07T19:53:37.000Z",
      "platform_version" => "14.04",
      "ec2" => { "public_ipv4" => nil, "instance_id" => "i-03ca29ac5ba118446", "instance_type" => "t2.medium", "placement_availability_zone" => "us-west-2a" },
      "fqdn" => "ip-172-21-116-135.us-west-2.compute.internal",
      "name" => "node1",
      "organization_name" => "chef",
      "platform_family" => "debian",
      "node_ping" => "2017-06-07T19:53:37.000Z",
      "platform" => "ubuntu",
      "status" => "missing",
    }
  end

  let(:viz_node_2) do
    {
      "uuid" => "a11648ac-8a78-4b9c-9e35-f44778f7322e",
      "chef_version" => "12.19.36",
      "checkin" => "2017-06-07T20:12:48.000Z",
      "@timestamp" => "2017-06-07T20:12:48.000Z",
      "platform_version" => "14.04",
      "ec2" => { "public_ipv4" => nil, "instance_id" => "i-00566914d16d8ee68", "instance_type" => "t2.medium", "placement_availability_zone" => "us-west-2a" },
      "fqdn" => "ip-172-21-116-252.us-west-2.compute.internal",
      "name" => "node2",
      "organization_name" => "chef",
      "platform_family" => "debian",
      "node_ping" => "2017-06-07T20:12:48.000Z",
      "platform" => "ubuntu",
      "status" => "missing",
    }
  end

  let(:es_nodes) do
    [ viz_node_1, viz_node_2 ]
  end

  before do
    allow(NodeUtils::ShellOut)
      .to receive(:shell_out)
      .and_return(true)
    allow(NodeUtils::ElasticSearch).to receive(:nodes).and_return(es_nodes)
    allow(NodeUtils::ElasticSearch).to receive(:agentless_nodes).and_return(agentless_nodes)
  end

  describe "#summary" do

    before do
      ENV["AUTOMATE_AGENTLESS_NODES"] = "1"
    end

    after do
      ENV.delete("AUTOMATE_AGENTLESS_NODES")
    end

    context "with nodes" do

      let(:summary) { described_class.summary }

      it "returns text formatted nodes" do
        expect(summary).to match(/node1\s+a8fb7d66-8bfb-43f4-b308-ff5ed1f0e7d1\s+missing\s+2017-06-07T19:53:37\.000Z/)
        expect(summary).to match(/node2\s+a11648ac-8a78-4b9c-9e35-f44778f7322e\s+missing\s+2017-06-07T20:12:48\.000Z/)
      end

      it "includes agentless scan nodes" do
        expect(summary).to match(/policyfile-fixture-2-delivered.cd.chef.co\s+0cf07cf2-fd2e-4659-9c6a-e381be0ab118\s+scanned\s+2017-11-15T21:32:09\+00:00/)
        expect(summary).to match(/a2-acceptance.cd.chef.co\s+1381f53d-ba7c-43e4-877e-95d415948dbd\s+scanned\s+2017-11-03T21:21:32\+00:00/)
      end

      context "with json output" do

        let(:combined_nodes) do
          [ viz_node_1, viz_node_2, agentless_node_1, agentless_node_2 ]
        end

        it "returns nodes as json" do
          expect(JSON.parse(described_class.summary(format: "json"))).to eq(combined_nodes)
        end
      end

      context "when some nodes in viz and agentless scan appear to be duplicates" do

        let(:agentless_node_2) do
          {
            "name"    => "ip-172-21-116-252.us-west-2.compute.internal",
            "checkin" => "2017-11-03T21:21:32+00:00",
            "uuid"    => "1381f53d-ba7c-43e4-877e-95d415948dbd",
            "status"  => "scanned",
          }
        end

        let(:combined_nodes) do
          [ viz_node_1, viz_node_2, agentless_node_1 ]
        end

        it "de-dups on node_name" do
          expect(JSON.parse(described_class.summary(format: "json"))).to eq(combined_nodes)
        end

      end
    end

    context "when there are no nodes" do

      let(:agentless_nodes) { [] }
      let(:es_nodes) { [] }

      it "notifies that there aren't nodes" do
        expect(described_class.summary).to match(/does not have any registered nodes/)
      end

      context "with json output" do
        it "returns an empty array as json" do
          expect(JSON.parse(described_class.summary(format: "json"))).to eq(es_nodes)
        end
      end
    end
  end
end
