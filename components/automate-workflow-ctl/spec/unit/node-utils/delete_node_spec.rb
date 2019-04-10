require "spec_helper.rb"
require "node-utils/delete_node"

describe NodeUtils::DeleteNode do
  let(:highline) { HighLine.new }
  let(:es_client) { double("Elasticsearch::Client") }
  let(:es_insights_index) do
    [
      { "_index" => "insights-2017.06.28",
        "_type" => "converge",
        "_id" => "AVzwoRmfHTYD3v0Bt39y",
        "_version" => 1,
        "found" => true,
        "_source" =>
        { "entity_uuid" => node_uuid,
          "node_name" => node_name,
          "organization_name" => "Foo",
          "run_id" => "364fcaba-b4d5-4b09-835d-9da266bdcca8",
          "source" => "chef_solo",
          "start_time" => "2017-06-28T21:35:07Z",
          "@version" => "1",
          "@timestamp" => "2017-06-28T21:35:07.000Z",
          "tags" => ["chef"],
          "event_action" => "started",
          "source_fqdn" => chef_server_fqdn,
          "event_type" => "converge",
          "type" => "converge",
          "@uuid" => "9682fa01-d773-46d4-a7fa-f4ab3d75f76f" },
      },
      { "_index" => "insights-2017.06.28",
        "_type" => "converge",
        "_id" => "AVzwzFKoHTYD3v0Bt3-H",
        "_version" => 1,
        "found" => true,
        "_source" =>
        { "entity_uuid" => node_uuid,
          "node_name" => node_name,
          "organization_name" => organization_name,
          "run_id" => "a9ac5a29-5d3b-4ebe-9615-0369f43fecfa",
          "source" => "chef_solo",
          "start_time" => "2017-06-28T22:22:20Z",
          "@version" => "1",
          "@timestamp" => "2017-06-28T22:22:20.000Z",
          "tags" => ["chef"],
          "event_action" => "started",
          "source_fqdn" => chef_server_fqdn,
          "event_type" => "converge",
          "type" => "converge",
          "@uuid" => "d4952449-c1b7-463a-9fbf-940862d7a019" } },
    ]
  end
  let(:delete_insights_bulk_requests) do
    [
      { :delete => { "_index" => "insights-2017.06.28", "_id" => "AVzwoRmfHTYD3v0Bt39y", "_type" => "converge" } },
      { :delete => { "_index" => "insights-2017.06.28", "_id" => "AVzwzFKoHTYD3v0Bt3-H", "_type" => "converge" } },
    ]
  end
  let(:es_compliance_index) do
    [
      { "_index" => "compliance-2017.05.17",
        "_type" => "inspec_summary",
        "_id" => "5c3a2bd6-e00b-4a03-aebe-85cbafd84f8f",
        "_version" => 6,
        "found" => true,
        "_source" =>
        { "platform" => { "name" => "mac_os_x", "release" => "10.11.6" },
          "type" => "inspec_summary",
          "node_name" => node_name,
          "end_time" => "2017-05-17T17:51:19+01:00",
          "node_uuid" => node_uuid,
          "report_uuid" => "5c3a2bd6-e00b-4a03-aebe-85cbafd84f8f",
          "@timestamp" => "2017-05-17T16:51:19.000Z",
        },
      },
      { "_index" => "compliance-2017.05.17",
        "_type" => "inspec_summary",
        "_id" => "ac320a87-eb25-478d-922f-2a5ff4742bcb",
        "_version" => 6,
        "found" => true,
        "_source" =>
          { "platform" => { "name" => "mac_os_x", "release" => "10.11.6" },
            "type" => "inspec_summary",
            "node_name" => node_name,
            "end_time" => "2017-05-17T17:51:19+01:00",
            "node_uuid" => node_uuid,
            "report_uuid" => "5c3a2bd6-e00b-4a03-aebe-85cbafd84f8f",
            "@timestamp" => "2017-05-17T16:51:19.000Z",
          },
        },
     ]
  end
  let(:delete_compliance_bulk_requests) do
    [
      { :delete => { "_index" => "compliance-2017.05.17", "_id" => "5c3a2bd6-e00b-4a03-aebe-85cbafd84f8f", "_type" => "inspec_summary" } },
      { :delete => { "_index" => "compliance-2017.05.17", "_id" => "ac320a87-eb25-478d-922f-2a5ff4742bcb", "_type" => "inspec_summary" } },
    ]
  end
  let(:es_node_state) do
    { "_index" => "node-state-1",
      "_type" => "node-state",
      "_id" => node_uuid,
      "_version" => 5,
      "found" => true,
      "_source" => {
        "entity_uuid" => node_uuid,
        "exists" => "true",
        "name" => node_name,
        "source_fqdn" => chef_server_fqdn,
        "organization_name" => organization_name,
        "@timestamp" => "2017-06-28T22:22:20.000Z",
      },
    }
  end
  let(:shim_node_request) do
    {
      :index => {
        :_index => "node-state-1",
        :_type => "node-state",
        :_id => node_uuid,
        :data => {
          :entity_uuid => node_uuid,
          :exists => "false",
          :name => node_name,
          :organization_name => organization_name,
          :@timestamp => "2017-06-28T22:22:20.000Z",
        },
      },
    }
  end
  let(:purge_node_request) do
    {
      :delete => {
        :_index => "node-state-1",
        :_type => "node-state",
        :_id => node_uuid,
      },
    }
  end
  let(:node_uuid) { "78d4781e-161b-43d0-9812-af97dfb59cb4" }
  let(:node_name) { "chef-Uppercase.solo" }
  let(:organization_name) { "Foo" }
  let(:chef_server_fqdn) { "mycompany.chef-server.net" }
  let(:default_args) do
    {
       uuid: node_uuid,
       name: nil,
       org: nil,
       chef_server_fqdn: nil,
       node_data: true,
       compliance_data: false,
       force: false,
       purge: false,
       request_timeout: 300,
    }
  end
  let(:args) { default_args }

  before do
    allow(HighLine).to receive(:new).and_return(highline)
    allow(Elasticsearch::Client).to receive(:new).and_return(es_client)
    allow(CtlHelpers::Prompt).to receive(:yes_no_prompt).and_return(true)
  end

  subject { described_class.new(args) }

  describe "#delete" do
    before do
      allow(subject).to receive(:determine_node_uuid!).and_return(true)
      allow(subject).to receive(:node).and_return(es_node_state)
      allow(subject).to receive(:delete_insights_bulk_requests).and_return(delete_insights_bulk_requests)
      allow(subject).to receive(:delete_compliance_bulk_requests).and_return(delete_compliance_bulk_requests)
    end

    context "default config" do
      it "deletes node converge data" do
        expect(es_client).to receive(:bulk) do |args|
          delete_insights_bulk_requests.each do |req|
            expect(args[:body]).to include(req)
          end
        end

        subject.delete
      end

      it "does not delete compliance data" do
        expect(es_client).to receive(:bulk) do |args|
          delete_compliance_bulk_requests.each do |req|
            expect(args[:body]).to_not include(req)
          end
        end

        subject.delete
      end

      it "creates a shim node in the node state" do
        expect(es_client).to receive(:bulk) do |args|
          expect(args[:body]).to include(shim_node_request)
        end

        subject.delete
      end

      it "does not purge the node in the node state" do
        expect(es_client).to receive(:bulk) do |args|
          expect(args[:body]).to_not include(purge_node_request)
        end

        subject.delete
      end

      it "prompts the user for permission to delete" do
        expect(CtlHelpers::Prompt).to receive(:yes_no_prompt)
        expect(es_client).to receive(:bulk)

        subject.delete
      end
    end

    context "deleting compliance data" do
      let(:args) { default_args.merge(compliance_data: true) }

      it "deletes compliance data" do
        expect(es_client).to receive(:bulk) do |args|
          delete_compliance_bulk_requests.each do |req|
            expect(args[:body]).to include(req)
          end
        end

        subject.delete
      end
    end

    context "purging the node" do
      let(:args) { default_args.merge(purge: true) }

      it "does not create a shim node in the node state" do
        expect(es_client).to receive(:bulk) do |args|
          expect(args[:body]).to_not include(shim_node_request)
        end

        subject.delete
      end

      it "purges the node in the node state" do
        expect(es_client).to receive(:bulk) do |args|
          expect(args[:body]).to include(purge_node_request)
        end

        subject.delete
      end
    end

    context "forcing delete" do
      let(:args) { default_args.merge(force: true) }

      it "prompts the user for permission to delete" do
        expect(CtlHelpers::Prompt).to_not receive(:yes_no_prompt)
        expect(es_client).to receive(:bulk)

        subject.delete
      end
    end

    context "when documents exceed the batch size" do
      let(:args) { default_args.merge(batch_size: 1) }

      it "sends multiple bulk requests" do
        expect(es_client).to receive(:bulk).exactly(3).times

        subject.delete
      end
    end
  end

  describe "#determine_node_uuid!" do
    context "with a uuid" do
      context "when the node cannot be found" do
        before do
          allow(es_client)
            .to receive(:get)
            .with(index: "node-state", id: node_uuid)
            .and_raise(Elasticsearch::Transport::Transport::Errors::NotFound)
        end

        it "raises an error" do
          expect { subject.determine_node_uuid! }
            .to raise_error(RuntimeError, /Could not find/)
        end
      end

      context "when one node is found" do
        before do
          allow(es_client)
            .to receive(:get)
            .with(index: "node-state", id: node_uuid)
            .and_return(es_node_state)
        end

        it "configures the node properties" do
          subject.determine_node_uuid!
          expect(subject.node).to eq(es_node_state)
          expect(subject.uuid).to eq(node_uuid)
          expect(subject.name).to eq(node_name)
          expect(subject.org).to eq(organization_name)
          expect(subject.chef_server_fqdn).to eq(chef_server_fqdn)
        end
      end
    end

    context "with a name, org, chef server fqdn" do
      let(:args) do
        default_args.merge(
          uuid: nil,
          name: node_name,
          org: organization_name,
          chef_server_fqdn: chef_server_fqdn
        )
      end

      let(:search_requirements) do
        [
          { term: { "name.raw" => node_name } },
          { term: { "organization_name.raw" => organization_name } },
          { term: { "source_fqdn.raw" => chef_server_fqdn } },
        ]
      end

      before do
        allow(subject).to receive(:scroll_search).and_return(search_results)
        expect(subject).to receive(:scroll_search).with(
          "node-state",
          "node-state",
          search_requirements
        )
      end

      context "when the node cannot be found" do
        let(:search_results) { [] }

        it "raises an error" do
          expect { subject.determine_node_uuid! }
            .to raise_error(RuntimeError, /Could not find/)
        end
      end

      context "when one node is found" do
        let(:search_results) { [es_node_state] }

        it "configures the node properties" do
          subject.determine_node_uuid!
          expect(subject.node).to eq(es_node_state)
          expect(subject.uuid).to eq(node_uuid)
          expect(subject.name).to eq(node_name)
          expect(subject.org).to eq(organization_name)
          expect(subject.chef_server_fqdn).to eq(chef_server_fqdn)
        end
      end

      context "when multiple nodes are found" do
        let(:search_results) { [es_node_state, es_node_state] }

        it "raises an error, outputs uuids, instructs uuid usage" do
          # Tells them to search via UUID
          expect(highline).to receive(:say) do |msg|
            expect(msg).to match(/automate-ctl delete-node --uuid/)
          end

          # Tells them the UUIDs for the matching nodes
          expect(highline).to receive(:say) do |msg|
            expect(msg).to match(/#{node_uuid}/)
          end

          # Raises an error to exit
          expect { subject.determine_node_uuid! }
            .to raise_error(RuntimeError, /Too many nodes found/)
        end
      end
    end
  end

  describe "#scroll_search" do
    let(:search_reqs) { { term: { "name.raw" => node_name } } }
    let(:search_query_body) do
      { query: { filtered: { filter: { bool: { must: search_reqs } } } } }
    end
    let(:search_results) do
      { "_scroll_id" => 1234,
        "hits" => { "hits" => [es_insights_index[0]] },
      }
    end

    before do
      allow(es_client)
        .to receive(:search)
        .and_return(search_results)

      allow(es_client)
        .to receive(:scroll)
        .and_return(
          { "hits" => { "hits" => [es_insights_index[1]] } },
          { "hits" => { "hits" => [] } }
        )
    end

    it "scrolls through the API and returns hits as an array" do
      expect(subject.scroll_search("insights-*", "converge", search_reqs))
        .to eq(es_insights_index)
    end
  end

  describe "#bulk_delete_requests" do
    let(:search_reqs) { { term: { "name.raw" => node_name } } }
    let(:search_query_body) do
      { query: { filtered: { filter: { bool: { must: search_reqs } } } } }
    end

    before do
      allow(subject)
        .to receive(:scroll_search)
        .with("insights-*", "converge,node_ping", { term: { "name.raw" => node_name } } , false)
        .and_return(es_insights_index)
    end

    it "returns all search results as bulk delete requests" do
      expect(subject.bulk_delete_requests("insights-*", "converge,node_ping", search_reqs))
        .to eq(delete_insights_bulk_requests)
    end
  end

  describe "#delete_insights_bulk_requests" do
    before do
      allow(subject)
        .to receive(:scroll_search)
        .with("insights-*", "converge,node_ping", { term: { "entity_uuid" => node_uuid } }, false)
        .and_return(es_insights_index)
    end

    it "maps hits to delete requests" do
      expect(subject.delete_insights_bulk_requests).to eq(delete_insights_bulk_requests)
    end
  end

  describe "#delete_compliance_bulk_requests" do
    before do
      allow(subject)
        .to receive(:scroll_search)
        .with("compliance-20*", "inspec_report,inspec_summary", { term: { "node_uuid" => node_uuid } }, false)
        .and_return(es_compliance_index)
    end

    it "maps hits to delete requests" do
      expect(subject.delete_compliance_bulk_requests).to eq(delete_compliance_bulk_requests)
    end
  end
end
