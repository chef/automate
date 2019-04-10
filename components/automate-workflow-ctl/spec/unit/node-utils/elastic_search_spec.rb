require "spec_helper"
require "node-utils/elastic_search"

describe NodeUtils::ElasticSearch do
  def build_node_json(status, checkin, node_ping = nil)
    json = base_node_json
    json["status"]    = status
    json["checkin"]   = checkin.to_s
    json["node_ping"] = node_ping.to_s if node_ping
    json
  end

  describe "update_status_for_billing!" do
    let(:current_datestring) { "2017-04-26T23:42:36.000Z" }
    let(:current_datetime)   { DateTime.parse(current_datestring) }
    let(:current_time)       { current_datetime.to_time() }
    let(:five_mins_ago)      { (current_datetime - 5 * 60).to_time() }
    let(:ten_mins_ago)       { (current_datetime - 10 * 60).to_time() }
    let(:fifteen_mins_ago)   { (current_datetime - 15 * 60).to_time() }

    let(:base_node_json) do
      {
        "name" => "build_node",
        "fqdn" => "build_node.delivery.co",
        "chef_version" => "13.0.0",
        "platform" => "ubuntu",
        "platform_verison" => "16.04",
        "platform_family" => "debian",
        "organization_name" => "cheffy"
      }
    end

    context "when status is 'success'" do
      let(:node_json) do
        build_node_json("success", ten_mins_ago, ten_mins_ago)
      end

      it "doesn't update the success status" do
        NodeUtils::ElasticSearch.update_status_for_billing!(node_json, fifteen_mins_ago)
        expect(node_json["status"]).to eq("success")
      end
    end

    context "when status is 'failure'" do
      let(:node_json) do
        build_node_json("failure", ten_mins_ago, ten_mins_ago)
      end

      it "doesn't update the success status" do
        NodeUtils::ElasticSearch.update_status_for_billing!(node_json, fifteen_mins_ago)
        expect(node_json["status"]).to eq("failure")
      end
    end

    context "when status is 'missing'" do
      context "and no ping has occured" do
        let(:threshold) { ten_mins_ago }
        let(:node_json) { build_node_json("missing", fifteen_mins_ago, nil) }

        it "doesn't change the status" do
          NodeUtils::ElasticSearch.update_status_for_billing!(node_json, ten_mins_ago)
          expect(node_json["status"]).to eq("missing")
        end
      end

      context "and ping has occured within threshold" do
        let(:threshold) { ten_mins_ago }
        let(:node_json) { build_node_json("missing", fifteen_mins_ago, five_mins_ago) }

        it "changes the status to live" do
          NodeUtils::ElasticSearch.update_status_for_billing!(node_json, ten_mins_ago)
          expect(node_json["status"]).to eq("live")
        end
      end

      context "and ping is outside the threshold" do
        let(:threshold) { ten_mins_ago }
        let(:node_json) { build_node_json("missing", fifteen_mins_ago, fifteen_mins_ago) }

        it "doesn't change the status" do
          NodeUtils::ElasticSearch.update_status_for_billing!(node_json, ten_mins_ago)
          expect(node_json["status"]).to eq("missing")
        end
      end
    end
  end

  describe "agentless_nodes" do
    let(:current_datestring) { "2017-04-26T23:42:36.000Z" }
    let(:current_datetime)   { DateTime.parse(current_datestring) }
    let(:current_time)       { current_datetime.to_time() }
    let(:five_mins_ago)      { (current_time - 5 * 60) }
    let(:sixy_one_mins_ago)  { (current_time - 61 * 60) }

    let(:agentless_query) do
      {
        query: {
          bool: {
            must: { exists: { field: "job_uuid"} }
          }
        },
        _source: ["node_name", "node_uuid", "end_time", "status", "@timestamp"]
      }
    end

    let(:es_hit_success) do
      {
        "_id" => "6e3542d0-7322-4bc3-bf44-9173e23fbde1",
        "_source" => {
          "node_name" => "node1",
          "end_time" => five_mins_ago.iso8601,
          "status" => "passed",
          "fqdn" => "node1.example.com",
          "extra_field" => "some_value",
        }
      }
    end

    let(:expected_node_data_success) do
      {
        "uuid" => "6e3542d0-7322-4bc3-bf44-9173e23fbde1",
        "node_name" => "node1",
        "name" => "node1",
        "end_time" => five_mins_ago.iso8601,
        "checkin" => five_mins_ago.iso8601,
        "status" => "scan-passed",
        "fqdn" => "node1.example.com",
        "extra_field" => "some_value",
      }
    end

    let(:es_hit_failed) do
      {
        "_id" => "59d92b29-2cb6-477c-8402-0c4fa67938a8",
        "_source" => {
          "node_name" => "node2",
          "end_time" => five_mins_ago.iso8601,
          "status" => "failed",
          "fqdn" => "node2.example.com",
          "extra_field" => "some_value2",
        }
      }
    end

    let(:expected_node_data_failed) do
      {
        "uuid" => "59d92b29-2cb6-477c-8402-0c4fa67938a8",
        "node_name" => "node2",
        "name" => "node2",
        "end_time" => five_mins_ago.iso8601,
        "checkin" => five_mins_ago.iso8601,
        "status" => "scan-failed",
        "fqdn" => "node2.example.com",
        "extra_field" => "some_value2",
      }
    end

    # A node that was scanned successfully, but falls outside the billing threshold
    let(:es_hit_unreachable) do
      {
        "_id" => "3ada0ce7-0df2-484d-9e15-0cbed41ab252",
        "_source" => {
          "node_name" => "node3",
          "end_time" => sixy_one_mins_ago.iso8601,
          "status" => "passed",
          "fqdn" => "node3.example.com",
          "extra_field" => "some_value3",
        }
      }
    end

    let(:expected_node_data_unreachable) do
      {
        "uuid" => "3ada0ce7-0df2-484d-9e15-0cbed41ab252",
        "node_name" => "node3",
        "name" => "node3",
        "end_time" => sixy_one_mins_ago.iso8601,
        "checkin" => sixy_one_mins_ago.iso8601,
        "status" => "scan-unreachable",
        "fqdn" => "node3.example.com",
        "extra_field" => "some_value3",
      }
    end

    before do
      allow(Time).to receive(:now).and_return(current_datetime.to_time)
    end

    it "fetches nodes from ES and reformats them" do
      expect(described_class).to receive(:search_with_scroll).
        with("compliance-latest", agentless_query, {}, "inspec_summary").
        and_return([es_hit_success, es_hit_failed])
      result = described_class.agentless_nodes
      expect(result.size).to eq(2)
      expect(result[0]).to eq(expected_node_data_success)
      expect(result[1]).to eq(expected_node_data_failed)
    end

    it "marks nodes with scans prior to the billing threshold as 'scan-unreachable'" do
      expect(described_class).to receive(:search_with_scroll).
        with("compliance-latest", agentless_query, {}, "inspec_summary").
        and_return([es_hit_success, es_hit_failed, es_hit_unreachable])
      result = described_class.agentless_nodes
      expect(result.size).to eq(3)
      expect(result[0]).to eq(expected_node_data_success)
      expect(result[1]).to eq(expected_node_data_failed)
      expect(result[2]).to eq(expected_node_data_unreachable)
    end
  end
end
