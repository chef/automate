require "spec_helper.rb"
require "node-utils/data_summary"

describe NodeUtils::DataSummary do
  let(:highline) { HighLine.new }

  let(:statistics_cluster_data) do
    { "name" => "chef-insights",
      "nodes" =>
     [{ "name" => "1QIktf-qTKmfUA2mSZmNGw",
        "totals" =>
        { "es_cpu_percent" => 0,
          "es_max_file_descriptors" => 50000,
          "es_mem_total_virtual_in_bytes" => 4897030144,
          "es_open_file_descriptors" => 239,
          "fs_free_in_bytes" => 41609461760,
          "fs_total_in_bytes" => 63381999616,
          "jvm_heap_max_in_bytes" => 1064042496,
          "jvm_heap_used_in_bytes" => 201859856,
          "jvm_non_heap_used_in_bytes" => 99344296,
          "os_cpu_percent" => 1,
          "os_mem_total_in_bytes" => 4397072384,
          "os_mem_used_in_bytes" => 3634642944 } }],
      "averages" =>
     { "es_cpu_percent" => 0,
       "es_max_file_descriptors" => 50000,
       "es_mem_total_virtual_in_bytes" => 4897030144,
       "es_open_file_descriptors" => 239,
       "fs_free_in_bytes" => 41609461760,
       "fs_total_in_bytes" => 63381999616,
       "jvm_heap_max_in_bytes" => 1064042496,
       "jvm_heap_used_in_bytes" => 201859856,
       "jvm_non_heap_used_in_bytes" => 99344296,
       "os_cpu_percent" => 1,
       "os_mem_total_in_bytes" => 4397072384,
       "os_mem_used_in_bytes" => 3634642944 } }
  end

  let(:statistics_indices_data) do
    { "totals" =>
       { "converges" => 6,
         "deleted_nodes" => 0,
         "docs" => 47,
         "indices" => 5,
         "inspec_summaries" => 2,
         "nodes" => 1,
         "size_in_bytes" => 1082495 },
      "insights" =>
         { "totals" => { "converges" => 6, "docs" => 8, "indices" => 1, "size_in_bytes" => 769717 },
           "averages" => { "converges" => 6, "docs" => 8, "size_in_bytes" => 769717 },
           "indices" => [{ "name" => "insights-2017.10.09", "totals" => { "converges" => 6, "docs" => 8, "size_in_bytes" => 769717 } }] },
      "compliance" =>
           { "totals" => { "docs" => 38, "indices" => 1, "inspec_summaries" => 2, "size_in_bytes" => 46040 },
             "averages" => { "docs" => 38, "inspec_summaries" => 2, "size_in_bytes" => 46040 },
             "indices" => [{ "name" => "compliance-2017.10.09", "totals" => { "docs" => 38, "inspec_summaries" => 2, "size_in_bytes" => 46040 } }] },
      "node_state" => { "totals" => { "deleted_nodes" => 0, "docs" => 1, "nodes" => 1, "size_in_bytes" => 266738 } } }
  end

  before do
    allow(HighLine).to receive(:new).and_return(highline)
    allow(subject).to receive(:cluster_data).and_return(statistics_cluster_data)
    allow(subject).to receive(:indices_data).and_return(statistics_indices_data)
  end

  subject { described_class.new(unit_measure: "b") }

  shared_examples "has indices group" do
    before do
      allow(subject).to receive(idx_group).and_return(true)
    end

    it "includes the index in the text output" do
      expect(subject.summary).to match(/#{idx_group.tr("_", "-")}/)
    end

    context "with json output" do
      before do
        allow(subject).to receive(:format).and_return("json")
      end

      it "include the index and totals in the json output" do
        sum = JSON.parse(subject.summary)

        expect(sum["indices"]).to have_key(idx_group)
        expect(sum["indices"]).to have_key("totals")
        expect(sum["indices"].length).to eq(2)
      end
    end
  end

  shared_examples "convertable unit" do
    subject do
      described_class.new(
        unit_measure: unit,
        compliance: true,
        insights: true,
        node_state: true,
        cluster: true,
        format: "json"
      )
    end

    it "converts units" do
      summary = JSON.parse(subject.summary)
      expect(summary["indices"]["compliance"]["totals"]["size_in_#{unit}"]).to eq(comp_total)
      expect(summary["indices"]["insights"]["totals"]["size_in_#{unit}"]).to eq(insights_total)
      expect(summary["indices"]["node_state"]["totals"]["size_in_#{unit}"]).to eq(node_total)
      expect(summary["cluster"]["averages"]["fs_free_in_#{unit}"]).to eq(cluster_avg_fs_free)
    end
  end

  describe "#summary" do
    context "when indices_stats fails" do
      subject { described_class.new(insights: true) }

      let(:statistics_indices_data) do
        { "error" => "indices error" }
      end

      it "raises the error" do
        expect { subject.summary }.to raise_error(
          RuntimeError,
          "unable to retrieve elasticsearch data: indices error"
        )
      end
    end

    context "when cluster_stats fails" do
      let(:statistics_cluster_data) do
        { "error" => "cluster error" }
      end

      subject { described_class.new(cluster: true) }

      it "raises the error" do
        expect { subject.summary }.to raise_error(
          RuntimeError,
          "unable to retrieve elasticsearch data: cluster error"
        )
      end
    end

    context "when compliance is enabled" do
      let(:idx_group) { "compliance" }

      it_behaves_like "has indices group"
    end

    context "when insights is enabled" do
      let(:idx_group) { "insights" }

      it_behaves_like "has indices group"
    end

    context "when node_state is enabled" do
      let(:idx_group) { "node_state" }

      it_behaves_like "has indices group"
    end

    context "when all three indices groups are enabled" do
      subject do
        described_class.new(
          insights: true, compliance: true, node_state: true, unit_measure: "b"
        )
      end

      it "includes the indices in the text output" do
        expect(subject.summary).to match(/insights/)
        expect(subject.summary).to match(/compliance/)
        expect(subject.summary).to match(/node-state/)
      end

      context "with json output" do
        before do
          allow(subject).to receive(:format).and_return("json")
        end

        it "include the indices and totals in the json output" do
          sum = JSON.parse(subject.summary)

          expect(sum["indices"]).to have_key("insights")
          expect(sum["indices"]).to have_key("compliance")
          expect(sum["indices"]).to have_key("node_state")
          expect(sum["indices"]).to have_key("totals")
          expect(sum["indices"].length).to eq(4)
        end
      end
    end

    context "when cluster data is enabled" do
      subject do
        described_class.new(cluster: true, unit_measure: "b")
      end

      it "includes the indices in the text output" do
        expect(subject.summary).to match(/CLUSTER NAME/)
        expect(subject.summary).to match(/chef-insights/)
      end

      context "with json output" do
        before do
          allow(subject).to receive(:format).and_return("json")
        end

        it "include the cluster stats and averages in the json output" do
          sum = JSON.parse(subject.summary)

          expect(sum).to have_key("cluster")
          expect(sum["cluster"]).to have_key("averages")
        end
      end

      context "when the unit of measure is different" do
        context "kb" do
          let(:unit) { "kb" }
          let(:comp_total) { 44.96 }
          let(:insights_total) { 751.68 }
          let(:node_total) { 260.49 }
          let(:cluster_avg_fs_free) { 40634240.0 }

          it_behaves_like "convertable unit"
        end

        context "mb" do
          let(:unit) { "mb" }
          let(:comp_total) { 0.04 }
          let(:insights_total) { 0.73 }
          let(:node_total) { 0.25 }
          let(:cluster_avg_fs_free) { 39681.88 }

          it_behaves_like "convertable unit"
        end

        context "gb" do
          let(:unit) { "gb" }
          let(:comp_total) { 0.0 }
          let(:insights_total) { 0.0 }
          let(:node_total) { 0.0 }
          let(:cluster_avg_fs_free) { 38.75 }

          it_behaves_like "convertable unit"
        end
      end
    end
  end
end
