require 'spec_helper.rb'
require 'migrate-compliance/migrate-compliance'

describe 'migrate_insights_to_compliance' do
  before do
    # keep test output clean
    allow(MigrateCompliance::Log).to receive(:info)
    allow(MigrateCompliance::Log).to receive(:error)
  end
  let(:insights_profile) {
    {"name"=>"linux-baseline",
     "title"=>"DevSec Linux Security Baseline",
     "version"=>"2.1.0",
     "controls"=>
      [{"title"=>"IPv4 Forwarding",
        "desc"=>
         "If you're not intending for your system to forward traffic between interfaces, or if you only have a single interface, the forwarding function must be disable.",
        "impact"=>1.0,
        "refs"=>[],
        "tags"=>{},
        "id"=>"sysctl-01",
        "results"=>
         [{"status"=>"passed",
           "code_desc"=>"Kernel Parameter net.ipv4.ip_forward value should eq 0",
           "run_time"=>0.027176204,
           "start_time"=>"2017-06-07 11:38:25 +0000"},
          {"status"=>"passed",
           "code_desc"=>"Kernel Parameter net.ipv4.conf.all.forwarding value should eq 0",
           "run_time"=>0.024983648,
           "start_time"=>"2017-06-07 11:38:26 +0000"}]}]}
  }
  let(:es_empty_search) {
    {"took"=>2,
     "timed_out"=>false,
     "_shards"=>{"total"=>5, "successful"=>5, "failed"=>0},
     "hits"=>{"total"=>0, "max_score"=>nil, "hits"=>[]}}
  }
  let(:es_one_profile) {
    {"took"=>2,
     "timed_out"=>false,
     "_shards"=>{"total"=>5, "successful"=>5, "failed"=>0},
     "hits"=>
      {"total"=>1,
       "max_score"=>12.343222,
       "hits"=>
        [{"_index"=>"compliance-profiles-1",
          "_type"=>"inspec_profile",
          "_id"=>"d2dcf9bf37feec01b31d743fbee6965f55218f730512e21308260c840f4f6654",
          "_score"=>12.343222,
          "_source"=>
           { "name"=>"linux-baseline",
             "version"=>"2.1.0",
             "sha256"=>"d2dcf9bf37feec01b31d743fbee6965f55218f730512e21308260c840f4f6654",
             "controls"=>
             [{"refs"=>"[]",
               "impact"=>1.0,
               "id"=>"sysctl-01",
               "title"=>"IPv4 Forwarding",
               "desc"=>
                "If you're not intending for your system to forward traffic between interfaces, or if you only have a single interface, the forwarding function must be disable.",
               "tags"=>"{}"}]}}]}}
  }

  let(:client) { double("client") }

  before do
    allow(MigrateCompliance::ElasticSearch).to receive(:elasticsearch_client).and_return(client)
  end


  context "when ElasticSearch is NOT reachable" do
    before do
      allow(client).to receive(:ping).and_raise(RuntimeError.new("No node available"))
    end
    it "elasticsearch_reachable? returns false" do
      expect(MigrateCompliance::ElasticSearch.elasticsearch_reachable?).to eq(false)
    end
    it "an exception is raised" do
      expect{ MigrateCompliance::ElasticSearch.migrate_insights_to_compliance }.to raise_error(
        RuntimeError, /ERROR: ElasticSearch is not reachable, aborting migration/
      )
    end
  end


  context "when ElasticSearch is reachable" do
    describe "and the profile to be migrated is NOT in compliance-profiles" do
      before do
        allow(client).to receive(:search).and_return(es_empty_search)
      end
      let(:profile_exists) { MigrateCompliance::ElasticSearch.profile_exists?(insights_profile) }
      it "profile_exists? returns false and new sha256 is added to the profile" do
        expect(profile_exists).to eq(false)
        expect(insights_profile['sha256']).to eq('b9c835e9dfcadbb889b5ae172505ff16da74da28771cc8796a8f9c05229eac36')
      end
    end

    describe "and the profile to be migrated is in compliance-profiles with a different version" do
      before do
        es_one_profile['hits']['hits'][0]['_source']['version'] = '5.5.5'
        allow(client).to receive(:search).and_return(es_one_profile)
      end
      let(:profile_exists) { MigrateCompliance::ElasticSearch.profile_exists?(insights_profile) }
      it "profile_exists? returns false and new sha256 is added to the profile" do
        expect(profile_exists).to eq(false)
        expect(insights_profile['sha256']).to eq('b9c835e9dfcadbb889b5ae172505ff16da74da28771cc8796a8f9c05229eac36')
      end
    end

    describe "and the profile to be migrated is in compliance-profiles" do
      before do
        allow(client).to receive(:ping).and_return(true)
        allow(client).to receive(:search).and_return(es_one_profile)
      end
      let(:profile_exists) { MigrateCompliance::ElasticSearch.profile_exists?(insights_profile) }
      it "profile_exists? returns true and the sha256 value is reused from the ElasticSearch match" do
        expect(profile_exists).to eq(true)
        expect(insights_profile['sha256']).to eq('d2dcf9bf37feec01b31d743fbee6965f55218f730512e21308260c840f4f6654')
      end
    end

    describe "migrate_insights_to_compliance" do
      let(:es_search_result) { { "_scroll_id" => "scroll_id",
                                 "hits"       => {"total"=>1} } }
      let(:es_scroll_result) {
        {"_scroll_id"=>"c2NhbjswOzE7dG90YWxfaGl0czoxOw==",
         "took"=>3,
         "timed_out"=>false,
         "_shards"=>{"total"=>10, "successful"=>10, "failed"=>0},
         "hits"=>
          {"total"=>1,
           "max_score"=>0.0,
           "hits"=>
            [{"_index"=>"insights-2017.06.07",
              "_type"=>"inspec",
              "_id"=>"AVyCWWVLg9uyZsjeBg-k",
              "_score"=>0.0,
              "_source"=>
               {
                 "event_type"=>"inspec",
                 "event_action"=>"exec",
                 "compliance_summary"=>
                  {"total"=>324,
                   "passed"=>{"total"=>114},
                   "skipped"=>{"total"=>1},
                   "failed"=>{"total"=>171, "minor"=>1, "major"=>0, "critical"=>170},
                   "status"=>"failed",
                   "node_name"=>"mysuite-centos6",
                   "end_time"=>"2017-06-07T11:38:35+00:00",
                   "duration"=>9.755170865,
                   "inspec_version"=>"1.23.0"},
                 "entity_uuid"=>"1e47101d-841e-4fe2-8dd5-16c70b91be0a",
                 "run_id"=>"26187bd9-db2e-4ce3-9981-38465b5569f1",
                 "@version"=>"1",
                 "@timestamp"=>"2017-06-07T11:38:35.000Z",
                 "tags"=>["chef"],
                 "type"=>"inspec",
                 "@uuid"=>"c25c21f6-c823-407a-a046-da5e8ce8b34e",
                 "profiles"=>
                 [{"name"=>"linux-baseline",
                   "title"=>"DevSec Linux Security Baseline",
                   "maintainer"=>"DevSec Hardening Framework Team",
                   "copyright"=>"DevSec Hardening Framework Team",
                   "copyright_email"=>"hello@dev-sec.io",
                   "license"=>"Apache 2 license",
                   "summary"=>"Test-suite for best-preactice Linux OS hardening",
                   "version"=>"9.9.9",
                   "supports"=>[{"os-family"=>"linux"}],
                   "controls"=>
                   [{"title"=>"IPv4 Forwarding",
                     "desc"=>
                      "If you're not intending for your system to forward traffic between interfaces, or if you only have a single interface, the forwarding function must be disable.",
                     "impact"=>1.0,
                     "refs"=>[],
                     "tags"=>{},
                     "code"=>
                      "control 'sysctl-01' do\n  impact 1.0\n  title 'IPv4 Forwarding'\n  desc \"If you're not intending for your system to forward traffic between interfaces, or if you only have a single interface, the forwarding function must be disable.\"\n  describe kernel_parameter('net.ipv4.ip_forward') do\n    its(:value) { should eq 0 }\n  end\n  describe kernel_parameter('net.ipv4.conf.all.forwarding') do\n    its(:value) { should eq 0 }\n  end\n  only_if { sysctl_forwarding == false }\nend\n",
                     "source_location"=>{"ref"=>"/root/profiles/linux-baseline-2.1.0/controls/sysctl_spec.rb", "line"=>22},
                     "id"=>"sysctl-01",
                     "results"=>
                      [{"status"=>"passed",
                        "code_desc"=>"Kernel Parameter net.ipv4.ip_forward value should eq 0",
                        "run_time"=>0.027176204,
                        "start_time"=>"2017-06-07 11:38:25 +0000"},
                       {"status"=>"passed",
                        "code_desc"=>"Kernel Parameter net.ipv4.conf.all.forwarding value should eq 0",
                        "run_time"=>0.024983648,
                        "start_time"=>"2017-06-07 11:38:26 +0000"}]}]}]}}]}}
      }
      before do
        allow(client).to receive(:ping).and_return(true)
        allow(client).to receive(:search).with(
          hash_including(index: 'insights-*')).and_return(es_search_result)
        allow(client).to receive(:scroll).exactly(2).times.and_return(es_scroll_result, nil)
        allow(client).to receive(:get).and_return(
          {
            "_index"=>"node-state-1",
            "_type"=>"node-state",
            "_id"=>"1e47101d-841e-4fe2-8dd5-16c70b91be0a",
            "_version"=>4,
            "found"=>true,
            "fields"=>{"platform_version"=>["6.6"], "platform"=>["centos"], "environment"=>["_default"]}
          }
        )
        allow(client).to receive_message_chain('indices.exists?').and_return(true)
        allow(client).to receive(:update).and_return({"_shards"=>{"failed"=>0}})
      end

      it "elasticsearch_reachable? returns true" do
        expect(MigrateCompliance::ElasticSearch.elasticsearch_reachable?).to eq(true)
      end

      context "when the profile does not exist in compliance-profiles" do
        before do
          allow(client).to receive(:search).with(
            hash_including(index: 'compliance-profiles')).and_return(es_one_profile)
        end
        it "migrate all the things, including profile" do
          expect(client).to receive(:search).with(hash_including(index: 'insights-*', search_type: 'scan'))
          expect(client).to receive(:scroll).with(hash_including(scroll: '5m'))
          expect(client).to receive(:get).with(hash_including(index: 'node-state'))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-profiles",   :type=>"inspec_profile", :id=>"3b03c0730beac75f6673a85e861734a8f10e80028408a08388ab0dda9cdadc0f"))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-2017.06.07", :type=>"inspec_report",  :id=>"26187bd9-db2e-4ce3-9981-38465b5569f1"))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-2017.06.07", :type=>"inspec_summary", :id=>"26187bd9-db2e-4ce3-9981-38465b5569f1"))
          expect(client).to receive(:update).with(hash_including(:index=>"insights-2017.06.07",   :type=>"inspec",         :id=>"AVyCWWVLg9uyZsjeBg-k"))
          expect(MigrateCompliance::ElasticSearch.migrate_insights_to_compliance).to eq(1)
        end
      end

      context "when the profile already exists in compliance-profiles" do
        before do
          es_one_profile['hits']['hits'][0]['_source']['version'] = '2.1.0'
          allow(client).to receive(:search).with(
            hash_including(index: 'compliance-profiles')).and_return(es_one_profile)
        end
        it "migrate all the things, but reuse profile" do
          expect(client).to receive(:search).with(hash_including(index: 'insights-*', search_type: 'scan'))
          expect(client).to receive(:scroll).with(hash_including(scroll: '5m'))
          expect(client).to receive(:get).with(hash_including(index: 'node-state'))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-profiles",   :type=>"inspec_profile", :id=>"3b03c0730beac75f6673a85e861734a8f10e80028408a08388ab0dda9cdadc0f"))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-2017.06.07", :type=>"inspec_report",  :id=>"26187bd9-db2e-4ce3-9981-38465b5569f1"))
          expect(client).to receive(:update).with(hash_including(:index=>"compliance-2017.06.07", :type=>"inspec_summary", :id=>"26187bd9-db2e-4ce3-9981-38465b5569f1"))
          expect(client).to receive(:update).with(hash_including(:index=>"insights-2017.06.07",   :type=>"inspec",         :id=>"AVyCWWVLg9uyZsjeBg-k"))
          expect(MigrateCompliance::ElasticSearch.migrate_insights_to_compliance).to eq(1)
        end
      end

    end
  end
end
