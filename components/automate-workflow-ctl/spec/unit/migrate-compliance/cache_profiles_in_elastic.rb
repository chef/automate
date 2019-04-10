require 'spec_helper.rb'
require 'migrate-compliance/migrate-compliance'

describe 'cache_profiles_in_elastic' do
  let(:delivery_config_content) { File.read(File.expand_path((File.join(__dir__, "../../fixtures/delivery-running.json")))) }
  let(:delivery_config) { JSON.parse(delivery_config_content) }

  before do
    # keep test output clean
    allow(MigrateCompliance::Log).to receive(:info)
    allow(MigrateCompliance::Log).to receive(:error)
  end

  let(:client) { double("client") }

  before do
    allow(MigrateCompliance::ElasticSearch).to receive(:elasticsearch_client).and_return(client)
  end

  context "when delivery config is NOT available" do
    before do
      allow(client).to receive(:ping).and_return(true)
      allow(File).to receive(:exist?).and_return(false)
    end
    it "an exception is raised" do
      expect{ MigrateCompliance::ElasticSearch.cache_profiles_in_elastic }.to raise_error(
        RuntimeError, /You must configure Chef Automate before running the migrate-compliance/
      )
    end
  end

  context "when ElasticSearch is NOT reachable" do
    before do
      allow(client).to receive(:ping).and_raise(RuntimeError.new("No node available"))
      allow(MigrateCompliance::Config).to receive(:delivery_running).and_return(delivery_config)
    end
    it "elasticsearch_reachable? returns false" do
      expect(MigrateCompliance::ElasticSearch.elasticsearch_reachable?).to eq(false)
    end
    it "an exception is raised" do
      expect{ MigrateCompliance::ElasticSearch.cache_profiles_in_elastic }.to raise_error(
        RuntimeError, /ERROR: ElasticSearch is not reachable, aborting profiles caching/
      )
    end
  end

  context "when delivery config is available and ElasticSearching ponging" do
    let(:cmd) { "/opt/delivery/embedded/service/compliance-profiles/bin/compliance rebuild-cache --profiles-path '/var/opt/delivery/compliance/profiles' --es-url 'http://localhost:8080/elasticsearch' --log-level 'info'" }
    before do
      allow(client).to receive(:ping).and_return(true)
      allow(File).to receive(:exist?).and_return(true)
      allow(File).to receive(:read).and_return(delivery_config_content)
      allow(MigrateCompliance::System).to receive(:run).exactly(1).times.with(cmd)
    end
    it "rebuild-cache subcommand is executed" do
      expect(MigrateCompliance::System).to receive(:run).exactly(1).times.with(cmd)
      expect(MigrateCompliance::ElasticSearch.cache_profiles_in_elastic).to eq(nil)
    end
  end
end
