# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'Elasticsearch Data Migration Integration Tests'

control 'elastic-data-migration-1' do
  title 'elastic data migration'
  desc 'Verify A1 data is copied to A2\'s Elasticsearch'

  describe "A1 Indices Are Migrated to A2 ES" do

    subject do
      # locky mc lockface index doesn't exist in a2, it will only be here if we
      # correctly migrated
      http('http://localhost:10141/.locky', enable_remote_worker: true)
    end

    its('status') { should eq 200 }

    it "returns JSON data about locky mc lockface" do
      data = JSON.parse(subject.body)
      expect(data).to have_key(".locky")
    end
  end

  describe "Config MGMT node-state A1 Index is Migrated to A2 ES" do

    subject do
      http('http://localhost:10141/node-state', enable_remote_worker: true)
    end

    its('status') { should eq 200 }

    it "returns JSON data about node-state-6" do
      data = JSON.parse(subject.body)
      expect(data).to have_key("node-state-6")
    end

  end

  describe "Config MGMT node-attribute A1 Index is Migrated to A2 ES" do

    subject do
      http('http://localhost:10141/node-attribute', enable_remote_worker: true)
    end

    its('status') { should eq 200 }

    it "returns JSON data about node-attribute" do
      data = JSON.parse(subject.body)
      expect(data).to have_key("node-attribute")
    end

  end

end
