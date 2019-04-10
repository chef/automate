# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'Filesystem Data Migration Integration Tests'

control 'filesystem-data-migration-1' do
  title 'filesystem data migration'
  desc 'Verify filesystem-persisted A1 data is copied to A2\'s directory layout'

  describe "Notification data is copied to /hab/svc/notifications-service/data" do

    subject do
      file("/hab/svc/notifications-service/data/rule_store")
    end

    it { should exist }
  end

  describe "Secrets service's a1 secrets key is copied to /hab/svc/secrets-service/data/secrets_key" do

    subject do
      file("/hab/svc/secrets-service/data/secrets_key")
    end

    it { should exist }
    its("content") { should match /21c9a5a2d80339aadbd404efe6e8cc6e/ }
  end

  describe "Local-user-service data directory is created, /hab/svc/local-user-service/data" do

    subject do
      file("/hab/svc/local-user-service/data")
    end

    it { should exist }
    its("mode") { should eq 0770 }
  end

  unless ENV['NO_WORKFLOW_TEST'] == "true"
    describe "Workflow Server git repo data directory is created, /hab/svc/automate-workflow-server/data/git/repos" do

      subject do
        file("/hab/svc/automate-workflow-server/data/git/repos")
      end

      it { should exist }
      it { should be_directory }
      its("mode") { should eq 0700 }
    end

    describe "Workflow Server git repositories are migrated (four directories inside)" do
      subject do
        Dir.entries('/hab/svc/automate-workflow-server/data/git/repos').length
      end

      # We check for 6 things since Dir entries counts '.' and '..' as directories
      it { should eq 6 }
    end
  end
end
