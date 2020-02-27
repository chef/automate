# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

# This test suite must ONLY run on iam_v1_force_upgrade_to_v2.sh.
# That test scenario tests a specific case that upgrades an old version of 
# Automate using IAM v1 to the latest Automate, which force-upgrades it to IAM v2
# The test a2-iam-v1-integration/controls/custom_access_to_migrate.rb adds a custom policy to that older version.
# In this test we verify that the policy migrated correctly as part of the force-upgrade.

require_relative '../../constants'

title 'v1 custom policy migrates to v2'

control 'iam-migrated-custom-legacy-policies-1' do
  title 'IAM access control with legacy custom policies'
  desc 'Verify that a custom v1 legacy policies are migrated to v2 during force-upgrade.'

  describe 'using custom policies' do
  
    # the first test (custom_access_to_migrate.rb) does not delete the token and user 
    # so we don't need to recreate them here

    after(:all) do
      delete_user_resp = automate_api_request(
        "/apis/iam/v2/users/#{MIGRATED_TEAM_VIEWER}",
        http_method: 'DELETE'
      )
      expect(delete_user_resp.http_status.to_s).to match(/200|404/)


      delete_token_resp = automate_api_request(
        "/apis/iam/v2/tokens/#{MIGRATED_TOKEN_ID}",
        http_method: 'DELETE'
      )
      expect(delete_token_resp.http_status.to_s).to match(/200|404/)
    end

    describe 'v1 admin token policy' do

      it 'token has been added as a member of the chef-managed admin policy' do
        admin_policy_resp = automate_api_request(
            '/apis/iam/v2/policies/administrator-access'
          )
        expect(admin_policy_resp.http_status).to eq 200

        members = admin_policy_resp.parsed_response_body[:policy][:members]
        expect(members).to include("token:#{MIGRATED_TOKEN_ID}")
      end

      it 'the v1 admin token can still access policies (admin-only resource)' do
        expect(
          automate_client_api_request(
            '/apis/iam/v2/policies',
            MIGRATED_TOKEN
          ).http_status
        ).to eq 200
      end
    end

    describe 'v1 user team-viewer policy' do

      it 'v1 custom policy has been migrated as a legacy policy on v2' do
        get_policies_resp = automate_api_request(
          '/apis/iam/v2/policies',
        )
        expect(get_policies_resp.http_status).to eq 200

        all_policies = get_policies_resp.parsed_response_body[:policies]
        legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }

        # legacy policies have randomly generated IDs, so we have to search for our migrated policy
        migrated_policy_found = false

        legacy_policies.each do |policy|
          first_statement = policy[:statements][0]
          # "auth:teams" resource is migrated to "iam:teams" and "read" is migrated to ["*:get", "*:list"]
          if first_statement[:resource] == "iam:teams" && first_statement[:actions].sort == ["*:get", "*:list"]
            migrated_policy_found = true
          end
        end

        expect(migrated_policy_found).to eq(true)
      end

      it 'the team-viewer user can still access the team list' do
        expect(
          automate_api_request(
            '/apis/iam/v2/teams',
            http_method: 'GET',
            user: MIGRATED_TEAM_VIEWER
          ).http_status
        ).to eq 200
      end
    end
  end
end
