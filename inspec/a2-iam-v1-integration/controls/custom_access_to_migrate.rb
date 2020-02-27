# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

# This test suite must ONLY run on iam_v1_force_upgrade_to_v2.sh because it uses old v1 APIs.
# That test scenario tests a specific case that upgrades an old version of 
# Automate using IAM v1 to the latest Automate, which force-upgrades it to IAM v2
# This test adds a custom policy to that older version to verify that it migrates
# correctly as part of the force-upgrade.

require_relative '../../constants'

title 'v1 custom policy migrates to v2'

control 'iam-custom-legacy-policies-to-migrate-1' do
  title 'IAM access control with legacy custom policies'
  desc 'Verify that a custom v1 legacy policies are migrated to v2 during force-upgrade.'

  describe 'using custom policies' do
  
    before(:all) do
      create_non_admin_resp = automate_api_request(
        # we need to keep this deprecated path here because this test is run
        # on an older version of Automate
        '/api/v0/auth/users',
        http_method: 'POST',
        request_body: {
          'username': MIGRATED_TEAM_VIEWER,
          'name': MIGRATED_TEAM_VIEWER,
          'password': ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )

      expect(create_non_admin_resp.http_status.to_s).to match(/200|409/)

      test_token_resp = automate_api_request(
        # we need to keep this deprecated path here because this test is run
        # on an older version of Automate
        '/api/v0/auth/tokens',
        http_method: 'POST',
        request_body: {
          'id': MIGRATED_TOKEN_ID,
          'value': MIGRATED_TOKEN,
          'description': 'v1 inspec admin token to be migrated',
          'active': true
        }.to_json
      )
      TOKEN = test_token_resp.parsed_response_body[:value]

      expect(test_token_resp.http_status).to eq(200)
    end

    # we don't delete the user, token, or policies because they must be migrated to v2 in the integration test 

    describe 'v1 admin token policy' do
      it 'can create policy for token to make it an admin token' do
        expect(
          automate_api_request(
            # we need to keep this deprecated path here because this test is run
            # on an older version of Automate
            '/api/v0/auth/policies',
            http_method: 'POST',
            request_body: {
              subjects: ["token:#{MIGRATED_TOKEN_ID}"],
              action: "*",
              resource: "*"
            }.to_json
          ).http_status
        ).to eq 200
      end

      it 'token with admin permissions can access policies (admin-only resource)' do
        expect(
          automate_client_api_request(
            # we need to keep this deprecated path here because this test is run
            # on an older version of Automate
            '/api/v0/auth/policies',
            TOKEN
          ).http_status
        ).to eq 200
      end
    end

    describe 'v1 user team-viewer policy' do
      it 'can create policy for a user to grant them access to view the teams list' do
        expect(
          automate_api_request(
            # we need to keep this deprecated path here because this test is run
            # on an older version of Automate
            '/api/v0/auth/policies',
            http_method: 'POST',
            request_body: {
              subjects: ["user:local:#{MIGRATED_TEAM_VIEWER}"],
              action: "read",
              resource: "auth:teams"
            }.to_json
          ).http_status
        ).to eq 200
      end

      it 'the team-viewer user can access the team list' do
        expect(
          automate_api_request(
            # we need to keep this deprecated path here because this test is run
            # on an older version of Automate
            '/api/v0/auth/teams',
            http_method: 'GET',
            user: MIGRATED_TEAM_VIEWER
          ).http_status
        ).to eq 200
      end
    end
  end
end
