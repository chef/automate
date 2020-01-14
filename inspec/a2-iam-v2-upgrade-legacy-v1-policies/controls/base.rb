# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

require_relative '../../constants'

include_controls 'a2-api-integration' do
  # stuff that changed with the changes in default profiles
  # see authz-access-control-1 for the IAM v2 behaviour
  skip_control 'authz-access-control-iam-v1'

  # this is using the v1 policy API, which will be disabled when using v2
  skip_control 'authz-api-crud-1'
end

# these must be run after a2-api-integration because those
# tests assume that the legacy policies exist
include_controls 'remove-legacy-policies'

title 'IAM v2.1 migration from v1 API integration tests'

control 'upgrade-iam-v2-1' do
  title 'IAM v2 Policies'
  desc 'test IAM v2 policy API post migration'

  describe 'list policies' do
    it 'includes the legacy policies we expect' do
      resp = automate_api_request('/apis/iam/v2/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }
      expect(legacy_policies.length).to eq 9
    end
  end
end

control 'upgrade-iam-v2-2' do
  title 'authz_access_control variants for IAM v2'
  desc <<EOF
In a2-api-integration, we've singled out a few checks whose results differ for
IAM v2. In this control, they're included with their NEW results.
EOF

  NON_ADMIN_USERNAME_V2 = 'inspec_test_non_admin_with_iam_v2'

  DEFAULT_PROJECT_ID = "default"

  describe 'AuthZ access control' do
    before(:all) do
      create_non_admin_request = automate_api_request(
        '/api/v0/auth/users',
        http_method: 'POST',
        request_body: {
          'name': NON_ADMIN_USERNAME_V2,
          'username': NON_ADMIN_USERNAME_V2,
          'password': ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_non_admin_request.http_status.to_s).to match(/200|409/)

      test_token_request = automate_api_request(
        '/api/v0/auth/tokens',
        http_method: 'POST',
        request_body: {
          'description': 'inspec_test_token',
          'active': true
        }.to_json
      )
      TEST_TOKEN_V2 = test_token_request.parsed_response_body[:value]
      TEST_TOKEN_ID_V2= test_token_request.parsed_response_body[:id]

      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_non_admin_request = automate_api_request(
        "/api/v0/auth/users/#{NON_ADMIN_USERNAME_V2}",
        http_method: 'DELETE',
      )
      expect(delete_non_admin_request.http_status.to_s).to match(/200|404/)

      delete_token_request = automate_api_request(
        "/api/v0/auth/tokens/#{TEST_TOKEN_ID_V2}",
        http_method: 'DELETE',
      )
      expect(delete_token_request.http_status.to_s).to match(/200|404/)
    end

    # compliance:profiles for client calls should be permitted by default
    describe '/api/v0/compliance/profiles/search as client' do
      it "api/v0/compliance/profiles/search returns 200 for client" do
        expect(
          automate_client_api_request(
            "/api/v0/compliance/profiles/search",
            TEST_TOKEN_V2,
            http_method: 'POST'
          ).http_status
        ).to eq(200)
      end
    end

    describe 'legacy data-collector endpoints' do
      %w(
        /api/v0/events/data-collector
        /data-collector/v0
      ).each do |url|
        { 'GET': 200, 'POST': 400 }.each do |method, status|
          it "#{method} #{url} returns #{status} for client" do
            expect(
              automate_client_api_request(
                url,
                TEST_TOKEN_V2,
                http_method: method,
              ).http_status
            ).to eq(status)
          end
        end
      end
    end

    describe '/api/v0/ingest/events/chef/' do
      {
        run: { entity_uuid: 'a6597b02-2d83-47ce-8e46-84d1e85be6c7' },
        action: {},
        nodedelete: { node_id: 'a6597b02-2d83-47ce-8e46-84d1e85be6c7' },
      }.each do |url, body|
        it "#{url} returns 403 for client" do
          expect(
            automate_client_api_request(
              "/api/v0/ingest/events/chef/#{url}",
              TEST_TOKEN_V2,
              http_method: 'POST',
              request_body: body.to_json,
            ).http_status
          ).not_to eq(403)
        end
      end
    end

    # ingest multiple delete
    describe '/api/v0/ingest/events/chef/node-multiple-deletes' do
      it "returns 403 for client" do
        expect(
            automate_client_api_request(
              "/api/v0/ingest/events/chef/node-multiple-deletes",
              TEST_TOKEN_V2,
              http_method: 'POST',
              request_body: { node_ids: [ 'fake-2d83-47ce-8e46-84d1e85be6c7' ] }.to_json,
            ).http_status
          ).to eq(403)
      end
    end

    shared_examples 'authz access control (iam v2)' do
      # ingest
      describe '/api/v0/ingest/events/chef/' do
        {
          'run': { 'entity_uuid': 'a6597b02-2d83-47ce-8e46-84d1e85be6c7' },
          'action': {},
          'nodedelete': { 'node_id': 'a6597b02-2d83-47ce-8e46-84d1e85be6c7' },
        }.each do |url, body|
          it "#{url} returns 403 for user" do
              expect(
                automate_api_request(
                  "/api/v0/ingest/events/chef/#{url}",
                  http_method: 'POST',
                  user: user,
                  request_body: body.to_json,
                ).http_status
              ).not_to eq(403)
          end
        end
      end

      # ingest multiple delete
      describe '/api/v0/ingest/events/chef/node-multiple-deletes' do
        it "returns 403 for user" do
            expect(
              automate_api_request(
                "/api/v0/ingest/events/chef/node-multiple-deletes",
                http_method: 'POST',
                user: user,
                request_body: { node_ids: [ 'fake-2d83-47ce-8e46-84d1e85be6c7' ] }.to_json,
              ).http_status
            ).not_to eq(403)
        end
      end

      # legacy data-collector
      describe 'legacy data-collector endpoints' do
        %w(
          /api/v0/events/data-collector
          /data-collector/v0
        ).each do |url|
          %w(GET POST).each do |method|
            it "#{method} #{url} returns 403 for user" do
                expect(
                  automate_api_request(
                    url,
                    http_method: method,
                    user: user,
                  ).http_status
                ).not_to eq(403)
            end
          end
        end
      end

      describe 'chef-managed policies' do
        it 'update returns 403' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/policies/#{id}",
              http_method: 'PUT',
              user: user,
              request_body: {
                name: "inspec-test-policy-#{Time.now.utc.to_i}",
                description: 'trying to update this chef-managed policy',
                members: []
              }.to_json
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'delete returns 403' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/policies/#{id}",
              http_method: 'DELETE',
              user: user
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'adding members returns 200' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/policies/#{id}/members:add",
              http_method: 'POST',
              request_body: {
                members: ["user:local:newmember1", "team:local:newmember2"],
              }.to_json
            )
            expect(resp.http_status).to eq(200)
          end
        end

        it 'removing members returns 200' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/policies/#{id}/members:remove",
              http_method: 'POST',
              request_body: {
                members: ["user:local:newmember1", "team:local:newmember2"],
              }.to_json
            )
            expect(resp.http_status).to eq(200)
          end
        end
      end

      describe 'chef-managed roles' do
        it 'update returns 403' do
          DEFAULT_ROLE_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/roles/#{id}",
              http_method: 'PUT',
              user: user,
              request_body: {
                name: "inspec-test-policy-#{Time.now.utc.to_i}",
                description: 'trying to update this chef-managed role',
                members: []
              }.to_json
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'delete returns 403' do
          DEFAULT_ROLE_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2/roles/#{id}",
              http_method: 'DELETE',
              user: user
            )
            expect(resp.http_status).to eq(403)
          end
        end
      end

      describe "when the user tries to delete itself" do
        it "user cannot delete itself" do
          resp = automate_api_request("/api/v0/auth/users/#{user}",
            http_method: 'DELETE',
            user: user)
          expect(resp.http_status).to eq(403)
        end
      end
    end

    describe 'when making requests as an admin' do
      let(:user) { ADMIN_USER_ID }
      include_examples 'authz access control (iam v2)'
    end

    describe 'when making requests as a non-admin' do
      let(:user) { NON_ADMIN_USERNAME_V2 }
      include_examples 'authz access control (iam v2)'
    end
  end
end

