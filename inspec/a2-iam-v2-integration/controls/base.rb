# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2 migration API integration tests'

include_controls 'a2-api-integration' do
   # this is for pre-migration v2 status
  skip_control 'iam-v2-1'

  # stuff that changed with the changes in default profiles
  # see below for the IAM v2 behaviour
  skip_control 'authz-access-control-iam-v1'

  # this is using the v1 policy API, which will be disabled when using v2
  skip_control 'authz-api-crud-1'
end

control 'upgrade-iam-v2-1' do
  title 'IAM v2 Policies'
  desc 'test IAM v2 policy API post migration'

  describe 'list policies' do
    it 'includes the legacy policies we expect' do
      resp = automate_api_request('/apis/iam/v2beta/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }
      expect(legacy_policies.length).to eq 9
    end

    it 'includes the IAM v2 default policies' do
      resp = automate_api_request('/apis/iam/v2beta/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      other_policies = all_policies
        .reject{ |p| /^[a-f0-9-]+ \(custom\)$/.match(p[:name]) }
        .reject{ |p| /^\[Legacy\]/.match(p[:name]) }

      # default policies automatically added during migration
      expect(other_policies.length).to eq 4
    end

    it 'the editors default policy includes editor role' do
      resp = automate_api_request('/apis/iam/v2beta/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      policies = all_policies.select{ |p| /^team:local:viewers$/.match(p[:members][0]) }
      expect(policies.length).to eq 1
    end

    it 'the viewers default policy includes editor role' do
      resp = automate_api_request('/apis/iam/v2beta/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      policies = all_policies.select{ |p| /^team:local:editors$/.match(p[:members][0]) }
      expect(policies.length).to eq 1
    end
  end
end

control 'upgrade-iam-v2-2' do
  title 'authz_access_control variants for IAM v2'
  desc <<EOF
In a2-api-integration, we've singled out a few checks whose results differ for
IAM v2. In this control, they're included with their NEW results.
EOF

  DEFAULT_POLICY_IDS = [
    "administrator-access",
    "editor-access",
    "viewer-access",
    "ingest-access"
  ]

  DEFAULT_ROLE_IDS = [
    "owner",
    "editor",
    "viewer",
    "ingest",
    "project-admin",
    "iam-members-viewer"
  ]

  NON_ADMIN_USERNAME_V2 = 'inspec_test_non_admin_with_iam_v2'
  TEST_TOKEN_ID_V2 = 'inspec_test_token_with_iam_v2'

  DEFAULT_PROJECT_ID = "default"

  describe 'AuthZ access control' do
    before(:all) do
      create_non_admin_request = automate_api_request(
        '/apis/iam/v2beta/users',
        http_method: 'POST',
        request_body: {
          id: NON_ADMIN_USERNAME_V2,
          name: NON_ADMIN_USERNAME_V2,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_non_admin_request.parsed_response_body).to eq({})
      expect(create_non_admin_request.http_status.to_s).to match(/200|409/)

      test_token_request = automate_api_request(
        '/apis/iam/v2beta/tokens',
        http_method: 'POST',
        request_body: {
          id: TEST_TOKEN_ID_V2,
          name: 'inspec_test_token',
        }.to_json
      )
      TEST_TOKEN_V2 = test_token_request.parsed_response_body[:value]

      expect(test_token_request.parsed_response_body).to eq({})
      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_non_admin_request = automate_api_request(
        "/apis/iam/v2beta/users/#{NON_ADMIN_USERNAME_V2}",
        http_method: 'DELETE',
      )
      expect(delete_non_admin_request.http_status.to_s).to match(/200|404/)

      delete_token_request = automate_api_request(
        "/apis/iam/v2beta/tokens/#{TEST_TOKEN_ID_V2}",
        http_method: 'DELETE',
      )
      expect(delete_token_request.http_status.to_s).to match(/200|404/)
    end

    # compliance:profiles for client calls should be permitted by default
    describe '/api/v0/compliance/profiles/search as client' do
      it "api/v0/compliance/profiles/search returns the correct response code for client" do
        resp = automate_client_api_request(
            "/api/v0/compliance/profiles/search",
            TEST_TOKEN_V2,
            http_method: 'POST'
        )
        expect(resp.parsed_response_body).to eq({}) # XXX just want to see the error
        expect(resp.http_status).to eq(200)
      end
    end

    describe 'legacy data-collector endpoints' do
      %w(
        /api/v0/events/data-collector
        /data-collector/v0
      ).each do |url|
        { 'GET': 200, 'POST': 400 }.each do |method, status|
          it "#{method} #{url} returns the correct response code for client" do
            resp = automate_client_api_request(
                url,
                TEST_TOKEN_V2,
                http_method: method,
            )
            expect(resp.parsed_response_body).to eq({}) # XXX just want to see the error
            expect(resp.http_status).to eq(status)
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
        it "#{url} returns the correct response code for client" do
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
      it "returns the correct response code for client" do
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
          it "#{url} returns the correct response code for user" do
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
        it "returns the correct response code for user" do
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
            it "#{method} #{url} returns the correct response code for user" do
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
            resp = automate_api_request("/apis/iam/v2beta/policies/#{id}",
              http_method: 'PUT',
              user: user,
              request_body: {
                name: "inspec-test-policy-#{Time.now.utc.to_i}",
                members: []
              }.to_json
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'delete returns 403' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2beta/policies/#{id}",
              http_method: 'DELETE',
              user: user
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'adding members returns 200' do
          DEFAULT_POLICY_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2beta/policies/#{id}/members:add",
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
            resp = automate_api_request("/apis/iam/v2beta/policies/#{id}/members:remove",
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
            resp = automate_api_request("/apis/iam/v2beta/roles/#{id}",
              http_method: 'PUT',
              user: user,
              request_body: {
                name: "inspec-test-policy-#{Time.now.utc.to_i}",
                members: []
              }.to_json
            )
            expect(resp.http_status).to eq(403)
          end
        end

        it 'delete returns 403' do
          DEFAULT_ROLE_IDS.each do |id|
            resp = automate_api_request("/apis/iam/v2beta/roles/#{id}",
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
      let(:user) { 'admin' }
      include_examples 'authz access control (iam v2)'
    end

    describe 'when making requests as a non-admin' do
      let(:user) { NON_ADMIN_USERNAME_V2 }
      include_examples 'authz access control (iam v2)'
    end
  end
end

control 'upgrade-iam-v2-3' do
  title 'IAM v2 admin token'
  desc 'test IAM v2 admin token post migration'

  # only run this if we've been handed an admin token to use here
  only_if { ENV['INSPEC_UPGRADE_IAM_V2_3_ADMIN_TOKEN'] }

  describe 'IAM v2 admin token' do
    it 'GET iam/v2beta/policies returns 200 for admin token generated by iam token create --admin' do
      token = ENV['INSPEC_UPGRADE_IAM_V2_3_ADMIN_TOKEN']

      expect(
        automate_client_api_request(
          '/apis/iam/v2beta/policies',
          token,
          http_method: 'GET',
        ).http_status
      ).to eq(200)
    end
  end
end
