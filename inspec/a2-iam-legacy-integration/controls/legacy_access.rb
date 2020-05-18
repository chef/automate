# encoding: utf-8
# copyright: 2020, Chef Software, Inc.
# license: All rights reserved

# This test suite is meant to verify APIs return 403 or not, given legacy policy permissions.
# Please don't test for API behavior beyond that;
# only for whether the invoker is authorized or not to make the call.

require_relative '../../constants'

title 'IAM access control integration tests'

NON_ADMIN_USERNAME = 'inspec_test_non_admin'

control 'iam-legacy-access-control-1' do
  title 'IAM access control with legacy policies'
  desc 'Verify proper access for both admin and users who are not members of any policies.
  V1 Legacy policies gave all users default permissions to most APIs, besides the IAM APIs.'

  describe 'IAM access control with migrated legacy policies' do
    before(:all) do
      create_non_admin_request = automate_api_request(
        '/apis/iam/v2/users',
        http_method: 'POST',
        request_body: {
          'id': NON_ADMIN_USERNAME,
          'name': NON_ADMIN_USERNAME,
          'password': ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )

      expect(create_non_admin_request.http_status.to_s).to match(/200|409/)

      test_token_request = automate_api_request(
        '/apis/iam/v2/tokens',
        http_method: 'POST',
        request_body: {
          'id': "inspec_test_token-#{TIMESTAMP}",
          'name': 'inspec_test_token'
        }.to_json
      )
      TEST_TOKEN = test_token_request.parsed_response_body[:token][:value]
      TEST_TOKEN_ID = test_token_request.parsed_response_body[:token][:id]

      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_non_admin_request = automate_api_request(
        "/apis/iam/v2/users/#{NON_ADMIN_USERNAME}",
        http_method: 'DELETE',
      )
      expect(delete_non_admin_request.http_status.to_s).to match(/200|404/)


      delete_token_request = automate_api_request(
        "/apis/iam/v2/tokens/#{TEST_TOKEN_ID}",
        http_method: 'DELETE',
      )
      expect(delete_token_request.http_status.to_s).to match(/200|404/)
    end

    it 'includes the legacy policies we expect' do
      resp = automate_api_request('/apis/iam/v2/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }
      legacy_policy_ids = legacy_policies.map { |p| p[:id] }

      expected_policies = [
        "events-access-legacy",
        "ingest-access-legacy",
        "nodes-access-legacy",
        "node-managers-access-legacy",
        "secrets-access-legacy",
        "compliance-profile-access-legacy",
        "telemetry-access-legacy",
        "compliance-access-legacy",
        "infrastructure-automation-access-legacy"
      ]

      expected_policies.each do |policy|
        expect(legacy_policy_ids).to include(policy)
      end
    end

    # This shared example will attempt to test all CRUD operations in a standard way on a URL.
    # If the pattern used here doesn't fit what you are testing, feel free to not use it!
    # It can test getting all objects, as well as CRUD on a single object.
    # It also supports just seeing a failure on every HTTP verb.
    #
    # Inputs:
    #  expect_403_response: Boolean representing whether or not you are expecting a 403 UNAUTHORIZED
    #      Example: `let(:expect_403_response) { true }` to test 403s for non-admins on admin only APIs.
    #
    #  success_expected: Whether or not you expect the requests to succeed.
    #                    If false you must also pass failure_test_id.
    #
    #  failure_test_id: The string of an ID that will pass validation for your URL.
    #                   Should be fake, just needs to pass validation so we see the expected 403.
    #    Example: `let(:failure_test_id) { 'some_username' }`
    #
    #  http_verbs: All the http methods you wish to test in an array, can contain some or all of
    #              GET_ALL, GET, POST, PUT, DELETE, where GET_ALL is GET on the base URL to retrieve all
    #              objects. If you want GET, PUT, or DELETE, you must also pass POST as you need an object
    #              to perform those actions on.
    #
    #  url: The base URL you wish to test against.
    #    Example: `let(:url) { '/apis/iam/v2/users' }`
    #
    #  id_keys: The path to the key that contains the relevant id your HTTP URL needs for actions on an object.
    #    Example: If /apis/iam/v2/:id is the path for your API and your object returned from POST looks like
    #             {"team"=>{"id"=>"test-team"}}
    #             You should use `let(:id_keys) { ["team", "id"] }`
    #
    #  test_object: A hash of the object you wish to POST (will be convert to JSON).
    #
    shared_examples 'try_every_http_verb' do
      resp_id = nil

      it 'returns the correct response when GETing all objects' do
        if http_verbs.include?('GET_ALL')
          expect(
            automate_api_request(
              url,
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to be(expect_403_response)
        end
      end

      describe 'object specific verbs (POST, PUT, GET, DELETE)' do
        it 'returns the correct response' do
          if (http_verbs.include?('GET') || http_verbs.include?('PUT') || http_verbs.include?('POST')) &&
            !http_verbs.include?('POST')

            raise 'You passed one or more of GET, PUT, DELETE to http_verbs but not POST.\
                   You must pass POST if you wish to test GET, PUT, or DELETE.'
          end

          if http_verbs.include?('POST')
            resp = automate_api_request(
              url,
              http_method: 'POST',
              request_body: test_object.to_json,
              user: user,
            )

            expect(url + ":POST:" + resp.http_status.to_s == url + ":POST:403").to be(expect_403_response)

            if success_expected
              resp_id = JSON.parse(resp.raw_response_body).dig(*id_keys)
              if resp_id.nil?
                raise "Unexpected error: no resp_id for #{url}."
              end
            else
              resp_id = failure_test_id
            end
          end

          if http_verbs.include?('POST') && http_verbs.include?('GET')
            expect(
              url + ":GET:" + automate_api_request(
                "#{url}/#{resp_id}",
                http_method: 'GET',
                user: user,
              ).http_status.to_s == url + ":GET:403"
            ).to be(expect_403_response)
          end

          if http_verbs.include?('POST') && http_verbs.include?('PUT')
            expect(
              url + ":PUT:" + automate_api_request(
                "#{url}/#{resp_id}",
                http_method: 'PUT',
                user: user,
                request_body: test_object.to_json,
                request_headers: { 'Content-Type': 'application/json+lax' } # we're messy with the payloads here
              ).http_status.to_s == url + ":PUT:403"
            ).to be(expect_403_response)
          end

          if http_verbs.include?('POST') && http_verbs.include?('DELETE')
            expect(
              url + ":DELETE:" + automate_api_request(
                "#{url}/#{resp_id}",
                http_method: 'DELETE',
                user: user,
              ).http_status.to_s == url + ":DELETE:403"
            ).to be(expect_403_response)
          end
        end
      end
    end # try_every_http_verb

    shared_examples 'iam access control with v1 legacy policies' do
      describe '/apis/iam/v2/teams' do
        let(:url) { '/apis/iam/v2/teams' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["team", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            id: "inspec_test_team-#{TIMESTAMP}",
            name: 'This team was created by inspec tests. DELETE ME.',
            projects: []
          }
        end

        include_examples 'try_every_http_verb'

        it "POST /apis/iam/v2/teams/{id}/users:add returns the correct response code" do
          expect(
            automate_api_request(
              "/apis/iam/v2/teams/inspec_test_team-#{TIMESTAMP}/users:add",
              http_method: 'POST',
              user: user,
              request_headers: { "Content-type": "application/json" },
              request_body: { membership_ids: ['some_user', 'another_user'] }.to_json,
            ).http_status == 403

          ).to eq(expect_403_for_admin_only_apis)
        end

        it "POST /apis/iam/v2/teams/{id}/users:remove returns the correct response code" do
          expect(
            automate_api_request(
              "/apis/iam/v2/teams/inspec_test_team-#{TIMESTAMP}/users:remove",
              http_method: 'POST',
              user: user,
              request_headers: { "Content-type": "application/json" },
              request_body: { membership_ids: ['some_user', 'another_user'] }.to_json,
            ).http_status == 403

          ).to eq(expect_403_for_admin_only_apis)
        end
      end # /apis/iam/v2s/teams

      describe '/apis/iam/v2/users' do
        let(:url) { '/apis/iam/v2/users' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["user", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'doesnotexist@example.com' }
        let(:test_id) { "inspec_test_user-#{TIMESTAMP}" }
        let(:test_object) do
          {
            name: test_id,
            id: test_id,
            password: 'chefautomate',
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/apis/iam/v2/tokens' do
        let(:url) { '/apis/iam/v2/tokens' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["token", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            id: "inspec_test_token_2-#{TIMESTAMP}",
            name: 'This token was created by inspec tests. DELETE ME.',
            projects: []
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/apis/iam/v2/policies' do
        let(:url) { '/apis/iam/v2/policies' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["policy", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            id: "inspec_test_policy-#{TIMESTAMP}",
            name: 'This policy was created by inspec tests. DELETE ME.',
            members: ['user:local:inspec', 'team:local:inspec'],
            statements: [
              {
                effect: 'ALLOW',
                actions: ['do:some:thing'],
                projects: ['*']
              }
            ],
            projects: []
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/apis/iam/v2/roles' do
        let(:url) { '/apis/iam/v2/roles' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["role", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            id: "inspec_test_role-#{TIMESTAMP}",
            name: 'This role was created by inspec tests. DELETE ME.',
            actions: ['do:a:thing'],
            projects: []
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/apis/iam/v2/projects' do
        let(:url) { '/apis/iam/v2/projects' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["project", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            id: "inspec_test_project-#{TIMESTAMP}",
            name: 'This project was created by inspec tests. DELETE ME.',
            skip_policies: true
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/api/v0/notifications' do
        let(:url) { '/api/v0/notifications/rules' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["id"] }
        let(:success_expected) { false }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'does-not-exist' }
        let(:test_object) do
          {
            rule: {
              name: 'test!!!',
              event: 'CCRFailure',
              SlackAlert: {
                url: 'http://testing'
                }
              }
            }
        end

        include_examples 'try_every_http_verb'
      end

      describe 'version endpoints' do
        it "GET gateway/version returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/version",
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to eq(expect_403_for_all_apis) # always true
        end

        it "GET iam/v2/policy_version returns the correct response code" do
          expect(
            automate_api_request(
              "/apis/iam/v2/policy_version",
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to eq(expect_403_for_all_apis) # always true
        end

        it "GET cfgmgmt/version returns the correct response code" do
            expect(
              automate_api_request(
                "/api/v0/cfgmgmt/version",
                http_method: 'GET',
                user: user,
              ).http_status == 403
            ).to eq(expect_403_for_all_apis) # always true
        end

        it "GET deployment/service_versions returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/deployment/service_versions",
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to eq(expect_403_for_all_apis) # always true
        end

        it "GET reporting/version returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/compliance/reporting/version",
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to eq(expect_403_for_all_apis) # always true
        end
      end

      describe '/api/v0/cfgmgmt/' do
        before(:all) do
          node_request = automate_client_api_request(
            '/data-collector/v0',
            TEST_TOKEN, # users not allowed to post to this API
            http_method: 'POST',
            request_body: {}
          )
          # to save time on this test we don't post any data
          # this results int a bad request
          # but we only care that it doesn't get a 403
          expect(node_request.http_status).to eq 400

          policy_node_request = automate_client_api_request(
            '/data-collector/v0',
            TEST_TOKEN, # users not allowed to post to this API
            http_method: 'POST',
            request_body: {}
          )
          expect(policy_node_request.http_status).to eq 400
        end

        %w(
          nodes
          nodes/82760210-4686-497e-b039-efca78dee64b/runs
          nodes/82760210-4686-497e-b039-efca78dee64b/runs/639844f4-2ce6-42ba-8c9d-853db69adff3
          stats/node_counts
          stats/run_counts?node_id=82760210-4686-497e-b039-efca78dee64b
          organizations
          source_fqdns
          nodes/82760210-4686-497e-b039-efca78dee64b/attribute
          policy_revision/6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482
          suggestions?type=cookbook&text=b
        ).each do |url|
          it "#{url} returns the correct response code" do
              expect(
                automate_api_request(
                  "/api/v0/cfgmgmt/#{url}",
                  http_method: 'GET',
                  user: user
                ).http_status == 403
              ).to eq(expect_403_for_all_apis) # always true
          end
        end
      end

    # event-feed-related endpoints
    describe '/api/v0/' do
      %w(
        eventfeed
        event_type_counts
        eventstrings
      ).each do |url|
        it "#{url} returns the correct response code" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: 'GET',
                user: user,
              ).http_status == 403
            ).to eq(expect_403_for_all_apis) # always true
        end
      end
    end

    #secrets endpoints
    describe '/api/v0/secrets' do
      {
        'GET': %w(
          secrets/id/SECRETID
        ),
        'DELETE': %w(secrets/id/SECRETID),
        'POST': %w(
          secrets
          secrets/search
        ),
        'PATCH': %w(secrets/id/SECRETID)
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns the correct response code" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user,
                ).http_status == 403
              ).to eq(expect_403_for_all_apis)
          end
        end
      end
    end

      # nodemanagers endpoints
      describe '/api/v0/nodemanagers' do
        {
          'POST': %w(
            nodemanagers/search
            nodemanagers
            nodemanagers/id/SOMEID/search-fields
            nodemanagers/id/SOMEID/search-nodes
          ),
          'GET': %w(
            nodemanagers/id/SOMEID
          ),
          'PUT': %w(
            nodemanagers/id/SOMEID
          ),
          'DELETE': %w(
            nodemanagers/id/SOMEID
          )
        }.each do |method, urls|
          urls.each do |url|
            it "#{url} returns the correct response code" do
                expect(
                  automate_api_request(
                    "/api/v0/#{url}",
                    http_method: method,
                    user: user,
                  ).http_status == 403
                ).to eq(expect_403_for_all_apis)
            end
          end
        end
      end

      # nodes endpoints
      describe '/api/v0/nodes' do
        {
          'POST': %w(
            nodes
            nodes/search
          ),
          'GET': %w(
            nodes/id/SOMEID
          ),
          'PUT': %w(
            nodes/id/SOMEID
          ),
          'DELETE': %w(
            nodes/id/SOMEID
          )
        }.each do |method, urls|
          urls.each do |url|
            it "#{url} returns the correct response code" do
                expect(
                  automate_api_request(
                    "/api/v0/#{url}",
                    http_method: method,
                    user: user,
                  ).http_status == 403
                ).to be(expect_403_for_all_apis)
            end
          end
        end
      end

      # compliance
      describe '/api/v0/compliance' do
        {
          'GET': %w(
            profiles/read/OWNER-FOO/NAME-FOO/version/VERSION-FOO
            profiles/OWNER-FOO/NAME-FOO/tar
            profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO/tar
            market/read/NAME-FOO/version/VERSION-FOO
            reporting/nodes/id/SOMENODEID
            scanner/jobs/id/JOBID
            scanner/jobs/rerun/id/JOBID
          ),
          'DELETE': %w(
            profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO
            scanner/jobs/id/JOBID
          ),
          'POST': %w(
            profiles/search
            reporting/reports
            reporting/reports/id/SOMEID
            reporting/suggestions
            reporting/profiles
            reporting/nodes/search
            scanner/jobs
            scanner/jobs/search
            reporting/stats/summary
            reporting/stats/trend
            reporting/stats/profiles
            reporting/stats/failures
            secrets
            secrets/search
          ),
          'PUT': %w(
            scanner/jobs/id/JOBID
          ),
        }.each do |method, urls|
          urls.each do |url|
            it "#{method} #{url} returns the correct response code" do
                expect(
                  automate_api_request(
                    "/api/v0/compliance/#{url}",
                    http_method: method,
                    user: user,
                  ).http_status == 403
                 # when adding requests, take good care that the URL is correct
                 # -- if it's not, the accepted 404 here can be deceiving
                ).to be(expect_403_for_all_apis)
            end
          end
        end
      end

      # Note: these handlers are not at all picky about the HTTP method used
      # (anything goes). For the tests, I've picked POST where a request body
      # is involved.
      describe 'compliance REST handlers' do
        it "GET reporting/export profiles returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/compliance/reporting/export",
              http_method: 'GET',
              user: user,
            ).http_status

          ).to eq(400)
        end

        it "POST profiles returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/compliance/profiles?owner=OWNER",
              http_method: 'POST',
              user: user,
              request_headers: { "Content-type": "application/json" },
              request_body: { name: 'NAME', version: 'VER' }.to_json,
            ).http_status

          ).to eq(400)
        end

        it "POST profiles/tar returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/compliance/profiles/tar",
              http_method: 'POST',
              user: user,
              request_headers: { "Content-type": "application/json" },
              request_body: { owner: 'OWNER', name: 'NAME', version: 'VER' }.to_json,
            ).http_status

          ).to eq(404)
        end
      end

      describe 'telemetry REST handlers' do
        it "GET telemetry/config returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/telemetry/config",
              http_method: 'GET',
              user: user,
            ).http_status

          ).to eq(200)
        end
      end

      describe 'license REST handlers' do
        it "POST license returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/license/apply",
              http_method: 'POST',
              user: user,
            ).http_status == 403

          ).to eq(expect_403_for_admin_only_apis)
        end

        it "GET license/status returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/license/status",
              http_method: 'GET',
              user: user,
            ).http_status == 403

          ).to eq(expect_403_for_all_apis)
        end

        it "POST license/request returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/license/request",
              http_method: 'POST',
              user: user,
            ).http_status == 403

          ).to eq(expect_403_for_admin_only_apis)
        end
      end

      # default policy allows client access
      # TODO how to distinguish between a 'read' and 'upload' if http calls
      # aren't important?
      describe 'compliance REST handlers for client calls' do
        it "POST profiles returns the correct response code" do
          expect(
            automate_client_api_request(
              "/api/v0/compliance/profiles?owner=OWNER",
              TEST_TOKEN,
              http_method: 'POST',
              request_headers: { "Content-type": "application/json" },
              request_body: { name: 'NAME', version: 'VER' }.to_json,
            ).http_status.to_s

          ).to match(/400/)
        end
      end

      describe '/api/v0/compliance/profiles/search as client' do
        it "api/v0/compliance/profiles/search returns the correct response code for client" do
          expect(
            automate_client_api_request(
              "/api/v0/compliance/profiles/search",
              TEST_TOKEN,
              http_method: 'POST'
            ).http_status
          ).not_to eq(403)
        end
      end

      describe 'legacy data-collector endpoints' do
        %w(
          /api/v0/events/data-collector
          /data-collector/v0
        ).each do |url|
          { 'GET': 200, 'POST': 400 }.each do |method, status|
            it "#{method} #{url} returns the correct response code for client" do
              expect(
                automate_client_api_request(
                  url,
                  TEST_TOKEN,
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
          it "#{url} returns the correct response code for client" do
            expect(
              automate_client_api_request(
                "/api/v0/ingest/events/chef/#{url}",
                TEST_TOKEN,
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
                TEST_TOKEN,
                http_method: 'POST',
                request_body: { node_ids: [ 'fake-2d83-47ce-8e46-84d1e85be6c7' ] }.to_json,
              ).http_status
            ).not_to eq(403)
        end
      end

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
                ).http_status == 403
              ).to eq(expect_403_for_admin_only_apis)
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
                ).http_status == 403
              ).to eq(expect_403_for_admin_only_apis)
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
                ).http_status == 403
              ).to eq(expect_403_for_admin_only_apis)
            end
          end
        end
      end

      # {job,node-missing,delete-node}-scheduler
      describe '/api/v0/retention/nodes/' do
        {
          'POST': %w(
            delete-nodes/config
            missing-nodes/config
            missing-nodes-deletion/config
          ),
          'GET': %w(
            status
          ),
        }.each do |method, urls|
          urls.each do |url|
            it "#{url} returns the correct response code" do
                expect(
                  automate_api_request(
                    "/api/v0/retention/nodes/#{url}",
                    http_method: method,
                    user: user,
                  ).http_status == 403
                ).to be(expect_403_for_admin_only_apis)
            end
          end
        end
      end
    end

    describe 'when making requests as an admin' do
      let(:expect_403_for_admin_only_apis) { false }
      let(:expect_403_for_all_apis) { false }
      let(:expect_403_for_client_only_apis) { false }
      let(:user) { ADMIN_USER_ID }
      let(:success_expected) { true }

      include_examples 'iam access control with v1 legacy policies'
    end

    describe 'when making requests as a non-admin' do
      let(:expect_403_for_admin_only_apis) { true }
      let(:expect_403_for_all_apis) { false }
      let(:expect_403_for_client_only_apis) { true }
      let(:user) { NON_ADMIN_USERNAME }
      let(:success_expected) { false }

      include_examples 'iam access control with v1 legacy policies'
    end
  end
end
