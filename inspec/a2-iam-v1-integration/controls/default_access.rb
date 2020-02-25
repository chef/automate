# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

# This test suite is meant to verify whether calls return unauthorized (403) or not for
# 'normal' users and admin users. Please don't test for API behavior beyond that; only for
# whether the invoker is authorized or not to make the call.
#
# Also, please note that if we don't set up the routing for an API correctly, that will result
# in a 404, which this approaches considers a success (its not a 403!), so if we want these
# tests to give improve our confidence that we are permissioning our APIs correctly, we need
# to make sure the routing is correct.

require_relative '../../constants'

title 'default authorization access control integration tests'

NON_ADMIN_USERNAME = 'inspec_test_non_admin'

control 'authz-access-control-1' do
  title 'AuthZ access control'
  desc 'Verify access granted by default policies for both admin and non-admin users'

  describe 'AuthZ access control' do
    before(:all) do
      create_non_admin_request = automate_api_request(
        '/api/v0/auth/users',
        http_method: 'POST',
        request_body: {
          'name': NON_ADMIN_USERNAME,
          'username': NON_ADMIN_USERNAME,
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
      TEST_TOKEN = test_token_request.parsed_response_body[:value]
      TEST_TOKEN_ID = test_token_request.parsed_response_body[:id]

      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_non_admin_request = automate_api_request(
        "/api/v0/auth/users/#{NON_ADMIN_USERNAME}",
        http_method: 'DELETE',
      )

      expect(delete_non_admin_request.http_status.to_s).to match(/200|404/)


      delete_token_request = automate_api_request(
        "/api/v0/auth/tokens/#{TEST_TOKEN_ID}",
        http_method: 'DELETE',
      )

      expect(delete_token_request.http_status.to_s).to match(/200|404/)
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
    #    Example: `let(:url) { '/api/v0/auth/users' }`
    #
    #  id_keys: The path to the key that contains the relevant id your HTTP URL needs for actions on an object.
    #    Example: If /api/v0/auth/:id is the path for your API and your object returned from POST looks like
    #             {"team"=>{"id"=>"85419132-8a84-4d5a-9a5b-41c8e570ce93"}}
    #             You should use `let(:id_keys) { ["team", "id"] }`
    #
    #  test_object: A hash of the object you wish to POST (will be convert to JSON).
    #
    #  test_update_object: A hash of the object you wish to PUT (will be convert to JSON).
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
                request_body: test_update_object.to_json,
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

    shared_examples 'authz access control' do
      describe '/api/v0/auth/teams' do
        let(:url) { '/api/v0/auth/teams' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["team", "id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        # Random valid ID to use in 403 case.
        let(:failure_test_id) { '4ae7307e-0ac2-4871-bda9-ebf6bf28d6a5' }
        let(:test_object) do
          {
            'name': "inspec_test_team-#{Time.now.to_i}",
            'description': 'This team was created by inspec tests. DELETE ME.',
          }
        end
        let(:test_update_object) do
          {
            'name': "inspec_test_team-#{Time.now.to_i}",
            'description': 'This team was created by inspec tests but was modified. DELETE ME.',
          }
        end

        include_examples 'try_every_http_verb'
        # TODO (tc) Test AddUsers RemoveUsers once they are their own HTTP API endpoints.
      end # /api/v0/auths/teams

      describe '/api/v0/auth/users' do
        let(:url) { '/api/v0/auth/users' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["username"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { 'some_email@example.com' }
        let(:test_name) { "inspec_test_user-#{Time.now.to_i}" }
        let(:test_object) do
          {
            'name': test_name,
            'username': test_name,
            'password': 'chefautomate',
          }
        end
        let(:test_update_object) do
          {
            'name': test_name,
            'username': test_name,
            'id': 'some_nonsense',
          }
        end

        include_examples 'try_every_http_verb'
      end

      describe '/api/v0/auth/tokens' do
        let(:url) { '/api/v0/auth/tokens' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["id"] }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { '4ae7307e-0ac2-4871-bda9-ebf6bf28d6a5' }
        let(:test_object) do
          {
            'description': 'This token was created by inspec tests. DELETE ME.',
            'active': false,
          }
        end
        let(:test_update_object) { test_object }

        include_examples 'try_every_http_verb'
      end

      describe '/api/v0/notifications' do
        let(:url) { '/api/v0/notifications/rules' }
        let(:expect_403_response) { expect_403_for_admin_only_apis }
        let(:id_keys) { ["id"] }
        let(:success_expected) { false }
        let(:http_verbs) { ["GET_ALL", "POST", "GET", "PUT", "DELETE"] }
        let(:failure_test_id) { '4ae7307e-0ac2-4871-bda9-ebf6bf28d6a5' }
        let(:test_object) do
          { "rule": {
              "name": "test!!!",
              "event": "CCRFailure",
              "SlackAlert": {
                "url": "http://testing"
                }
              }
            }
        end
        let(:test_update_object) { test_object }

        include_examples 'try_every_http_verb'
      end

      it "gateway/version returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/version",
              http_method: 'GET',
              user: user,
            ).http_status == 403
          ).to eq(expect_403_for_all_apis)
      end

      it "gateway/policy_version returns the correct response code" do
          policy_version_request = automate_api_request(
            "/apis/iam/v2/policy_version", # V2 endpoint also used for v1
            http_method: 'GET',
            user: user,
          )
          expect(policy_version_request.http_status == 403).to eq(expect_403_for_all_apis)
      end

      describe '/api/v0/cfgmgmt/' do
        before(:all) do
          node = "chefdk-debian-7-tester-2d206b_run_converge"
          node_request = automate_api_request(
            '/data-collector/v0',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/#{node}.json")
          )

          expect(node_request.http_status).to eq 200

          policy_node = "policy-node"
          policy_node_request = automate_api_request(
            '/data-collector/v0',
            http_method: 'POST',
            request_body: inspec.profile.file("fixtures/converge/#{policy_node}.json")
          )

          expect(policy_node_request.http_status).to eq 200

          # Wait for data to be indexed
          command('sleep 15') do
            its('exit_status') { should eq 0 }
          end
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
          version
          policy_revision/6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482
          suggestions?type=cookbook&text=b
        ).each do |url|
          it "#{url} returns the correct response code" do
              expect(
                automate_api_request(
                  "/api/v0/cfgmgmt/#{url}",
                  http_method: 'GET',
                  user: user,
                ).http_status.to_s

                # in travis, test nodes aren't always created in time, so might get authorized 404 response
              ).to match(/200|404/)
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
                ).http_status.to_s

                # eventstrings gives us a 400, but that's ok, it means we've passed authz
              ).to match(/200|400/)
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
            reporting/version
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

      describe '/api/v0/deployment' do
        it "GET deployment/service_versions returns the correct response code" do
            expect(
              automate_api_request(
                "/api/v0/deployment/service_versions",
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

          ).to match(expect_403_for_admin_only_apis)
        end

        it "GET license/status returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/license/status",
              http_method: 'GET',
              user: user,
            ).http_status == 403

          ).to match(expect_403_for_all_apis)
        end

        it "POST license/request returns the correct response code" do
          expect(
            automate_api_request(
              "/api/v0/license/request",
              http_method: 'POST',
              user: user,
            ).http_status == 403

          ).to match(expect_403_for_admin_only_apis)
        end
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

    describe 'when making requests as an admin' do
      let(:expect_403_for_admin_only_apis) { false }
      let(:expect_403_for_all_apis) { false }
      let(:expect_403_for_client_only_endpoints) { false }
      let(:user) { ADMIN_USER_ID }
      let(:success_expected) { true }

      include_examples 'authz access control'
    end

    describe 'when making requests as a non-admin' do
      let(:expect_403_for_admin_only_apis) { true }
      let(:expect_403_for_all_apis) { false }
      let(:expect_403_for_client_only_endpoints) { true }
      let(:user) { NON_ADMIN_USERNAME }
      let(:success_expected) { false }

      include_examples 'authz access control'
    end
  end
end

control 'authz-access-control-iam-v1' do
  title 'access control checks specific to the v1 default policies'

  describe 'AuthZ access control' do
    before(:all) do
      create_non_admin_request = automate_api_request(
        '/api/v0/auth/users',
        http_method: 'POST',
        request_body: {
          'name': NON_ADMIN_USERNAME,
          'username': NON_ADMIN_USERNAME,
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
      TEST_TOKEN_V1 = test_token_request.parsed_response_body[:value]
      TEST_TOKEN_ID_V1= test_token_request.parsed_response_body[:id]

      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_non_admin_request = automate_api_request(
        "/api/v0/auth/users/#{NON_ADMIN_USERNAME}",
        http_method: 'DELETE',
      )
      expect(delete_non_admin_request.http_status.to_s).to match(/200|404/)

      delete_token_request = automate_api_request(
        "/api/v0/auth/tokens/#{TEST_TOKEN_ID_V1}",
        http_method: 'DELETE',
      )
      expect(delete_token_request.http_status.to_s).to match(/200|404/)
    end

    # compliance:profiles for client calls should be permitted by default
    describe '/api/v0/compliance/profiles/search as client' do
      it "api/v0/compliance/profiles/search returns the correct response code for client" do
        expect(
          automate_client_api_request(
            "/api/v0/compliance/profiles/search",
            TEST_TOKEN_V1,
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
          it "#{method} #{url} returns the correct response code for client" do
            expect(
              automate_client_api_request(
                url,
                TEST_TOKEN_V1,
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
              TEST_TOKEN_V1,
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

    shared_examples 'authz access control (iam v1)' do
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
              ).to be(expect_403_for_client_only_endpoints)
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
            ).to be(expect_403_for_admin_only_apis)
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
                ).to be(expect_403_for_client_only_endpoints)
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
      let(:expect_403_for_client_only_endpoints) { false }
      let(:user) { ADMIN_USER_ID }
      let(:success_expected) { true }

      include_examples 'authz access control (iam v1)'
    end

    describe 'when making requests as a non-admin' do
      let(:expect_403_for_admin_only_apis) { true }
      let(:expect_403_for_all_apis) { false }
      let(:expect_403_for_client_only_endpoints) { true }
      let(:user) { NON_ADMIN_USERNAME }
      let(:success_expected) { false }

      include_examples 'authz access control (iam v1)'
    end
  end
end
