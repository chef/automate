# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

require_relative '../../constants'

title 'iam chef-managed policy access integration tests'

control 'iam-chef-managed-access-1' do
  title 'iam chef-managed policy access'
  desc 'role-based access for chef-managed policies on a fresh system with no legacy policies'

  VIEWER_USER_ID = "inspec-viewer-#{TIMESTAMP}"
  EDITOR_USER_ID = "inspec-editor-#{TIMESTAMP}"
  PROJECT_OWNER_USER_ID = "inspec-project-owner-#{TIMESTAMP}"

  ALL_ROLES = [ADMIN_USER_ID, VIEWER_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID]

  PROJECT_ID = "inspec-project-#{TIMESTAMP}"

  # chef-managed IDs saved in inspec/constants.rb
  ADMIN_POLICY_ID = DEFAULT_POLICY_IDS[0]
  EDITOR_POLICY_ID = DEFAULT_POLICY_IDS[1]
  VIEWER_POLICY_ID = DEFAULT_POLICY_IDS[2]
  PROJECT_OWNER_POLICY_ID = "#{PROJECT_ID}-project-owners"

  describe 'chef-managed policy access' do
    before(:all) do
      create_viewer_resp = automate_api_request(
        '/apis/iam/v2/users',
        http_method: 'POST',
        request_body: {
          id: VIEWER_USER_ID,
          name: VIEWER_USER_ID,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate'
        }.to_json
      )
      expect(create_viewer_resp.http_status).to eq 200

      create_editor_resp = automate_api_request(
        '/apis/iam/v2/users',
        http_method: 'POST',
        request_body: {
          id: EDITOR_USER_ID,
          name: EDITOR_USER_ID,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate'
        }.to_json
      )
      expect(create_editor_resp.http_status).to eq 200

      create_project_owner_resp = automate_api_request(
        '/apis/iam/v2/users',
        http_method: 'POST',
        request_body: {
          id: PROJECT_OWNER_USER_ID,
          name: PROJECT_OWNER_USER_ID,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate'
        }.to_json
      )
      expect(create_project_owner_resp.http_status).to eq 200

      create_project_resp = automate_api_request(
        '/apis/iam/v2/projects',
        http_method: 'POST',
        request_body: {
          id: PROJECT_ID,
          name: PROJECT_ID
        }.to_json
      )
      expect(create_project_resp.http_status).to eq 200

      add_viewer_member_resp = automate_api_request("/apis/iam/v2/policies/#{VIEWER_POLICY_ID}/members:add",
      http_method: 'POST',
      request_body: {
        members: ["user:local:#{VIEWER_USER_ID}"]
      }.to_json)
      expect(add_viewer_member_resp.http_status).to eq 200

      add_editor_member_resp = automate_api_request("/apis/iam/v2/policies/#{EDITOR_POLICY_ID}/members:add",
      http_method: 'POST',
      request_body: {
        members: ["user:local:#{EDITOR_USER_ID}"]
      }.to_json)
      expect(add_editor_member_resp.http_status).to eq 200

      add_project_owner_member_resp = automate_api_request("/apis/iam/v2/policies/#{PROJECT_OWNER_POLICY_ID}/members:add",
      http_method: 'POST',
      request_body: {
        members: ["user:local:#{PROJECT_OWNER_USER_ID}"]
      }.to_json)
      expect(add_project_owner_member_resp.http_status).to eq 200
    end

    after(:all) do
      delete_user_resp = automate_api_request(
        "/apis/iam/v2/users/#{VIEWER_USER_ID}", http_method: 'DELETE'
      )
      expect(delete_user_resp.http_status).to eq 200

      delete_user_resp = automate_api_request(
        "/apis/iam/v2/users/#{EDITOR_USER_ID}", http_method: 'DELETE'
      )
      expect(delete_user_resp.http_status).to eq 200

      delete_user_resp = automate_api_request(
        "/apis/iam/v2/users/#{PROJECT_OWNER_USER_ID}", http_method: 'DELETE'
      )
      expect(delete_user_resp.http_status).to eq 200

      delete_project_resp = automate_api_request(
        "/apis/iam/v2/projects/#{PROJECT_ID}", http_method: 'DELETE'
      )
      expect(delete_project_resp.http_status).to eq 200
    end

    describe 'reading compliance data' do
      {
        'GET': [
          'profiles/read/OWNER-FOO/NAME-FOO/version/VERSION-FOO', # redirect GetProfile
          'market/read/NAME-FOO/version/VERSION-FOO',
          'reporting/nodes/id/SOMENODEID',
          'scanner/jobs/id/JOBID',
          'profiles/OWNER-FOO/NAME-FOO/tar', # legacy GetProfile
          'profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO/tar' # legacy GetProfile
        ],
        'POST': [
          'profiles/search', # redirect Search
          'profiles/tar', # GetProfile
          'reporting/reports', # ListReports
          'reporting/reports/id/SOMEID', # ReadReport
          'reporting/suggestions', # ListSuggestions
          'reporting/profiles', # ListProfiles
          'reporting/nodes/search', # ListNodes
          'reporting/stats/summary', # ReadSummary
          'reporting/stats/trend', # ReadTrend
          'reporting/stats/profiles', # ReadProfiles
          'reporting/stats/failures', # ReadFailures
          'scanner/jobs/search' # List
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/compliance/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'modifying compliance data' do
      {
        'GET': [
          'scanner/jobs/rerun/id/JOBID' # this is a rerun, it's not "looking at stuff" only
        ],
        'DELETE': %w[
          profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO
          scanner/jobs/id/JOBID
        ],
        'PUT': %w[
          scanner/jobs/id/JOBID
        ],
        'POST': [
          'scanner/jobs', # Create job
          'reporting/export' # ExportReport as json/csv
        ]
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer" do
            expect(
              automate_api_request(
                "/api/v0/compliance/#{url}",
                request_headers: { 'Content-type': 'application/json' },
                http_method: method,
                user: VIEWER_USER_ID
              ).http_status
            ).to eq 403
          end

          [ADMIN_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/compliance/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'handcrafted profile upload handler' do
      it 'POST profiles?owner=OWNER returns 403 for viewer' do
        expect(
          automate_api_request(
            '/api/v0/compliance/profiles?owner=OWNER}',
            request_headers: { 'Content-type': 'application/json' },
            request_body: { name: 'NAME', version: 'VER' }.to_json,
            http_method: 'POST',
            user: VIEWER_USER_ID
          ).http_status
        ).to eq 403
      end

      [ADMIN_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
        it "POST profiles?owner=OWNER does not return 403 for #{user}" do
          expect(
            automate_api_request(
              '/api/v0/compliance/profiles?owner=OWNER',
              request_headers: { 'Content-type': 'application/json' },
              request_body: { name: 'NAME', version: 'VER' }.to_json,
              http_method: 'POST',
              user: user
            ).http_status
          ).not_to eq 403
        end
      end
    end

    describe 'reading node manager data' do
      {
        'GET': %w[
          nodes/id/NODEID
          nodes/search
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              status = automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: user
              ).http_status
              expect(status).not_to eq 403
            end
          end
        end
      end
    end

    describe 'writing node manager data' do
      {
        'GET': %w[
          nodes/rerun/id/NODEID
        ],
        'PUT': %w[
          nodes/id/NODEID
        ],
        'DELETE': %w[
          nodes/id/NODEID
        ],
        'POST': %w[
          nodes
          nodes/delete
          nodes/bulk-create
        ]
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: VIEWER_USER_ID
              ).http_status
            ).to eq 403
          end

          [ADMIN_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'reading infra data' do
      {
        'GET': %w[
          nodes
          nodes/NODEID/runs
          stats/node_counts
          stats/run_counts
          nodes/NODEID/runs/RUNID
          suggestions
          organizations
          source_fqdns
          policy_revision/REVID
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              status = automate_api_request(
                "/api/v0/cfgmgmt/#{url}",
                http_method: method,
                user: user
              ).http_status
              expect(status).not_to eq 403
            end
          end
        end
      end
    end

    # note: we don't test data ingestion here, that's done in gateway_integration,
    # and we have a v2-only variant of those tests.

    describe 'reading event data' do
      {
        'GET': %w[
          eventfeed
          event_type_counts
          event_task_counts
          eventstrings
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'version, health, and status endpoints' do
      {
        'GET': %w[
          cfgmgmt/version
          compliance/reporting/version
          deployment/service_versions
          license/status
          gateway/version
          gateway/health
          ingest/version
          notifications/version
          telemetry/config
          version
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              status = automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: user
              ).http_status
              expect(status).not_to eq 403
            end
          end
        end
      end
    end

    describe 'reading introspect data' do
      {
        'GET': %w[
          introspect
        ],
        'POST': %w[
          introspect
          introspect_some
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'modifying secrets' do
      {

        'POST': %w[
          secrets
        ],
        'PATCH': %w[
          secrets/id/foo
        ],
        'DELETE': %w[
          secrets/id/foo
        ]
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: VIEWER_USER_ID
              ).http_status
            ).to eq 403
          end

          [ADMIN_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'reading secrets' do
      {
        'GET': %w[
          secrets/id/foo
        ],
        'POST': %w[
          secrets/search
        ]
      }.each do |method, urls|
        urls.each do |url|
          ALL_ROLES.each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'applying and requesting license' do
      {
        'POST': %w[
          apply
          request
        ]
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer" do
            expect(
              automate_api_request(
                "/api/v0/license/#{url}",
                http_method: method,
                user: VIEWER_USER_ID
              ).http_status
            ).to eq 403
          end

          [ADMIN_USER_ID, EDITOR_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/license/#{url}",
                  http_method: method,
                  user: user
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe "reading IAM data" do
      {
        'GET': %w(
          /apis/iam/v2/users
          /apis/iam/v2/teams
          /apis/iam/v2/users/username
          /apis/iam/v2/teams/some-team-id
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          [ ADMIN_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end

      {
        'GET': %w(
          /apis/iam/v2/tokens
          /apis/iam/v2/tokens/some-token-id
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          it "#{method} #{url} does not return 403 for admin" do
            expect(
              automate_api_request(
                url,
                http_method: method,
                user: ADMIN_USER_ID
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe "modifying IAM data" do
      {
        'DELETE': %w(
          /apis/iam/v2/users/username
          /apis/iam/v2/teams/some-team-id
          /apis/iam/v2/tokens/some-token-id
        ),
        'PUT': %w(
          /apis/iam/v2/users/username
          /apis/iam/v2/teams/some-team-id
          /apis/iam/v2/tokens/some-token-id
        ),
        'POST': %w(
          /apis/iam/v2/users
          /apis/iam/v2/teams
          /apis/iam/v2/tokens
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          it "#{method} #{url} does not return 403 for admin" do
            expect(
              automate_api_request(
                url,
                http_method: method,
                user: ADMIN_USER_ID
              ).http_status
            ).not_to eq 403
          end
        end
      end

      {
        'GET': %w(
          /apis/iam/v2/teams/some-team-id/users
        ),
        'POST': %w(
          /apis/iam/v2/teams/some-team-id/users:add
          /apis/iam/v2/teams/some-team-id/users:remove
        ),
      }.each do |method, urls|
        urls.each do |url|
          [EDITOR_USER_ID, VIEWER_USER_ID].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          [ADMIN_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  url,
                  http_method: method,
                  user: user,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end

    ALL_ROLES.each do |user|
      it "a #{user} can see their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2/users/#{user}",
            http_method: "GET",
            user: user,
          ).http_status
        ).not_to eq 403
      end

      it "a #{user} can update their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2/users/#{user}",
            http_method: "PUT",
            user: user,
          ).http_status
        ).not_to eq 403
      end

      it "a #{user} cannot delete their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2/users/#{user}",
            http_method: "DELETE",
            user: user,
          ).http_status
        ).to eq 403
      end
    end

    {
      'GET': %w(
        projects
        projects/some-project-id
      )
    }.each do |method, urls|
      urls.each do |url|
        ALL_ROLES.each do |user|
          it "#{method} #{url} does not return 403 for #{user}" do
            expect(
              automate_api_request(
                "/apis/iam/v2/#{url}",
                http_method: method,
                user: user,
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

      {
        'GET': %w(
          roles
          roles/some-role-id
        ),
        'DELETE': %w(
          policies/some-policy-id
          roles/some-role-id
          projects/some-project-id
        ),
        'PUT': %w(
          policies/some-policy-id
          roles/some-role-id
          projects/some-project-id
        ),
        'POST': %w(
          policies
          roles
          projects
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID, PROJECT_OWNER_USER_ID].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  "/apis/iam/v2/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          it "#{method} #{url} does not return 403 for admin" do
            expect(
              automate_api_request(
                url,
                http_method: method,
                user: ADMIN_USER_ID
              ).http_status
            ).not_to eq 403
          end
        end
      end

      {
        'GET': %w(
          policies
          policies/some-policy-id
          policies/some-policy-id/members
        ),
        'PUT': %w(
          policies/some-policy-id/members
        ),
        'POST': %w(
          policies/some-policy-id/members:add
          policies/some-policy-id/members:remove
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  "/apis/iam/v2/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          [ ADMIN_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} does not return 403 for #{user}" do
              expect(
                automate_api_request(
                  "/apis/iam/v2/#{url}",
                  http_method: method,
                  user: PROJECT_OWNER_USER_ID,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe "reading and modifying scheduler settings" do
      {
        'GET': %w(
            status
        ),
        'POST': %w(
            delete-nodes/config
            missing-nodes/config
            missing-nodes-deletion/config
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/retention/nodes/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          it "#{method} #{url} does not return 403 for admin" do
            expect(
              automate_api_request(
                url,
                http_method: method,
                user: ADMIN_USER_ID
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe "reading and modifying notifications" do
      {
        'GET': %w(
          rules
          rules/RULE-ID
        ),
        'DELETE': %w(
          rules/RULE-ID
        ),
        'POST': %w(
          rules
          webhook
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USER_ID, VIEWER_USER_ID, PROJECT_OWNER_USER_ID ].each do |user|
            it "#{method} #{url} returns 403 for #{user}" do
              expect(
                automate_api_request(
                  "/api/v0/notifications/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end

          it "#{method} #{url} does not return 403 for admin" do
            expect(
              automate_api_request(
                url,
                http_method: method,
                user: ADMIN_USER_ID
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end
  end
end
