# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2 role access API integration tests'

control 'iam-v2-roles-1' do
  title 'v2-only access'
  desc 'role-based access for editor and viewer when v1 policies are purged'

  VIEWER_USERNAME = 'inspec_test_viewer'
  EDITOR_USERNAME = 'inspec_test_editor'

  describe 'migrated legacy v1 policies' do
    it 'legacy policies can be deleted' do
      resp = automate_api_request('/apis/iam/v2beta/policies')
      expect(resp.http_status).to eq 200

      all_policies = resp.parsed_response_body[:policies]
      legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }
      legacy_policies.map { |p| p[:id] }.each do |id|
        resp = automate_api_request("/apis/iam/v2beta/policies/#{id}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
      end
    end
  end

  describe 'viewer and editor access' do
    before(:all) do
      create_editor_request = automate_api_request(
        '/apis/iam/v2beta/users',
        http_method: 'POST',
        request_body: {
          id: EDITOR_USERNAME,
          name: EDITOR_USERNAME,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_editor_request.http_status.to_s).to match(/200|409/)

      create_viewer_request = automate_api_request(
        '/apis/iam/v2beta/users',
        http_method: 'POST',
        request_body: {
          id: VIEWER_USERNAME,
          name: VIEWER_USERNAME,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_viewer_request.http_status.to_s).to match(/200|409/)

      add_editor_request = automate_api_request(
        '/apis/iam/v2beta/policies/editor-access/members:add',
        http_method: 'POST',
        request_body: {
          members: [ "user:local:#{EDITOR_USERNAME}" ]
        }.to_json
      )
      expect(add_editor_request.http_status.to_s).to match(/200|409/)

      add_viewer_request = automate_api_request(
        '/apis/iam/v2beta/policies/viewer-access/members:add',
        http_method: 'POST',
        request_body: {
          members: [ "user:local:#{VIEWER_USERNAME}" ]
        }.to_json
      )
      expect(add_viewer_request.http_status.to_s).to match(/200|409/)
    end

    after(:all) do
      delete_editor_request = automate_api_request(
        "/apis/iam/v2beta/users/#{EDITOR_USERNAME}",
        http_method: 'DELETE',
      )
      expect(delete_editor_request.http_status.to_s).to match(/200|404/)

      delete_viewer_request = automate_api_request(
        "/apis/iam/v2beta/users/#{VIEWER_USERNAME}",
        http_method: 'DELETE',
      )
      expect(delete_viewer_request.http_status.to_s).to match(/200|404/)
    end

    describe "reading compliance data" do
      {
        'GET': [
          'profiles/read/OWNER-FOO/NAME-FOO/version/VERSION-FOO', # redirect GetProfile
          'market/read/NAME-FOO/version/VERSION-FOO',
          'reporting/nodes/id/SOMENODEID',
          'scanner/jobs/id/JOBID',
          'profiles/OWNER-FOO/NAME-FOO/tar', # legacy GetProfile
          'profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO/tar', # legacy GetProfile
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
          'scanner/jobs/search', # List
        ],
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} does not return 403 for viewer user" do
            expect(
              automate_api_request(
                "/api/v0/compliance/#{url}",
                http_method: method,
                user: VIEWER_USERNAME,
              ).http_status
            ).not_to eq 403
          end

          it "#{method} #{url} does not return 403 for editor user" do
            expect(
              automate_api_request(
                "/api/v0/compliance/#{url}",
                http_method: method,
                user: EDITOR_USERNAME,
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe "modifying compliance data" do
      {
        'GET': [
          'scanner/jobs/rerun/id/JOBID', # this is a rerun, it's not "looking at stuff" only
        ],
        'DELETE': %w(
          profiles/OWNER-FOO/NAME-FOO/version/VERSION-FOO
          scanner/jobs/id/JOBID
        ),
        'PUT': %w(
          scanner/jobs/id/JOBID
        ),
        'POST': [
          'scanner/jobs', # Create job
          'reporting/export', # ExportReport as json/csv
        ],
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer user" do
            expect(
              automate_api_request(
                "/api/v0/compliance/#{url}",
                request_headers: { 'Content-type': 'application/json' },
                http_method: method,
                user: VIEWER_USERNAME,
              ).http_status
            ).to eq 403
          end

          it "#{method} #{url} does not return 403 for editor user" do
            expect(
              automate_api_request(
                "/api/v0/compliance/#{url}",
                http_method: method,
                user: EDITOR_USERNAME,
              ).http_status
            ).not_to eq 403
          end
        end
      end

      describe 'handcrafted profile upload handler' do
        it 'POST profiles?owner=OWNER returns 403 for viewer user' do
          expect(
            automate_api_request(
              '/api/v0/compliance/profiles?owner=OWNER}',
              request_headers: { 'Content-type': 'application/json' },
              request_body: { name: 'NAME', version: 'VER' }.to_json,
              http_method: 'POST',
              user: VIEWER_USERNAME,
            ).http_status
          ).to eq 403
        end

        it 'POST profiles?owner=OWNER does not return 403 for editor user' do
          expect(
            automate_api_request(
              '/api/v0/compliance/profiles?owner=OWNER',
              request_headers: { 'Content-type': 'application/json' },
              request_body: { name: 'NAME', version: 'VER' }.to_json,
              http_method: 'POST',
              user: EDITOR_USERNAME,
            ).http_status
          ).not_to eq 403
        end
      end

    end

    describe 'reading node manager data' do
      {
        'GET': %w(
          nodes/id/NODEID
          nodes/search
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              status =  automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: user,
              ).http_status
              expect(status).not_to eq 403
            end
          end
        end
      end
    end

    describe 'writing node manager data' do
      {
        'GET': %w(
          nodes/rerun/id/NODEID
        ),
        'PUT': %w(
          nodes/id/NODEID
        ),
        'DELETE': %w(
          nodes/id/NODEID
        ),
        'POST': %w(
          nodes
          nodes/delete
          nodes/bulk-create
        ),
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer user" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: VIEWER_USERNAME,
              ).http_status
            ).to eq 403
          end

          it "#{method} #{url} does not return 403 for editor user" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: EDITOR_USERNAME,
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe 'reading infra data' do
      {
        'GET': %w(
          nodes
          nodes/NODEID/runs
          stats/node_counts
          stats/run_counts
          nodes/NODEID/runs/RUNID
          suggestions
          organizations
          source_fqdns
          policy_revision/REVID
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              status =  automate_api_request(
                "/api/v0/cfgmgmt/#{url}",
                http_method: method,
                user: user,
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
        'GET': %w(
          eventfeed
          event_type_counts
          event_task_counts
          eventstrings
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'version, health, and status endpoints' do
      {
        'GET': %w(
          auth/policies/version
          cfgmgmt/version
          compliance/reporting/version
          deployment/service_versions
          gateway/version
          gateway/health
          ingest/version
          license/status
          notifications/version
          telemetry/config
          version
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              status = automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: user,
              ).http_status
              expect(status).not_to eq 403
              expect(status).not_to eq 404 # let's check 404 where we can to avoid phantom tests
            end
          end
        end
      end
    end

    describe 'reading introspect data' do
      {
        'GET': %w(
          introspect
        ),
        'POST': %w(
          introspect
          introspect_some
        )
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe 'modifying secrets' do
      {

        'POST': %w(
          secrets
        ),
        'PATCH': %w(
          secrets/id/foo
        ),
        'DELETE': %w(
          secrets/id/foo
        ),
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer user" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: VIEWER_USERNAME,
              ).http_status
            ).to eq 403
          end

          it "#{method} #{url} does not return 403 for editor user" do
            expect(
              automate_api_request(
                "/api/v0/#{url}",
                http_method: method,
                user: EDITOR_USERNAME,
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe 'reading secrets' do
      {
        'GET': %w(
          secrets/id/foo
        ),
        'POST': %w(
          secrets/search
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} does not return 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/api/v0/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).not_to eq 403
            end
          end
        end
      end
    end

    describe "applying and requesting license" do
      {
        'POST': %w(
            apply
            request
        ),
      }.each do |method, urls|
        urls.each do |url|
          it "#{method} #{url} returns 403 for viewer user" do
            expect(
              automate_api_request(
                "/api/v0/license/#{url}",
                http_method: method,
                user: VIEWER_USERNAME,
              ).http_status
            ).to eq 403
          end

          it "#{method} #{url} does not return 403 for editor user" do
            expect(
              automate_api_request(
                "/api/v0/license/#{url}",
                http_method: method,
                user: EDITOR_USERNAME,
              ).http_status
            ).not_to eq 403
          end
        end
      end
    end

    describe "reading and modifying IAM data" do
      {
        'GET': %w(
          /apis/iam/v2beta/users
          /apis/iam/v2beta/teams
          /apis/iam/v2beta/tokens
          /apis/iam/v2beta/users/username
          /apis/iam/v2beta/teams/some-team-id
          /apis/iam/v2beta/tokens/some-token-id
        ),
        'DELETE': %w(
          /apis/iam/v2beta/users/username
          /apis/iam/v2beta/teams/some-team-id
          /apis/iam/v2beta/tokens/some-token-id
        ),
        'PUT': %w(
          /apis/iam/v2beta/users/username
          /apis/iam/v2beta/teams/some-team-id
          /apis/iam/v2beta/tokens/some-token-id
        ),
        'POST': %w(
          /apis/iam/v2beta/users
          /apis/iam/v2beta/teams
          /apis/iam/v2beta/tokens
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} returns 403 for #{user} user" do
              expect(
                automate_api_request(url,
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end
        end
      end

    [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
      it "a #{user} can see their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2beta/users/#{user}",
            http_method: "GET",
            user: user,
          ).http_status
        ).not_to eq 403
      end

      it "a #{user} can update their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2beta/users/#{user}",
            http_method: "PUT",
            user: user,
          ).http_status
        ).not_to eq 403
      end

      it "a #{user} cannot delete their own user record" do
        expect(
          automate_api_request(
            "/apis/iam/v2beta/users/#{user}",
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
        [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
          it "#{method} #{url} returns 200 for #{user} user" do
            expect(
              automate_api_request(
                "/apis/iam/v2beta/#{url}",
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
          policies
          roles
          policies/some-policy-id
          roles/some-role-id
          policies/some-policy-id/members
        ),
        'DELETE': %w(
          policies/some-policy-id
          roles/some-role-id
          projects/some-project-id
        ),
        'PUT': %w(
          policies/some-policy-id
          roles/some-role-id
          policies/some-policy-id/members
          projects/some-project-id
        ),
        'POST': %w(
          policies
          roles
          projects
          policies/some-policy-id/members:add
          policies/some-policy-id/members:remove
        ),
      }.each do |method, urls|
        urls.each do |url|
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} returns 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/apis/iam/v2beta/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
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
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} returns 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/api/v0/retention/nodes/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
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
          [ EDITOR_USERNAME, VIEWER_USERNAME ].each do |user|
            it "#{method} #{url} returns 403 for #{user} user" do
              expect(
                automate_api_request(
                  "/api/v0/notifications/#{url}",
                  http_method: method,
                  user: user,
                ).http_status
              ).to eq 403
            end
          end
        end
      end
    end

  end
end
