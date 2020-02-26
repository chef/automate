require_relative '../../../constants'

# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'iam project filtering integration tests'

control 'iam-project-filtering-1' do
  title 'project filtering using the role API as an example'

  CUSTOM_PROJECT_ID_1 = "inspec-custom-project-role-test-1-#{TIMESTAMP}"
  CUSTOM_PROJECT_ID_2 = "inspec-custom-project-role-test-2-#{TIMESTAMP}"
  CUSTOM_ROLE_ID_1 = "inspec-custom-role-1-#{TIMESTAMP}"
  CUSTOM_ROLE_ID_2 = "inspec-custom-role-2-#{TIMESTAMP}"
  CUSTOM_ROLE_ID_3 = "inspec-custom-role-3-#{TIMESTAMP}"

  CUSTOM_ROLE_1 = {
          id: CUSTOM_ROLE_ID_1,
          name: "Test Role 1",
          actions: ["test:some:action", "test:other:action"],
          projects: [CUSTOM_PROJECT_ID_1, CUSTOM_PROJECT_ID_2],
          type: "CUSTOM"
  }
  CUSTOM_ROLE_2 = {
          id: CUSTOM_ROLE_ID_2,
          name: "Test Role 2",
          actions: ["test:other:action"],
          projects: [CUSTOM_PROJECT_ID_2],
          type: "CUSTOM"
  }
  CUSTOM_ROLE_3 = {
          id: CUSTOM_ROLE_ID_3,
          name: "Test Role 2",
          actions: ["test:other:action"],
          projects: [],
          type: "CUSTOM"
  }

  CUSTOM_ROLES = [ CUSTOM_ROLE_1, CUSTOM_ROLE_2, CUSTOM_ROLE_3 ]

  CUSTOM_PROJECT_1 = {
          id: CUSTOM_PROJECT_ID_1,
          name: "Test Project 1"
  }
  CUSTOM_PROJECT_2 = {
          id: CUSTOM_PROJECT_ID_2,
          name: "Test Project 2"
  }

  Projects = [ CUSTOM_PROJECT_1, CUSTOM_PROJECT_2 ]

  describe 'project filtering' do
    before(:all) do
      Projects.each do|project|
        resp = automate_api_request("/apis/iam/v2/projects",
          http_method: 'POST',
          request_body: project.to_json
        )
        expect(resp.http_status).to eq 200
      end

      CUSTOM_ROLES.each do|role|
        resp = automate_api_request("/apis/iam/v2/roles",
          http_method: 'POST',
          request_body: role.to_json
        )
        expect(resp.http_status).to eq 200
      end
    end

    after(:all) do
      CUSTOM_ROLES.each do|role|
        resp = automate_api_request("/apis/iam/v2/roles/#{role[:id]}", http_method: 'delete')
        expect(resp.http_status.to_s).to match(/200|404/)
      end
      Projects.each do|project|
        resp = automate_api_request("/apis/iam/v2/projects/#{project[:id]}", http_method: 'delete')
        # TODO (tc) remove 500 after API bug fixed: https://github.com/chef/automate/issues/2126
        expect(resp.http_status.to_s).to match(/200|404|500/)
      end
    end

    describe 'roles with default admin policy' do
      it 'returns roles for allowed project' do
        resp = automate_api_request(
          "/apis/iam/v2/roles",
          request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2 ] },
          )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:roles].length).to eq 2
        expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_2 }).to_not be_nil
        expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
      end

      it 'returns single role containing a project' do
        resp = automate_api_request(
          "/apis/iam/v2/roles",
          request_headers: { 'projects': CUSTOM_PROJECT_ID_1 },
          )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:roles].length).to eq 1
        expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
      end

      it 'returns multiple roles containing a project' do
        resp = automate_api_request(
          "/apis/iam/v2/roles",
          request_headers: { 'projects': CUSTOM_PROJECT_ID_2 },
          )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:roles].length).to eq 2
        # Would be nice to use the include matcher, but child arrays may differ in order.
        # expect(resp.parsed_response_body[:roles]).to include(CUSTOM_ROLE_2)
        # Instead, just check IDs:
        expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_2 }).to_not be_nil
        expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
      end

      it 'returns no filtered roles for unknown project' do
        resp = automate_api_request(
          "/apis/iam/v2/roles",
          request_headers: { 'projects': 'unknown-project' },
          )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:roles].length).to eq 0
      end

      it 'returns some roles that are unassigned' do
        # these are system default roles
        resp = automate_api_request(
          "/apis/iam/v2/roles",
          request_headers: { 'projects': '(unassigned)' },
          )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:roles]).not_to be_empty
      end

      it 'returns all roles when unfiltered' do
        # these are system default roles plus 3 roles used here
        resp = automate_api_request(
          "/apis/iam/v2/roles"
          )
        expect(resp.http_status).to eq 200
        expected_roles = CUSTOM_ROLES.map { |r| r[:id] } + DEFAULT_ROLE_IDS
        expected_roles.each do |role|
          expect(resp.parsed_response_body[:roles].map{|r| r[:id] }).to include(role)
        end
      end
    end

    describe 'roles with deny statements' do
      DENY_POLICY_ID = "inspec-test-policy-1-#{TIMESTAMP}"
      POLICY_ROLE_ID = "inspec-test-role-1-#{TIMESTAMP}"

      before(:all) do
        resp = automate_api_request("/apis/iam/v2/roles",
          http_method: 'POST',
          request_body: {
            id: POLICY_ROLE_ID,
            name: "display name !#$#",
            actions: ["iam:roles:*"]
          }.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/policies",
          http_method: 'POST',
          request_body: {
            id: DENY_POLICY_ID,
            name: 'brand new name',
            members: ["user:local:*", "token:*"],
            statements: [
              {
                effect: "DENY",
                role: POLICY_ROLE_ID,
                projects: [CUSTOM_PROJECT_ID_1]
              },
              {
                effect: "ALLOW",
                role: POLICY_ROLE_ID,
                projects: [CUSTOM_PROJECT_ID_1, CUSTOM_PROJECT_ID_2]
              }
            ]
          }.to_json()
        )
        expect(resp.http_status).to eq 200
      end

      after(:all) do
        resp = automate_api_request("/apis/iam/v2/policies/#{DENY_POLICY_ID}", http_method: 'delete')
        expect(resp.http_status).to eq 200
        resp = automate_api_request("/apis/iam/v2/roles/#{POLICY_ROLE_ID}", http_method: 'delete')
        expect(resp.http_status).to eq 200
        CUSTOM_ROLES.each do|role|
          if role[:id] != CUSTOM_ROLE_ID_1
            resp = automate_api_request("/apis/iam/v2/roles/#{role[:id]}", http_method: 'delete')
            expect(resp.http_status).to eq 200
          end
        end
        Projects.each do|project|
          resp = automate_api_request("/apis/iam/v2/projects/#{project[:id]}", http_method: 'delete')
          expect(resp.http_status).to eq 200
        end
      end

      describe 'ListRoles' do

        it 'returns roles for allowed projects' do resp = automate_api_request(
            "/apis/iam/v2/roles",
            request_headers: { 'projects': CUSTOM_PROJECT_ID_2 },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:roles].length).to eq 2
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_2 }).to_not be_nil
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
        end

        it 'returns 403 due to explicitly denied project' do
          resp = automate_api_request(
            "/apis/iam/v2/roles",
            request_headers: { 'projects': CUSTOM_PROJECT_ID_1 },
            )
          expect(resp.http_status).to eq 403
        end

        it 'returns no roles due to project not found' do
          resp = automate_api_request(
            "/apis/iam/v2/roles",
            request_headers: { 'projects': "unknown-project" },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:roles].length).to eq 0
        end

        it 'returns roles for allowed project even though denied on another project' do
          resp = automate_api_request(
            "/apis/iam/v2/roles",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2, CUSTOM_PROJECT_ID_1 ] },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:roles].length).to eq 2
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_2 }).to_not be_nil
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
        end

        it 'returns roles for allowed project even though other project not found' do
          resp = automate_api_request(
            "/apis/iam/v2/roles",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2, "unknown-project" ] },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:roles].length).to eq 2
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_2 }).to_not be_nil
          expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
        end

      end #ListRoles

      describe 'GetRole' do

        it 'returns role for allowed project filtered by that project' do
          # role2 includes project2, which is allowed by policy
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_2}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2 ] },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:role][:name]).to eq CUSTOM_ROLE_2[:name]
          expect(resp.parsed_response_body[:role][:id]).to eq CUSTOM_ROLE_2[:id]
        end

        it 'returns role for other project' do
          # role1 includes project1 & project2, and project2 is allowed, so should return it
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2 ] },
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:role][:name]).to eq CUSTOM_ROLE_1[:name]
          expect(resp.parsed_response_body[:role][:id]).to eq CUSTOM_ROLE_1[:id]
        end

        it 'returns role for project with no filters applied' do
          # role1 includes project1 (denied by policy) & project2 (allowed by policy)
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:role][:name]).to eq CUSTOM_ROLE_1[:name]
          expect(resp.parsed_response_body[:role][:id]).to eq CUSTOM_ROLE_1[:id]
        end

        it 'returns 403 for denied project on the role' do
          # role1 includes project1 (denied by policy) so result is denied
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_1 ] },
            )
          expect(resp.http_status).to eq 403
        end

        it 'returns 404 when not found after applying project filter' do
          # role1 does not include the project so should not be found
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ "unknown-project" ] },
            )
          expect(resp.http_status).to eq 404
        end

      end #GetRole

      describe 'UpdateRole' do

        it "returns 403 when attempting update for denied project" do
          # role1 includes project1 & project2, and project1 is denied
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_1 ] },
            http_method: 'PUT',
            request_body: {
              name: "inspec test role updated",
              actions: ["brand:new:action"],
              projects: [CUSTOM_PROJECT_ID_1, CUSTOM_PROJECT_ID_2]
            }.to_json
          )
          expect(resp.http_status).to eq 403
        end

        it 'returns 404 when not found after applying project filter' do
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ "unknown-project" ] },
            http_method: 'PUT',
            request_body: {
              name: "inspec test role updated",
              actions: ["brand:new:action"],
              projects: [CUSTOM_PROJECT_ID_1, CUSTOM_PROJECT_ID_2]
            }.to_json
          )
          expect(resp.http_status).to eq 404
        end

        it "allows update for allowed project" do
          # role1 includes project1 & project2, and project2 is allowed, so should return it
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2 ] },
            http_method: 'PUT',
            request_body: {
              name: "inspec test role updated",
              actions: ["brand:new:action"],
              projects: [CUSTOM_PROJECT_ID_1, CUSTOM_PROJECT_ID_2]
            }.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:role][:name]).to eq "inspec test role updated"
          expect(resp.parsed_response_body[:role][:actions]).to eq ["brand:new:action"]
          expect(resp.parsed_response_body[:role][:projects].length).to eq 2
        end

      end #UpdateRole

      describe 'DeleteRole' do

        it "returns 403 when attempting delete for denied project" do
          # role1 includes project1 & project2, and project1 is denied

          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_1 ] },
            http_method: 'DELETE'
          )
          expect(resp.http_status).to eq 403
        end

        it 'returns 404 when not found after applying project filter' do

          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ "unknown-project" ] },
            http_method: 'DELETE'
          )
          expect(resp.http_status).to eq 404
        end

        it "allows delete for allowed project" do
          # role1 includes project1 & project2, and project2 is allowed
          resp = automate_api_request(
            "/apis/iam/v2/roles/#{CUSTOM_ROLE_ID_1}",
            request_headers: { 'projects': [ CUSTOM_PROJECT_ID_2 ] },
            http_method: 'DELETE'
          )
          expect(resp.http_status).to eq 200
        end

      end #DeleteRole
    end
  end
end
