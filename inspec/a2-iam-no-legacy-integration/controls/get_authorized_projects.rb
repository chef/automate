# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

require_relative '../../constants'

title 'get all allowed projects for global projects filter'

control 'iam-global-projects-filter-1' do
  title 'getting all allowed projects for a user'

 describe 'allowed projects' do
    PROJECT_ID_1 = "inspec-custom-project-1-#{Time.now.utc.to_i}"
    PROJECT_ID_2 = "inspec-custom-project-2-#{Time.now.utc.to_i}"
    PROJECT_ID_3 = "inspec-custom-project-3-#{Time.now.utc.to_i}"

    PROJECT_1 = { id: PROJECT_ID_1, name: "Test Project 1" }
    PROJECT_2 = { id: PROJECT_ID_2, name: "Test Project 2" }
    PROJECT_3 = { id: PROJECT_ID_3, name: "Test Project 3" }
    PROJECTS = [ PROJECT_1, PROJECT_2, PROJECT_3 ]
    UNASSIGNED = { id: UNASSIGNED_PROJECT_ID, name: UNASSIGNED_PROJECT_NAME }

    before(:all) do
      PROJECTS.each do|project|
        resp = automate_api_request("/apis/iam/v2/projects",
          http_method: 'POST',
          request_body: project.to_json
        )
        expect(resp.http_status).to eq 200
      end
    end

    after(:all) do
      PROJECTS.each do|project|
        resp = automate_api_request("/apis/iam/v2/projects/#{project[:id]}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
      end
    end

    describe 'allowed projects for admin' do
      it 'returns list of all projects and unassigned' do
        resp = automate_api_request("/apis/iam/v2/introspect_projects", http_method: 'GET')

        expect(resp.http_status).to eq 200
        expected_projects = [ PROJECT_1, PROJECT_2, PROJECT_3, UNASSIGNED ]
        expected_projects.each do |p|
          expect(resp.parsed_response_body[:projects]).to include(
            { id: p[:id],
              name: p[:name],
              type: p[:id] == UNASSIGNED_PROJECT_ID ? "CHEF_MANAGED" : "CUSTOM",
              status: "NO_RULES"
            })
        end
      end
    end

    describe 'allowed projects for non-admin' do
      non_admin_username = 'inspec-test-non-admin'
      non_admin_policy_id = 'non-admin-policy'

      before(:all) do
        create_user_response = automate_api_request(
          '/apis/iam/v2/users',
          http_method: 'POST',
          request_body: {
            'name': non_admin_username,
            'id': non_admin_username,
            'password': ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
          }.to_json
        )

        expect(create_user_response.http_status.to_s).to match(/200|409/)

        create_policy_response = automate_api_request("/apis/iam/v2/policies",
          http_method: 'POST',
          request_body: {
            id: non_admin_policy_id,
            name: 'gotta catch em all',
            members: ["user:local:#{non_admin_username}"],
            statements: [
              {
                effect: "ALLOW",
                actions: ["iam:teams:get"],
                projects: [PROJECT_ID_1, PROJECT_ID_2]
              }
            ]
          }.to_json()
        )
        expect(create_policy_response.http_status.to_s).to match (/200|409/)
      end

      after(:all) do
        delete_user_response = automate_api_request(
          "/apis/iam/v2/users/#{non_admin_username}",
          http_method: 'DELETE',
        )

        expect(delete_user_response.http_status.to_s).to match(/200|404/)

        delete_policy_response = automate_api_request(
          "/apis/iam/v2/policies/#{non_admin_policy_id}",
          http_method: 'DELETE',
        )

        expect(delete_policy_response.http_status.to_s).to match(/200|404/)
      end

      it 'returns list of allowed projects' do
        resp = automate_api_request("/apis/iam/v2/introspect_projects",
          http_method: 'GET',
          user: non_admin_username,
        )

        expect(resp.http_status).to eq 200
        expected_projects = [PROJECT_1, PROJECT_2]
        expect(resp.parsed_response_body[:projects]).to match_array(
          expected_projects.map { |p|
            { id: p[:id],
              name: p[:name],
              type: "CUSTOM",
              status: "NO_RULES"
            }}
        )
      end
    end
  end
end

