# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2.1 get all allowed projects for global projects filter'

control 'iam-v2-global-projects-filter-1' do
  title 'getting all allowed projects for a user on v2.1'

  PROJECT_ID_1 = "inspec-custom-project-1-#{Time.now.utc.to_i}"
  PROJECT_ID_2 = "inspec-custom-project-2-#{Time.now.utc.to_i}"
  PROJECT_ID_3 = "inspec-custom-project-3-#{Time.now.utc.to_i}"

  PROJECT_1 = {
          id: PROJECT_ID_1,
          name: "Test Project 1"
  }
  PROJECT_2 = {
          id: PROJECT_ID_2,
          name: "Test Project 2"
  }
  PROJECT_3 = {
          id: PROJECT_ID_3,
          name: "Test Project 3"
  }

  PROJECTS = [ PROJECT_1, PROJECT_2, PROJECT_3 ]
  
  describe 'allowed projects' do
    before(:all) do
      PROJECTS.each do|project|
        resp = automate_api_request("/apis/iam/v2beta/projects",
          http_method: 'POST',
          request_body: project.to_json
        )
        expect(resp.http_status).to eq 200
      end
    end
    
    after(:all) do
      PROJECTS.each do|project|
        resp = automate_api_request("/apis/iam/v2beta/projects/#{project[:id]}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
      end
    end

    describe 'allowed projects for admin' do
      it 'returns list of all projects and unassigned' do
        resp = automate_api_request("/api/v0/auth/introspect_projects", http_method: 'GET')

        expect(resp.http_status).to eq 200
        # always returns complete list of projects + (unassigned)
        expected_projects = PROJECTS.map { |p| p[:id] }.push('(unassigned)')
        expect(resp.parsed_response_body[:projects]).to match_array(expected_projects)
      end
    end

    describe 'allowed projects for non-admin' do
      non_admin_username = 'inspec-test-non-admin'
      non_admin_policy_id = 'non-admin-policy'

      before(:all) do
        create_user_response = automate_api_request(
          '/apis/iam/v2beta/users',
          http_method: 'POST',
          request_body: {
            'name': non_admin_username,
            'id': non_admin_username,
            'password': ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
          }.to_json
        )

        expect(create_user_response.http_status.to_s).to match(/200|409/)

        create_policy_response = automate_api_request("/apis/iam/v2beta/policies",
          http_method: 'POST',
          request_body: {
            id: non_admin_policy_id,
            name: 'gotta catch em all',
            members: ["user:local:#{non_admin_username}"],
            statements: [
              {
                effect: "ALLOW",
                # TODO this test will fail if there is a lone parameterized action (i.e. just iam:teams:get)
                actions: ["iam:teams:list", "iam:teams:get"],
                projects: [PROJECT_ID_1, PROJECT_ID_2]
              }
            ]
          }.to_json()
        )
        expect(create_policy_response.http_status.to_s).to match (/200|409/)
      end

      after(:all) do
        delete_user_response = automate_api_request(
          "/apis/iam/v2beta/users/#{non_admin_username}",
          http_method: 'DELETE',
        )

        expect(delete_user_response.http_status.to_s).to match(/200|404/)

        delete_policy_response = automate_api_request(
          "/apis/iam/v2beta/policies/#{non_admin_policy_id}",
          http_method: 'DELETE',
        )

        expect(delete_policy_response.http_status.to_s).to match(/200|404/)
      end

      it 'returns list of allowed projects' do
        resp = automate_api_request("/api/v0/auth/introspect_projects", http_method: 'GET', user: non_admin_username)

        expect(resp.http_status).to eq 200
        expected_projects = [PROJECT_ID_1, PROJECT_ID_2]
        expect(resp.parsed_response_body[:projects]).to match_array(expected_projects)
      end
    end
  end
end

