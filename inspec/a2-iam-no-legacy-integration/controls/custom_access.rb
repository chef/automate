# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

require_relative '../../constants'

title 'iam custom policy access integration tests'

control 'iam-custom-access-1' do
  title 'iam access using custom policies and roles'
  desc 'role-based access for custom policies on a fresh system with no legacy policies'

  describe 'custom role access' do
    before(:all) do
      TEAM_MANAGER = "team-manager-alice-#{TIMESTAMP}"
      TEAM_MANAGER_ROLE = "team-manager-#{TIMESTAMP}"
      TEAM_MANAGER_POLICY_ID = "team-manager-pol-#{TIMESTAMP}"
      PROJECT_1_ID = "project-1-#{TIMESTAMP}"
      PROJECT_1_TEAM_ID = "team-1-#{TIMESTAMP}"
      PROJECT_2_ID = "project-2-#{TIMESTAMP}"
      PROJECT_2_TEAM_ID = "team-2-#{TIMESTAMP}"

      create_user_resp = automate_api_request(
        '/apis/iam/v2/users',
        http_method: 'POST',
        request_body: {
          id: TEAM_MANAGER,
          name: TEAM_MANAGER,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_user_resp.http_status).to eq 200

      create_role_resp = automate_api_request(
        '/apis/iam/v2/roles',
        http_method: 'POST',
        request_body: {
          id: TEAM_MANAGER_ROLE,
          name: TEAM_MANAGER_ROLE,
          actions: ["iam:teams:*"],
        }.to_json
      )
      expect(create_role_resp.http_status).to eq 200

      create_project_resp = automate_api_request(
        '/apis/iam/v2/projects',
        http_method: 'POST',
        request_body: {
          id: PROJECT_1_ID,
          name: PROJECT_1_ID
        }.to_json
      )
      expect(create_project_resp.http_status).to eq 200

      create_project_2_resp = automate_api_request(
        '/apis/iam/v2/projects',
        http_method: 'POST',
        request_body: {
          id: PROJECT_2_ID,
          name: PROJECT_2_ID
        }.to_json
      )
      expect(create_project_2_resp.http_status).to eq 200

      create_team_resp = automate_api_request(
        '/apis/iam/v2/teams',
        http_method: 'POST',
        request_body: {
          id: PROJECT_2_TEAM_ID,
          name: PROJECT_2_TEAM_ID,
          projects: [PROJECT_2_ID]
        }.to_json
      )
      expect(create_team_resp.http_status).to eq 200

      create_policy_resp = automate_api_request("/apis/iam/v2/policies",
      http_method: 'POST',
      request_body: {
          id: TEAM_MANAGER_POLICY_ID,
          name: TEAM_MANAGER_POLICY_ID,
          members: ["user:local:#{TEAM_MANAGER}"],
          statements: [
            {
              effect: "ALLOW",
              role: TEAM_MANAGER_ROLE,
              actions: ["iam:projects:assign"],
              projects: [PROJECT_1_ID],
            }
          ]
        }.to_json
      )
      expect(create_policy_resp.http_status).to eq 200
    end

    after(:all) do
      delete_user_resp = automate_api_request(
        "/apis/iam/v2/users/#{TEAM_MANAGER}", http_method: 'DELETE')
      expect(delete_user_resp.http_status).to eq 200

      delete_role_resp = automate_api_request(
        "/apis/iam/v2/roles/#{TEAM_MANAGER_ROLE}", http_method: 'DELETE')
      expect(delete_role_resp.http_status).to eq 200

      delete_project_1_resp = automate_api_request(
        "/apis/iam/v2/projects/#{PROJECT_1_ID}", http_method: 'DELETE')
      expect(delete_project_1_resp.http_status).to eq 200

      delete_team_2_resp = automate_api_request(
        "/apis/iam/v2/teams/#{PROJECT_2_TEAM_ID}", http_method: 'DELETE')
      expect(delete_team_2_resp.http_status).to eq 200

      delete_project_2_resp = automate_api_request(
        "/apis/iam/v2/projects/#{PROJECT_2_ID}", http_method: 'DELETE')
      expect(delete_project_2_resp.http_status).to eq 200
    end

    describe 'team manager user' do
      
      it 'can create teams in their project' do 
        expect(
          automate_api_request(
            '/apis/iam/v2/teams',
            http_method: 'POST',
            user: TEAM_MANAGER,
            request_body: {
              id: PROJECT_1_TEAM_ID,
              name: PROJECT_1_TEAM_ID,
              projects: [PROJECT_1_ID]
            }.to_json
          ).http_status
        ).to eq 200
      end

      it 'can get the team in their project' do
        get_teams_resp = automate_api_request(
          "/apis/iam/v2/teams",
          user: TEAM_MANAGER
        )
        expect(get_teams_resp.http_status).to eq 200
        expect(get_teams_resp.parsed_response_body[:teams]).to eq [
          {
            id: PROJECT_1_TEAM_ID,
            name: PROJECT_1_TEAM_ID,
          projects: [PROJECT_1_ID]
          }
        ]

        expect(
          automate_api_request(
            "/apis/iam/v2/teams/#{PROJECT_1_TEAM_ID}",
            user: TEAM_MANAGER
          ).http_status
        ).to eq 200
      end

      it 'can modify a team in their project' do 
        expect(
          automate_api_request(
            "/apis/iam/v2/teams/#{PROJECT_1_TEAM_ID}",
            http_method: 'PUT',
            user: TEAM_MANAGER,
            request_body: {
              name: 'UPDATED NAME',
              projects: [PROJECT_1_ID]
            }.to_json
          ).http_status
        ).to eq 200
      end

      it 'can delete a team in their project' do
        expect(
          automate_api_request(
            "/apis/iam/v2/teams/#{PROJECT_1_TEAM_ID}",
            http_method: 'DELETE',
            user: TEAM_MANAGER
          ).http_status
        ).to eq 200
      end

      it 'cannot view a team outside their project' do
        expect(
          automate_api_request(
            "/apis/iam/v2/teams/#{PROJECT_2_TEAM_ID}",
            user: TEAM_MANAGER
          ).http_status
          # teams outside their project will return as Not Found
        ).to eq 404
      end

      it 'cannot modify a team outside their project' do
        expect(
          automate_api_request(
            "/apis/iam/v2/teams/#{PROJECT_2_TEAM_ID}",
            http_method: 'PUT',
            user: TEAM_MANAGER,
            request_body: {
              name: 'UPDATED NAME',
              projects: [PROJECT_2_ID]
            }.to_json
          ).http_status
          # teams outside their project will return as Not Found
        ).to eq 404
      end
    end
  end
end
