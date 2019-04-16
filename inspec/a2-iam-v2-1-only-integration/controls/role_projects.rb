# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2.1 project filtering API integration tests'

control 'iam-v2-projects-1' do
  title 'v2.1-only access for project filtering'

  CUSTOM_PROJECT_ID_1 = 'inspec-custom-project-1'
  CUSTOM_PROJECT_ID_2 = 'inspec-custom-project-2'
  CUSTOM_ROLE_ID_1 = 'inspec-custom-role-1'
  CUSTOM_ROLE_ID_2 = 'inspec-custom-role-2'
  CUSTOM_ROLE_ID_3 = 'inspec-custom-role-3'

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

  Roles = [ CUSTOM_ROLE_1, CUSTOM_ROLE_2, CUSTOM_ROLE_3 ]

  CUSTOM_PROJECT_1 = {
          id: CUSTOM_PROJECT_ID_1,
          name: "Test Project 1"
  }
  CUSTOM_PROJECT_2 = {
          id: CUSTOM_PROJECT_ID_2,
          name: "Test Project 2"
  }

  Projects = [ CUSTOM_PROJECT_1, CUSTOM_PROJECT_2 ]

  describe 'get roles' do

    before(:all) do
      Projects.each do|project|
        resp = automate_api_request("/apis/iam/v2beta/projects",
          http_method: 'POST',
          request_body: project.to_json
        )
        expect(resp.http_status).to eq 200
      end
 
      Roles.each do|role|
        resp = automate_api_request("/apis/iam/v2beta/roles",
          http_method: 'POST',
          request_body: role.to_json
        )
        expect(resp.http_status).to eq 200
      end
   end

    after(:all) do
      Roles.each do|role|
        resp = automate_api_request("/apis/iam/v2beta/roles/#{role[:id]}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
      end
      Projects.each do|project|
        resp = automate_api_request("/apis/iam/v2beta/projects/#{project[:id]}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
      end
    end

    it 'returns 1 filtered role' do
      resp = automate_api_request(
        "/apis/iam/v2beta/roles",
        request_headers: { 'projects': CUSTOM_PROJECT_ID_1 },
        )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to eq 1
      expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
    end

    it 'returns 2 filtered roles' do
      resp = automate_api_request(
        "/apis/iam/v2beta/roles",
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
        "/apis/iam/v2beta/roles",
        request_headers: { 'projects': 'unknown-project' },
        )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to eq 0
    end

    it 'returns some roles that are unassigned' do
      # these are system default roles
      resp = automate_api_request(
        "/apis/iam/v2beta/roles",
        request_headers: { 'projects': '(unassigned)' },
        )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles]).not_to be_empty
    end

    it 'returns all roles when unfiltered' do
      # these are system default roles plus 3 roles used here
      resp = automate_api_request(
        "/apis/iam/v2beta/roles"
        )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to be > 3
    end
  end
end
