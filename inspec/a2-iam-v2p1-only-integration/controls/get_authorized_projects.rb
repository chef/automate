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

  describe 'allowed projects for admin' do

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
        resp = automate_api_request("/apis/iam/v2beta/projects/#{project[:id]}", http_method: 'delete')
        expect(resp.http_status).to eq 200
      end
    end
 
    it 'returns list of all projects and unassigned' do
      resp = automate_api_request("/api/v0/auth/introspect_projects", http_method: 'get')

      expect(resp.http_status).to eq 200
      # always returns complete list of projects + (unassigned)
      expect(resp.parsed_response_body[:projects].length).to eq PROJECTS.length + 1
      expected_projects = PROJECTS.map { |p| p[:id] }.push('(unassigned)')
      expect(resp.parsed_response_body[:projects]).to match_array(expected_projects)
    end
  end
end

  # describe 'all allowed projects for non-admin user' do

  #   before(:all) do
  #     #   // before: 
  #     #   // create projects
  #     #   // create policy
  #     #   // create user
  #     #   // log in as user
  #     Projects.each do|project|
  #       resp = automate_api_request("/apis/iam/v2beta/projects",
  #         http_method: 'POST',
  #         request_body: project.to_json
  #       )
  #       expect(resp.http_status).to eq 200
  #     end
  #     end
  #   end

  #   after(:all) do
  #     Projects.each do|project|
  #       resp = automate_api_request("/apis/iam/v2beta/projects/#{project[:id]}", http_method: 'delete')
  #       expect(resp.http_status).to eq 200
  #     end
  #   end
 
  #   # // for non-admin with access to project 1 and 2, only displays those 2 projects
  #   it "returns only projects specified in user's policy" do
  #   #   // assert: 
  #   #   // check global projects filter contents
  #   #   expect(resp.parsed_response_body[:roles].find { |item| item[:id] == CUSTOM_ROLE_ID_1 }).to_not be_nil
  #   end
  # end

