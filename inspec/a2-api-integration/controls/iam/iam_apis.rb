require_relative '../../../constants'

# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'iam REST API integration tests'

# TODO port this test to Cypress integration/api tests

control 'iam-api-1' do
  title 'iam endpoints'
  desc 'Verify behavior of all the endpoints under the "iam" namespace'

  TOKEN_ID = "iam-1-token-#{TIMESTAMP}"
  TOKEN_ID_2 = "iam-1-token-2-#{TIMESTAMP}"
  TOKEN_ID_3 = "iam-1-token-3-#{TIMESTAMP}"
  TOKEN_NAME = 'iam REST API integration test token'
  USER_ID = "inspec-user-#{TIMESTAMP}"
  TEAM_ID = "inspec-team-#{TIMESTAMP}"
  POLICY_ID = "inspec-custom-policy-#{TIMESTAMP}"
  ROLE_ID = "inspec-custom-role-#{TIMESTAMP}"
  
  describe "tokens API" do

    project_id = "inspec-token-project-#{TIMESTAMP}"
    project_id_2 = "inspec-token-project-2-#{TIMESTAMP}"

    before(:all) do
      resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: project_id,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: project_id_2,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID_2}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID_3}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/projects/#{project_id}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/projects/#{project_id_2}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
    end

    it "CREATE token responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.http_status).to eq 200
      init_token_count = resp.parsed_response_body[:tokens].length

      resp = automate_api_request("/apis/iam/v2/tokens",
        http_method: 'POST',
        request_body: {
          id: TOKEN_ID,
          name: TOKEN_NAME,
          active: true,
          projects: [project_id]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:id]).to eq TOKEN_ID
      expect(resp.parsed_response_body[:token][:name]).to eq TOKEN_NAME
      expect(resp.parsed_response_body[:token][:active]).to be true
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id]

      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.parsed_response_body[:tokens].length).to eq init_token_count + 1
      expect(resp.parsed_response_body[:tokens].map {|t| t[:id]}).to include(TOKEN_ID)
    end

    it "CREATE sets active to True when not passed" do
      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.http_status).to eq 200
      init_token_count = resp.parsed_response_body[:tokens].length

      resp = automate_api_request("/apis/iam/v2/tokens",
        http_method: 'POST',
        request_body: {
          id: TOKEN_ID_2,
          name: TOKEN_NAME,
          projects: [project_id]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:id]).to eq TOKEN_ID_2
      expect(resp.parsed_response_body[:token][:name]).to eq TOKEN_NAME
      expect(resp.parsed_response_body[:token][:active]).to be true
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id]

      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID_2}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:active]).to eq true

      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.parsed_response_body[:tokens].length).to eq init_token_count + 1
      expect(resp.parsed_response_body[:tokens].map {|t| t[:id]}).to include(TOKEN_ID_2)
    end

    it "CREATE token succeeds if projects are empty" do
      resp = automate_api_request("/apis/iam/v2/tokens",
        http_method: 'POST',
        request_body: {
          id: TOKEN_ID_3,
          name: TOKEN_ID_3
        }.to_json
      ) 
      expect(resp.http_status).to eq 200
    end

    it "CREATE token returns a 409 on ID conflict" do
      response = automate_api_request(
        "/apis/iam/v2/tokens",
        http_method: "POST",
          request_body: {
            id: TOKEN_ID,
            name: TOKEN_NAME
          }.to_json
      )
      expect(response.http_status).to eq 409
    end 

    it "LIST tokens returns list of tokens" do
      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body.keys).to include (:tokens)
    end

    it "GET token responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:id]).to eq TOKEN_ID
      expect(resp.parsed_response_body[:token][:name]).to eq TOKEN_NAME
      expect(resp.parsed_response_body[:token][:active]).to eq true
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id]
    end

    it "UPDATE token responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test token updated",
          projects: [project_id, project_id_2]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:name]).to eq "inspec test token updated"
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id, project_id_2]
    end

    it "UPDATE token defaults active to true if not specified" do
      # precondition: set active to false
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          active: false,
          name: "inspec test token updated",
          projects: [project_id, project_id_2]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:name]).to eq "inspec test token updated"
      expect(resp.parsed_response_body[:token][:active]).to eq false
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id, project_id_2]

      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:active]).to eq false

      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test token updated",
          projects: [project_id, project_id_2]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:name]).to eq "inspec test token updated"
      expect(resp.parsed_response_body[:token][:active]).to eq true
      expect(resp.parsed_response_body[:token][:projects]).to eq [project_id, project_id_2]

      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:active]).to eq true
    end

    it "UPDATE token succeeds if update does not include projects" do
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test token updated",
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:projects]).to eq []
    end

    it "DELETE token succeeds with a valid id" do
      resp = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/tokens")
      expect(resp.parsed_response_body[:tokens].map {|t| t[:id]}).to_not include(TOKEN_ID)
    end

    it "GET/DELETE/PUT returns 404 Not Found if the token does not exist" do
        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}", http_method: 'PUT')
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}", http_method: 'DELETE')
        expect(response.http_status).to eq 404
    end
  end

  describe "users API" do
    TEST_USER = {
        id: USER_ID,
        name: "display name !#$#",
        password: "chefautomate"
    }

    describe "when the user is not yet created" do
      CREATED_ID = "unique-user-id"

      after(:each) do
        resp = automate_api_request("/apis/iam/v2/users/#{CREATED_ID}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)
      end

      describe "POST /iam/v2/users/:id" do
        it "creates the user" do
          createdName =  "i created my own name"
          resp = automate_api_request("/apis/iam/v2/users",
            http_method: 'POST',
            request_body: {
              id: CREATED_ID,
              name: createdName,
              password: 'something-new'
            }.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:user][:name]).to eq createdName
          expect(resp.parsed_response_body[:user][:id]).to eq CREATED_ID
        end
      end
    end

    describe "when multiple users exists" do
      let (:custom_user_id_2) { 'inspec-user-2' }
      let (:custom_user_2) do
        {
          id: custom_user_id_2,
          name: "display name 2",
          password: "chefautomate"
        }
      end

      before(:each) do
        resp = automate_api_request("/apis/iam/v2/users",
          http_method: 'POST',
          request_body: TEST_USER.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/users",
          http_method: 'POST',
          request_body: custom_user_2.to_json
        )
        expect(resp.http_status).to eq 200
      end

      after(:each) do
        resp = automate_api_request("/apis/iam/v2/users/#{USER_ID}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2/users/#{custom_user_id_2}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)
      end

      describe "GET /iam/v2/users/" do
        it "returns the list of users" do
          resp = automate_api_request("/apis/iam/v2/users")
          expect(resp.http_status).to eq 200

          user_ids = resp.parsed_response_body[:users].map { |u| u[:id] }
          expect(user_ids).to include(USER_ID)
          expect(user_ids).to include(custom_user_id_2)
          expect(user_ids).to include(ADMIN_USER_ID)
        end
      end

      describe "PUT /apis/iam/v2/self/:id" do
        it "user can update their own display name" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/self/#{USER_ID}",
            http_method: 'PUT',
            request_body: {
              name: updatedName
            }.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:user][:name]).to eq updatedName
        end

        it "user gets a 400 if their password is wrong" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/self/#{USER_ID}",
            http_method: 'PUT',
            request_body: {
              name: updatedName,
              password: "newpassword",
              previous_password: "wrongagain"
            }.to_json
          )
          expect(resp.http_status).to eq 400
        end

        # TODO (tc): right now, it returns 400 for a non-existent user because
        # it checks for the password first, and if it doesn't match for a user
        # that doesn't exist (always the case), it returns a 400.
        # It should return a 404.
        it "returns 400 for a non-existent user" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/self/some_wrong_id",
            http_method: 'PUT',
            request_body: {
              name: updatedName,
              password: "newpassword",
              previous_password: "chefautomate"
            }.to_json
          )
          expect(resp.http_status).to eq 400
        end

        it "user can update their own display name and password" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/self/#{USER_ID}",
            http_method: 'PUT',
            request_body: {
              name: updatedName,
              password: "newpassword",
              previous_password: "chefautomate"
            }.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:user][:name]).to eq updatedName
        end
      end

      describe "DELETE /iam/v2/users/:id" do
        it "deletes the user if it exists" do
          resp = automate_api_request("/apis/iam/v2/users/#{USER_ID}")
          expect(resp.http_status).to eq 200

          resp = automate_api_request("/apis/iam/v2/users/#{USER_ID}", http_method: 'DELETE')
          expect(resp.http_status).to eq 200

          resp = automate_api_request("/apis/iam/v2/users/#{USER_ID}")
          expect(resp.http_status).to eq 404
        end

        it "user gets a 404 when the user does not exist" do
          resp = automate_api_request("/apis/iam/v2/users/some_wrong_id", http_method: 'DELETE')
          expect(resp.http_status).to eq 404
        end
      end

      describe "PUT /iam/v2/users/:id" do
        it "updates the user if it exists" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/users/#{USER_ID}",
            http_method: 'PUT',
            request_body: {
              name: updatedName,
              password: 'something-new'
            }.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:user][:name]).to eq updatedName
        end

        it "returns 404 if the user does not exist" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2/users/some_wrong_id",
            http_method: 'PUT',
            request_body: {
              name: updatedName,
              password: "newpassword",
              previous_password: "wrongagain"
            }.to_json
          )
          expect(resp.http_status).to eq 404
        end
      end
    end
  end

  describe "teams API" do
    project_id = "inspec-team-project-#{TIMESTAMP}"
    project_id_2 = "inspec-team-project-2-#{TIMESTAMP}"

    before(:all) do
      resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: project_id,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: project_id_2,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2/projects/#{project_id}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/projects/#{project_id_2}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
    end

    it "LIST teams responds properly" do
      resp = automate_api_request("/apis/iam/v2/teams")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body.keys).to include(:teams)
    end

    it "CREATE team responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/teams")
      expect(resp.http_status).to eq 200
      init_team_count = resp.parsed_response_body[:teams].length

      projects = [project_id_2]
      resp = automate_api_request("/apis/iam/v2/teams",
        http_method: 'POST',
        request_body: {
          id: TEAM_ID,
          name: "display name !#$#",
          projects: projects
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:id]).to eq TEAM_ID
      expect(resp.parsed_response_body[:team][:name]).to eq 'display name !#$#'
      expect(resp.parsed_response_body[:team][:projects]).to eq projects

      resp = automate_api_request("/apis/iam/v2/teams")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:teams].length).to eq init_team_count + 1
      expect(resp.parsed_response_body[:teams].map { |p| p[:id] }).to include TEAM_ID
    end

    it "CREATE team returns a 409 on ID conflict" do
      response = automate_api_request(
        "/apis/iam/v2/teams",
        http_method: "POST",
          request_body: {
            id: TEAM_ID,
            name: "display name !#$#"
          }.to_json
      )
      expect(response.http_status).to eq 409
    end 

    it "GET team responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE team responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:name]).to eq "display name !#$#"

      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test team updated",
          projects: [project_id_2]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:name]).to eq "inspec test team updated"
    end

    it "POST (add) and POST (remove) team members responds properly to happy path inputs" do
      user_id_1 = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      user_id_2 = "93a2fe1f-0793-4cbe-9b4b-737a9316a515"
      users = [user_id_1, user_id_2]
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: users,
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 2
      expect(resp.parsed_response_body[:user_ids]).to cmp(users)

      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 2

      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users:remove",
        http_method: 'POST',
        request_body: {
          user_ids: [user_id_2],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 1

      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq [user_id_1]
    end

    it "GET teams for user responds properly to happy path inputs" do
      user = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: [user],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])

      resp = automate_api_request("/apis/iam/v2/users/#{user}/teams")
      expect(resp.http_status).to eq 200
    end

    it "GET users for team responds properly to happy path inputs" do
      user = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: [user],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])

      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])
    end

    it "DELETE team succeeds with a valid id" do
      resp = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/teams")
      expect(resp.parsed_response_body[:teams].map {|t| t[:id]}).to_not include(TEAM_ID)
    end

    it "GET/DELETE/PUT returns 404 Not Found if the team does not exist" do
        response = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}")
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}", 
          http_method: 'PUT',
          request_body: {
            name: "updated inspec team",
          }.to_json
        )
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/teams/#{TEAM_ID}", http_method: 'DELETE')
        expect(response.http_status).to eq 404
    end
  end

  describe "roles API" do

    after(:all) do
      resp = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}", http_method: 'DELETE')
      expect(resp.http_status.to_s).to match(/200|404/)
    end

    it "CREATE role responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/roles")
      expect(resp.http_status).to eq 200
      init_role_count = resp.parsed_response_body[:roles].length

      id = "inspec-role-#{TIMESTAMP}"
      resp = automate_api_request("/apis/iam/v2/roles",
        http_method: 'POST',
        request_body: {
          id: ROLE_ID,
          name: "display name !#$#",
          actions: ["test:some:action", "test:other:action"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:role][:id]).to eq ROLE_ID
      expect(resp.parsed_response_body[:role][:name]).to eq 'display name !#$#'
      expect(resp.parsed_response_body[:role][:actions].length).to eq 2

      resp = automate_api_request("/apis/iam/v2/roles")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to eq init_role_count + 1
    end

    it "CREATE role returns a 409 on ID conflict" do
      response = automate_api_request(
        "/apis/iam/v2/roles",
        http_method: "POST",
          request_body: {
            id: ROLE_ID,
          name: "display name !#$#",
          actions: ["test:some:action", "test:other:action"]
          }.to_json
      )
      expect(response.http_status).to eq 409
    end 

    it "LIST roles responds properly" do
      resp = automate_api_request("/apis/iam/v2/roles")
      expect(resp.http_status).to eq 200
    end

    it "GET role responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE role responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test role updated",
          actions: ["brand:new:action"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:role][:name]).to eq "inspec test role updated"
      expect(resp.parsed_response_body[:role][:actions]).to eq ["brand:new:action"]
    end

    it "DELETE role succeeds with a valid id" do
      resp = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/roles")
      expect(resp.parsed_response_body[:roles].map {|t| t[:id]}).to_not include(ROLE_ID)
    end

    it "GET/DELETE/PUT returns 404 Not Found if the role does not exist" do
        response = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}")
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}", 
          http_method: 'PUT',
          request_body: {
            name: "updated inspec role",
            actions: ["some:action:todo"]
          }.to_json
        )
        expect(response.http_status).to eq 404

        response = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}", http_method: 'DELETE')
        expect(response.http_status).to eq 404
    end
  end

  describe "projects API" do
    custom_project_id = "inspec-custom-project-api-test-#{TIMESTAMP}"
    before(:all) do
       resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: custom_project_id,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "LIST projects responds properly" do
      resp = automate_api_request("/apis/iam/v2/projects")
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE project properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/projects")
      expect(resp.http_status).to eq 200
      init_project_count = resp.parsed_response_body[:projects].length

      id = "inspec-project-#{TIMESTAMP}"
      resp = automate_api_request("/apis/iam/v2/projects",
        http_method: 'POST',
        request_body: {
          id: id,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:project][:id]).to eq id
      expect(resp.parsed_response_body[:project][:name]).to eq 'display name !#$#'

      resp = automate_api_request("/apis/iam/v2/projects")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:projects].length).to eq init_project_count + 1
      expect(resp.parsed_response_body[:projects].map { |p| p[:id] }).to include id

      resp = automate_api_request("/apis/iam/v2/projects/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/projects")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:projects].length).to eq init_project_count
      expect(resp.parsed_response_body[:projects].map { |p| p[:id] }).to_not include id
    end

    it "GET project responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE project responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test project updated",
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:project][:name]).to eq "inspec test project updated"
    end
  end

  describe "project rules API" do
    context "when the project does not exist" do
      describe "GET /iam/v2/projects/:id/rules" do
        it "returns Not Found" do
          resp = automate_api_request("/apis/iam/v2/projects/project-not-found/rules")
          expect(resp.http_status).to eq 404
          expect(resp.parsed_response_body[:rules]).to eq(nil)
          expect(resp.parsed_response_body[:status]).to eq(nil)
        end
      end
    end

    context "when there are no rules but the project exists" do
      custom_project_id = "inspec-custom-project-rules-test-#{TIMESTAMP}"

      before(:all) do
        resp = automate_api_request("/apis/iam/v2/projects",
         http_method: 'POST',
         request_body: {
           id: custom_project_id,
           name: "display name !#$#"
         }.to_json
       )
       expect(resp.http_status).to eq 200
     end

     after(:all) do
       resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}", http_method: 'DELETE')
       expect(resp.http_status.to_s).to match(/200|404/)
     end

      it "GET /iam/v2/projects/:id/rules returns an empty list" do
        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules")
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rules]).to eq([])
        expect(resp.parsed_response_body[:status]).to eq("NO_RULES")
      end

      it "GET /iam/v2/projects/:project_id/rules/:id returns a 404" do
        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules/not-found")
        expect(resp.http_status).to eq 404
        expect(resp.parsed_response_body[:rule]).to eq(nil)
      end

      it "PUT /iam/v2/projects/:project_id/rules/:id returns a 404" do
        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules/not-found")
        expect(resp.http_status).to eq 404
        expect(resp.parsed_response_body[:rule]).to eq(nil)
      end

      it "DELETE /iam/v2/projects/:project_id/rules/:id returns a 404" do
        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules/not-found", http_method: 'DELETE')
        expect(resp.http_status).to eq 404
        expect(resp.parsed_response_body[:rule]).to eq(nil)
      end

      describe "POST /iam/v2/projects/:project_id/rules" do
        CUSTOM_RULE = {
          id: "custom-rule",
          name: "display name !#$#",
          project_id: custom_project_id,
          type: "NODE",
          conditions: [
            {
              attribute: "CHEF_TAG",
              operator: "MEMBER_OF",
              values: ["tag1", "tag2"]
            }
          ],
          status: "STAGED"
        }

        after(:each) do
          resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE[:project_id]}/rules/#{CUSTOM_RULE[:id]}", http_method: 'DELETE')
          expect(resp.http_status.to_s).to match(/200|404/)
        end

        it "creates a new rule" do
          resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules",
            http_method: 'POST',
            request_body: CUSTOM_RULE.to_json
          )
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:rule]).to eq(CUSTOM_RULE)

          resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE[:project_id]}/rules/#{CUSTOM_RULE[:id]}")
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:rule]).to eq(CUSTOM_RULE)
        end
      end
    end

    context "when there are multiple rules for multiple projects" do
      custom_project_id = "inspec-custom-project-multiple-projects-test-1-#{TIMESTAMP}"
      custom_project_id_2 = "inspec-custom-project-multiple-projects-test-2-#{TIMESTAMP}"

      CUSTOM_RULE_1 = {
        id: "custom-rule-1-#{TIMESTAMP}",
        name: "display name !#$#",
        project_id: custom_project_id,
        type: "NODE",
        conditions: [
          {
            attribute: "CHEF_TAG",
            operator: "MEMBER_OF",
            values: ["tag1", "tag2"]
          }
        ],
        status: "STAGED"
      }

      CUSTOM_RULE_2 = {
        id: "custom-rule-2-#{TIMESTAMP}",
        name: "display name !#$#",
        project_id: custom_project_id,
        type: "EVENT",
        conditions: [
          {
            attribute: "CHEF_SERVER",
            operator: "EQUALS",
            values: ["server1"]
          }
        ],
        status: "STAGED"
      }

      CUSTOM_RULE_3 = {
        id: "custom-rule-3-#{TIMESTAMP}",
        name: "display name !#$#",
        project_id: custom_project_id_2,
        type: "NODE",
        conditions: [
          {
            attribute: "CHEF_ORGANIZATION",
            operator: "EQUALS",
            values: ["org1"]
          }
        ],
        status: "STAGED"
      }

      before(:all) do
        resp = automate_api_request("/apis/iam/v2/projects",
          http_method: 'POST',
          request_body: {
            id: custom_project_id_2,
            name: "display name !#$#"
          }.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/projects",
          http_method: 'POST',
          request_body: {
            id: custom_project_id,
            name: "display name !#$#"
          }.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules",
          http_method: 'POST',
          request_body: CUSTOM_RULE_1.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules",
          http_method: 'POST',
          request_body: CUSTOM_RULE_2.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id_2}/rules",
          http_method: 'POST',
          request_body: CUSTOM_RULE_3.to_json
        )
        expect(resp.http_status).to eq 200
      end

      after(:all) do
        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE[:project_id]}/rules/#{CUSTOM_RULE[:id]}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_2[:project_id]}/rules/#{CUSTOM_RULE_2[:id]}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_3[:project_id]}/rules/#{CUSTOM_RULE_3[:id]}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id_2}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

      end

      it "GET /iam/v2/projects/:project_id/rules/:id returns a specific rule" do
        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_1[:project_id]}/rules/#{CUSTOM_RULE_1[:id]}")
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rule]).to eq(CUSTOM_RULE_1)
      end

      it "GET /iam/v2/projects/:id/rules returns any staged rules and applied rules with no staged changes for the project" do
        updated_rule = {
          id: CUSTOM_RULE_2[:id],
          name: "updated display name",
          project_id: CUSTOM_RULE_2[:project_id],
          type: "EVENT",
          conditions: [
            {
              attribute: "CHEF_SERVER",
              operator: "EQUALS",
              values: ["brand new server"]
            }
           ],
           status: "STAGED"
         }

        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_2[:project_id]}/rules/#{CUSTOM_RULE_2[:id]}",
          http_method: 'PUT',
          request_body: updated_rule.to_json
        )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rule]).to eq(updated_rule)

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules")
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rules]).to match_array([CUSTOM_RULE_1, updated_rule])
        expect(resp.parsed_response_body[:status]).to eq("EDITS_PENDING")
      end

      it "PUT /iam/v2/projects/:project_id/rules/:id updates the rule" do
        updated_rule = {
          id: CUSTOM_RULE_1[:id],
          name: "updated display name",
          project_id: CUSTOM_RULE_1[:project_id],
          type: "NODE",
          conditions: [
            {
              attribute: "CHEF_TAG",
              operator: "EQUALS",
              values: ["new tag"]
            }
           ],
           status: "STAGED"
         }

        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_1[:project_id]}/rules/#{CUSTOM_RULE_1[:id]}",
          http_method: 'PUT',
          request_body: updated_rule.to_json
        )
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rule]).to eq(updated_rule)

        resp = automate_api_request("/apis/iam/v2/projects/#{CUSTOM_RULE_1[:project_id]}/rules/#{CUSTOM_RULE_1[:id]}")
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rule]).to eq(updated_rule)
      end

      it "DELETE /iam/v2/projects/:project_id/rules/:id deletes the specific rule" do
        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules/#{CUSTOM_RULE_1[:id]}", http_method: 'DELETE')
        expect(resp.http_status).to eq 200
        expect(resp.parsed_response_body[:rule]).to eq(nil)

        resp = automate_api_request("/apis/iam/v2/projects/#{custom_project_id}/rules/#{CUSTOM_RULE_1[:id]}")
        expect(resp.http_status).to eq 404
      end
    end
  end

  describe 'policies API' do

    before(:all) do
      resp = automate_api_request("/apis/iam/v2/roles",
        http_method: 'POST',
        request_body: {
          id: ROLE_ID,
          name: "display name !#$#",
          actions: ["test:some:action", "test:other:action"]
        }.to_json
      )
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/policies",
        http_method: 'POST',
        request_body: {
          id: POLICY_ID,
          name: 'gotta catch em all',
          members: ["user:local:member"],
          statements: [
            {
              effect: "DENY",
              role: ROLE_ID,
              projects: ["*"]
            },
            {
              effect: "ALLOW",
              actions: ["test:some:action"],
              projects: ["*"]
            }
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/roles/#{ROLE_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    describe 'list default policies' do 
      it 'includes the IAM v2 default policies' do
        resp = automate_api_request('/apis/iam/v2/policies')
        expect(resp.http_status).to eq 200
  
        policy_ids = resp.parsed_response_body[:policies].map { |u| u[:id] }
        expect(policy_ids).to include("administrator-access")
        expect(policy_ids).to include("ingest-access")
        expect(policy_ids).to include("editor-access")
        expect(policy_ids).to include("viewer-access")
      end
  
      it 'the editors default policy includes editor role' do
        resp = automate_api_request('/apis/iam/v2/policies')
        expect(resp.http_status).to eq 200
  
        all_policies = resp.parsed_response_body[:policies]
        policies = all_policies.select{ |p| /^team:local:viewers$/.match(p[:members][0]) }
        expect(policies.length).to eq 1
      end
  
      it 'the viewers default policy includes editor role' do
        resp = automate_api_request('/apis/iam/v2/policies')
        expect(resp.http_status).to eq 200
  
        all_policies = resp.parsed_response_body[:policies]
        policies = all_policies.select{ |p| /^team:local:editors$/.match(p[:members][0]) }
        expect(policies.length).to eq 1
      end
    end

    # Note: all endpoints provided by grpc-gateway satisfy this. We're using
    # this specific test case as a canary.
    # Also note that we set up a test policy here that has at most one element
    # in each embedded array, to sidestep ordering issues.
    it "supports ?pretty for enabling pretty-printed JSON responses" do
      id = "inspec-test-policy-0-#{TIMESTAMP}"
      resp = automate_api_request("/apis/iam/v2/policies",
        http_method: 'POST',
        request_body: {
          id: id,
          name: 'Name',
          members: ["user:local:member"],
          statements: [
            {
              effect: "DENY",
              role: ROLE_ID,
              projects: ["*"]
            },
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 200
      resp = automate_api_request("/apis/iam/v2/policies/#{id}?pretty")
      expect(resp.raw_response_body).to eq <<EOF.chomp
{
  "policy": {
    "name": "Name",
    "id": "#{id}",
    "type": "CUSTOM",
    "members": [
      "user:local:member"
    ],
    "statements": [
      {
        "effect": "DENY",
        "actions": [
        ],
        "role": "#{ROLE_ID}",
        "resources": [
          "*"
        ],
        "projects": [
          "*"
        ]
      }
    ],
    "projects": [
    ]
  }
}
EOF

      resp = automate_api_request("/apis/iam/v2/policies/#{id}", http_method: "DELETE")
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE policy properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies")
      expect(resp.http_status).to eq 200
      init_policy_count = resp.parsed_response_body[:policies].length

      id = "inspec-test-policy-1-#{TIMESTAMP}"
      resp = automate_api_request("/apis/iam/v2/policies",
        http_method: 'POST',
        request_body: {
          id: id,
          name: 'brand new name',
          members: ["user:local:member", "team:local:member"],
          statements: [
            {
              effect: "DENY",
              role: ROLE_ID,
              projects: ["*"]
            },
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:policy][:id]).to eq id
      expect(resp.parsed_response_body[:policy][:name]).to eq 'brand new name'
      expect(resp.parsed_response_body[:policy][:members].length).to eq 2
      expect(resp.parsed_response_body[:policy][:statements].length).to eq 1
      expect(resp.parsed_response_body[:policy][:statements].first[:resources].length).to eq 1
      expect(resp.parsed_response_body[:policy][:statements].first[:resources]).to eq ["*"]

      resp = automate_api_request("/apis/iam/v2/policies")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:policies].length).to eq init_policy_count + 1

      resp = automate_api_request("/apis/iam/v2/policies/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2/policies")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:policies].length).to eq init_policy_count
    end

    it "CREATE policy with resources fails with error" do
      id = "inspec-test-policy-1-#{TIMESTAMP}"
      resp = automate_api_request("/apis/iam/v2/policies",
        http_method: 'POST',
        request_body: {
          id: id,
          name: 'brand new name',
          members: ["user:local:member", "team:local:member"],
          statements: [
            {
              effect: "DENY",
              resources: ["compliance:foo1","compliance:bar1"],
              role: ROLE_ID,
              projects: ["*"]
            },
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 400
      expect(resp.parsed_response_body[:error]).to eq "could not parse statements: cannot define resources on v2 policy"
    end

    it "LIST policies responds properly" do
      resp = automate_api_request("/apis/iam/v2/policies")
      expect(resp.http_status).to eq 200
    end

    it "GET policy responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}")
      expect(resp.http_status).to eq 200
    end

    it "GET policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
    end

    it "PUT (replace) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members",
        http_method: 'PUT',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2", "team:local:newmember3"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3

      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
    end

    it "POST (remove) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members:remove",
        http_method: 'POST',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
    end

    it "POST (add) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members:add",
        http_method: 'POST',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
    end

    it "UPDATE policy responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2/policies/#{POLICY_ID}",
        http_method: 'PUT',
        request_body: {
          name: "updated policy!",
          statements: [
            {
              effect:    "ALLOW",
              actions:   ["compliance:profiles:upload"],
              projects: ["*"]
            }
          ],
          members: []
        }.to_json
      )
      expect(resp.http_status).to eq 200

      expect(resp.parsed_response_body[:policy][:id]).to eq POLICY_ID
      expect(resp.parsed_response_body[:policy][:name]).to eq 'updated policy!'
      expect(resp.parsed_response_body[:policy][:members].length).to eq 0
    end
  end
end


