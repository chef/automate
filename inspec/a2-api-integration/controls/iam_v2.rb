# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved
title 'IAM v2 REST API integration tests'

# (tc) Happy path tests. I think we should put more thought into how
# to integration test our APIs. I don't think inspec is the right
# tool for the job. Let's follow up there. We aren't shipping to customers
# so it's low risk to merge as is but I wanted some basic testing while
# we figure out what we wanna do long-term.
control 'iam-v2-1' do
  title 'IAM v2 endpoints'

  # no default v2 policies pre-migration
  CUSTOM_POLICY_ID = 'inspec-custom-policy'
  CUSTOM_ROLE_ID = 'inspec-custom-role'
  CUSTOM_PROJECT_ID = 'inspec-custom-project'
  CUSTOM_TOKEN_ID = 'inspec-token'
  CUSTOM_TEAM_ID = 'inspec-team'
  CUSTOM_USER_ID = 'inspec-user'
  ADMIN_USER_ID = 'admin'

  describe 'v2beta policy API' do
    before(:all) do
      resp = automate_api_request("/apis/iam/v2beta/policies",
        http_method: 'POST',
        request_body: {
          id: CUSTOM_POLICY_ID,
          name: 'gotta catch em all',
          members: ["user:local:member"],
          statements: [
            {
              effect: "DENY",
              role: "test"
            },
            {
              effect: "ALLOW",
              actions: ["test:some:action"]
            }
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE policy properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies")
      expect(resp.http_status).to eq 200
      init_policy_count = resp.parsed_response_body[:policies].length

      id = "inspec-test-policy-1-#{Time.now.utc.to_i}"
      resp = automate_api_request("/apis/iam/v2beta/policies",
        http_method: 'POST',
        request_body: {
          id: id,
          name: 'brand new name',
          members: ["user:local:member", "team:local:member"],
          statements: [
            {
              effect: "DENY",
              role: "test"
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

      resp = automate_api_request("/apis/iam/v2beta/policies")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:policies].length).to eq init_policy_count + 1

      resp = automate_api_request("/apis/iam/v2beta/policies/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2beta/policies")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:policies].length).to eq init_policy_count
    end

    it "CREATE policy with resources fails with error" do
      id = "inspec-test-policy-1-#{Time.now.utc.to_i}"
      resp = automate_api_request("/apis/iam/v2beta/policies",
        http_method: 'POST',
        request_body: {
          id: id,
          name: 'brand new name',
          members: ["user:local:member", "team:local:member"],
          statements: [
            {
              effect: "DENY",
              resources: ["compliance:foo1","compliance:bar1"],
              role: "test"
            },
          ]
        }.to_json()
      )
      expect(resp.http_status).to eq 400
      expect(resp.parsed_response_body[:error]).to eq "could not parse statements: cannot define resources on v2 policy"
    end

    it "LIST policies responds properly" do
      resp = automate_api_request("/apis/iam/v2beta/policies")
      expect(resp.http_status).to eq 200
    end

    it "GET policy responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}")
      expect(resp.http_status).to eq 200
    end

    it "GET policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
    end

    it "PUT (replace) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members",
        http_method: 'PUT',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2", "team:local:newmember3"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3

      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
    end

    it "POST (remove) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members:remove",
        http_method: 'POST',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 1
    end

    it "POST (add) policy members responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members:add",
        http_method: 'POST',
        request_body: {
          members: ["user:local:newmember1", "team:local:newmember2"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}/members")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:members].length).to eq 3
    end

    it "UPDATE policy responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/policies/#{CUSTOM_POLICY_ID}",
        http_method: 'PUT',
        request_body: {
          name: "updated policy!",
          members: []
        }.to_json
      )
      expect(resp.http_status).to eq 200

      expect(resp.parsed_response_body[:policy][:id]).to eq CUSTOM_POLICY_ID
      expect(resp.parsed_response_body[:policy][:name]).to eq 'updated policy!'
      expect(resp.parsed_response_body[:policy][:members].length).to eq 0
    end
  end

  describe "v2beta role API" do
    before(:all) do
       resp = automate_api_request("/apis/iam/v2beta/roles",
        http_method: 'POST',
        request_body: {
          id: CUSTOM_ROLE_ID,
          name: "display name !#$#",
          actions: ["test:some:action", "test:other:action"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2beta/roles/#{CUSTOM_ROLE_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE role properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/roles")
      expect(resp.http_status).to eq 200
      init_role_count = resp.parsed_response_body[:roles].length

      id = "inspec-role-#{Time.now.utc.to_i}"
      resp = automate_api_request("/apis/iam/v2beta/roles",
        http_method: 'POST',
        request_body: {
          id: id,
          name: "display name !#$#",
          actions: ["test:some:action", "test:other:action"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:role][:id]).to eq id
      expect(resp.parsed_response_body[:role][:name]).to eq 'display name !#$#'
      expect(resp.parsed_response_body[:role][:actions].length).to eq 2

      resp = automate_api_request("/apis/iam/v2beta/roles")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to eq init_role_count + 1

      resp = automate_api_request("/apis/iam/v2beta/roles/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2beta/roles")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:roles].length).to eq init_role_count
    end

    it "LIST roles responds properly" do
      resp = automate_api_request("/apis/iam/v2beta/roles")
      expect(resp.http_status).to eq 200
    end

    it "GET role responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/roles/#{CUSTOM_ROLE_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE role responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/roles/#{CUSTOM_ROLE_ID}",
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
  end

  describe "v2beta project API" do
    before(:all) do
       resp = automate_api_request("/apis/iam/v2beta/projects",
        http_method: 'POST',
        request_body: {
          id: CUSTOM_PROJECT_ID,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2beta/projects/#{CUSTOM_PROJECT_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "LIST projects responds properly" do
      resp = automate_api_request("/apis/iam/v2beta/projects")
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE project properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/projects")
      expect(resp.http_status).to eq 200
      init_project_count = resp.parsed_response_body[:projects].length

      id = "inspec-project-#{Time.now.utc.to_i}"
      resp = automate_api_request("/apis/iam/v2beta/projects",
        http_method: 'POST',
        request_body: {
          id: id,
          name: "display name !#$#"
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:project][:id]).to eq id
      expect(resp.parsed_response_body[:project][:name]).to eq 'display name !#$#'
      expect(resp.parsed_response_body[:project][:projects]).to eq [id]

      resp = automate_api_request("/apis/iam/v2beta/projects")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:projects].length).to eq init_project_count + 1
      expect(resp.parsed_response_body[:projects].map { |p| p[:id] }).to include id

      resp = automate_api_request("/apis/iam/v2beta/projects/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2beta/projects")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:projects].length).to eq init_project_count
      expect(resp.parsed_response_body[:projects].map { |p| p[:id] }).to_not include id
    end

    it "GET project responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/projects/#{CUSTOM_PROJECT_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE project responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/projects/#{CUSTOM_PROJECT_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test project updated",
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:project][:name]).to eq "inspec test project updated"
    end
  end

  describe "v2beta token API" do
    before(:all) do
       resp = automate_api_request("/apis/iam/v2beta/tokens",
        http_method: 'POST',
        request_body: {
          id: CUSTOM_TOKEN_ID,
          name: "my cool token",
          projects: ["project-random"],
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2beta/tokens/#{CUSTOM_TOKEN_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "CREATE and DELETE token responds as expected" do
      resp = automate_api_request("/apis/iam/v2beta/tokens")
      expect(resp.http_status).to eq 200
      init_token_count = resp.parsed_response_body[:tokens].length

      id = "inspec-token-#{Time.now.utc.to_i}"
      resp = automate_api_request("/apis/iam/v2beta/tokens",
        http_method: 'POST',
        request_body: {
          id: id,
          name: "my neat token",
          active: true,
          projects: ["project-1"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:id]).to eq id
      expect(resp.parsed_response_body[:token][:name]).to eq 'my neat token'
      expect(resp.parsed_response_body[:token][:active]).to be true
      expect(resp.parsed_response_body[:token][:projects]).to eq ["project-1"]

      resp = automate_api_request("/apis/iam/v2beta/tokens")
      expect(resp.parsed_response_body[:tokens].length).to eq init_token_count + 1
      expect(resp.parsed_response_body[:tokens].map {|t| t[:id]}).to include(id)

      resp = automate_api_request("/apis/iam/v2beta/tokens/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2beta/tokens")
      expect(resp.parsed_response_body[:tokens].length).to eq init_token_count
      expect(resp.parsed_response_body[:tokens].map {|t| t[:id]}).to_not include(id)
    end

    it "LIST tokens returns list of tokens" do
      resp = automate_api_request("/apis/iam/v2beta/tokens")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body.keys).to include (:tokens)
    end

    it "GET token responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/tokens/#{CUSTOM_TOKEN_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE token responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/tokens/#{CUSTOM_TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test token updated",
          projects: ["project-1", "my-new-project"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:name]).to eq "inspec test token updated"
      expect(resp.parsed_response_body[:token][:projects]).to eq ["project-1", "my-new-project"]
    end

    it "UPDATE token succeeds if update does not include projects" do
      resp = automate_api_request("/apis/iam/v2beta/tokens/#{CUSTOM_TOKEN_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test token updated",
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:token][:projects]).to eq []
    end

    it "CREATE token succeeds if projects are empty" do
      resp = automate_api_request("/apis/iam/v2beta/tokens",
        http_method: 'POST',
        request_body: {
          id: 'project-is-required-will-not-be-created',
          name: "inspec test token create missing projects",
          projects: []
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end
  end

  describe "v2beta users API" do
    TEST_USER = {
        id: CUSTOM_USER_ID,
        name: "display name !#$#",
        password: "chefautomate"
    }

    describe "when the user is not yet created" do
      CREATED_ID = "unique-user-id"

      after(:each) do
        resp = automate_api_request("/apis/iam/v2beta/users/#{CREATED_ID}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)
      end

      describe "POST /iam/v2beta/users/:id" do
        it "creates the user" do
          createdName =  "i created my own name"
          resp = automate_api_request("/apis/iam/v2beta/users",
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
        resp = automate_api_request("/apis/iam/v2beta/users",
          http_method: 'POST',
          request_body: TEST_USER.to_json
        )
        expect(resp.http_status).to eq 200

        resp = automate_api_request("/apis/iam/v2beta/users",
          http_method: 'POST',
          request_body: custom_user_2.to_json
        )
        expect(resp.http_status).to eq 200
      end

      after(:each) do
        resp = automate_api_request("/apis/iam/v2beta/users/#{CUSTOM_USER_ID}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)

        resp = automate_api_request("/apis/iam/v2beta/users/#{custom_user_id_2}", http_method: 'DELETE')
        expect(resp.http_status.to_s).to match(/200|404/)
      end

      describe "GET /iam/v2beta/users/" do
        it "returns the list of users" do
          resp = automate_api_request("/apis/iam/v2beta/users")
          expect(resp.http_status).to eq 200
          expect(resp.parsed_response_body[:users].length).to eq 3
          expect(resp.parsed_response_body[:users].map { |u| u[:id] })
            .to match_array([custom_user_id_2, CUSTOM_USER_ID, ADMIN_USER_ID])
        end
      end

      describe "PUT /apis/iam/v2beta/self/:id" do
        it "user can update their own display name" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2beta/self/#{CUSTOM_USER_ID}",
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
          resp = automate_api_request("/apis/iam/v2beta/self/#{CUSTOM_USER_ID}",
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
          resp = automate_api_request("/apis/iam/v2beta/self/some_wrong_id",
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
          resp = automate_api_request("/apis/iam/v2beta/self/#{CUSTOM_USER_ID}",
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

      describe "DELETE /iam/v2beta/users/:id" do
        it "deletes the user if it exists" do
          resp = automate_api_request("/apis/iam/v2beta/users/#{CUSTOM_USER_ID}")
          expect(resp.http_status).to eq 200

          resp = automate_api_request("/apis/iam/v2beta/users/#{CUSTOM_USER_ID}", http_method: 'DELETE')
          expect(resp.http_status).to eq 200

          resp = automate_api_request("/apis/iam/v2beta/users/#{CUSTOM_USER_ID}")
          expect(resp.http_status).to eq 404
        end

        it "user gets a 404 when the user does not exist" do
          resp = automate_api_request("/apis/iam/v2beta/users/some_wrong_id", http_method: 'DELETE')
          expect(resp.http_status).to eq 404
        end
      end

      describe "PUT /iam/v2beta/users/:id" do
        it "updates the user if it exists" do
          updatedName =  "i updated my own name"
          resp = automate_api_request("/apis/iam/v2beta/users/#{CUSTOM_USER_ID}",
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
          resp = automate_api_request("/apis/iam/v2beta/users/some_wrong_id",
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

  describe "v2beta team API" do
    before(:all) do
       resp = automate_api_request("/apis/iam/v2beta/teams",
        http_method: 'POST',
        request_body: {
          id: CUSTOM_TEAM_ID,
          name: "display name !#$#",
          projects: ['a-project']
        }.to_json
      )
      expect(resp.http_status).to eq 200
    end

    after(:all) do
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200
    end

    it "LIST teams responds properly" do
      resp = automate_api_request("/apis/iam/v2beta/teams")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body.keys).to include(:teams)
    end

    it "CREATE and DELETE team properly respond to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/teams")
      expect(resp.http_status).to eq 200
      init_team_count = resp.parsed_response_body[:teams].length

      id = "inspec-team-#{Time.now.utc.to_i}"
      projects = ["another-project"]
      resp = automate_api_request("/apis/iam/v2beta/teams",
        http_method: 'POST',
        request_body: {
          id: id,
          name: "display name !#$#",
          projects: projects
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:id]).to eq id
      expect(resp.parsed_response_body[:team][:name]).to eq 'display name !#$#'
      expect(resp.parsed_response_body[:team][:projects]).to eq projects

      resp = automate_api_request("/apis/iam/v2beta/teams")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:teams].length).to eq init_team_count + 1
      expect(resp.parsed_response_body[:teams].map { |p| p[:id] }).to include id

      resp = automate_api_request("/apis/iam/v2beta/teams/#{id}", http_method: 'DELETE')
      expect(resp.http_status).to eq 200

      resp = automate_api_request("/apis/iam/v2beta/teams")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:teams].length).to eq init_team_count
      expect(resp.parsed_response_body[:teams].map { |p| p[:id] }).to_not include id
    end

    it "GET team responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}")
      expect(resp.http_status).to eq 200
    end

    it "UPDATE team responds properly to happy path inputs" do
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:name]).to eq "display name !#$#"

      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}",
        http_method: 'PUT',
        request_body: {
          name: "inspec test team updated",
          projects: ["a-project"]
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:team][:name]).to eq "inspec test team updated"
    end

    it "POST (add) and POST (remove) team members responds properly to happy path inputs" do
      user_id_1 = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      user_id_2 = "93a2fe1f-0793-4cbe-9b4b-737a9316a515"
      users = [user_id_1, user_id_2]
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: users,
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 2
      expect(resp.parsed_response_body[:user_ids]).to cmp(users)

      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 2

      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users:remove",
        http_method: 'POST',
        request_body: {
          user_ids: [user_id_2],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids].length).to eq 1

      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq [user_id_1]
    end

    it "GET teams for user responds properly to happy path inputs" do
      user = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: [user],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])

      resp = automate_api_request("/apis/iam/v2beta/users/#{user}/teams")
      expect(resp.http_status).to eq 200
    end

    it "GET users for team responds properly to happy path inputs" do
      user = "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users:add",
        http_method: 'POST',
        request_body: {
          user_ids: [user],
        }.to_json
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])

      resp = automate_api_request("/apis/iam/v2beta/teams/#{CUSTOM_TEAM_ID}/users")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:user_ids]).to eq([user])
    end
  end
end
