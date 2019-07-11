# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

TEST_POLICY_ACTION_PREFIX = "policytestaction"

title 'authz-service REST API integration tests'

control 'authz-api-crud-1' do
  title 'AuthZ CRUD operations'
  desc 'Verifies CRUD operations on the authz api'

  describe "policies API" do
    after do
      all_policies = automate_api_request(
        "/api/v0/auth/policies",
        http_method: 'GET'
      )

      filtered_policies = []
      all_policies.parsed_response_body[:policies].each do |policy|
        if policy[:action].start_with?(TEST_POLICY_ACTION_PREFIX)
          filtered_policies.push(policy)
        end
      end

      filtered_policies.each do |policy|
        if policy[:action].start_with?(TEST_POLICY_ACTION_PREFIX)
          automate_api_request(
            "/api/v0/auth/policies/#{policy[:id]}",
            http_method: 'DELETE'
          ).http_status
        end
      end
    end

    let(:create_request) do
      automate_api_request(
        '/api/v0/auth/policies',
        http_method: 'POST',
        request_body: policy_body
      )
    end

    let(:list_request) do
      automate_api_request(
        "/api/v0/auth/policies",
        http_method: 'GET'
      )
    end

    let(:delete_request) do
      automate_api_request(
        "/api/v0/auth/policies/#{create_request.parsed_response_body[:id]}",
        http_method: 'DELETE'
      )
    end

    let(:policy_hash) do
      hash = {}
      hash[:action]   = policy_action   unless policy_action.nil?
      hash[:subjects] = policy_subjects unless policy_subjects.nil?
      hash[:resource] = policy_resource unless policy_resource.nil?
      hash
    end

    let(:policy_body) do
      policy_hash.to_json
    end

    let(:policy_action) do
      "#{TEST_POLICY_ACTION_PREFIX}#{SecureRandom.hex.tr('0-9', '')}"
    end

    let(:policy_subjects) do
      ["team:local:team1", "team:local:team2"]
    end

    let(:policy_resource) do
      "test:resource"
    end

    let(:filtered_list) do
      filtered_policies = []
      list_request.parsed_response_body[:policies].each do |policy|
        if policy[:action].start_with?(TEST_POLICY_ACTION_PREFIX)
          filtered_policies.push(policy)
        end
      end
      filtered_policies
    end

    let(:policy_action2) do
      # Note: actions have to match ^[a-z]+$ or ^\*$ (wildcard), so, this is a
      # lame way of getting something random-ish that still matches
      "#{TEST_POLICY_ACTION_PREFIX}#{SecureRandom.hex.tr('0-9', '')}"
    end

    let(:policy_subjects2) do
      ["team:local:team3", "team:local:team4"]
    end

    let(:policy_resource2) do
      "test:resource2"
    end

    let(:create_request2) do
      automate_api_request(
        '/api/v0/auth/policies',
        http_method: 'POST',
        request_body: {
          action: policy_action2,
          subjects: policy_subjects2,
          resource: policy_resource2,
        }.to_json
      )
    end

    describe "when creating a policy" do
      shared_examples "when a field is missing" do
        it "fails with a 400" do
          expect(create_request.http_status).to eq 400
          expect(create_request.parsed_response_body[:error] || "").not_to eq("")
        end
      end

      context "when the action field is not specified" do
        let(:policy_action) { nil }
        include_examples "when a field is missing"
      end

      context "when the action field is empty" do
        let(:policy_action) { '' }
        include_examples "when a field is missing"
      end

      context "when the subjects field is not specified" do
        let(:policy_subjects) { nil }
        include_examples "when a field is missing"
      end

      context "when a subjects array is empty" do
        let(:policy_subjects) { [] }
        include_examples "when a field is missing"
      end

      context "when the resource field is not specified" do
        let(:policy_resource) { nil }
        include_examples "when a field is missing"
      end

      context "when the resource field is empty" do
        let(:policy_resource) { '' }
        include_examples "when a field is missing"
      end

      context "when the request is valid" do
        it "returns the created policy with 200" do
          expect(create_request.http_status).to eq(200)
          expect(create_request.parsed_response_body[:action]).to eq(policy_action)
          expect(create_request.parsed_response_body[:subjects]).to eq(policy_subjects)
          expect(create_request.parsed_response_body[:resource]).to eq(policy_resource)
          expect(create_request.parsed_response_body[:effect]).to eq("allow")
          expect(create_request.parsed_response_body[:id]).to_not be_nil
          expect(create_request.parsed_response_body[:created_at]).to_not be_nil
          expect(create_request.parsed_response_body[:updated_at]).to be_nil
        end
      end
    end

    describe "when listing policies" do
      context "when there are no policies" do
        it "returns the empty list with with 200" do
          expect(list_request.http_status).to eq(200)
          expect(filtered_list).to eq([])
        end
      end

      context "when there is a single policy" do
        before do
          expect(create_request.http_status).to eq(200)
        end

        it "returns the empty list with with 200" do
          expect(list_request.http_status).to eq(200)
          expect(filtered_list.length).to eq(1)
          expect(filtered_list[0][:action]).to eq(policy_action)
          expect(filtered_list[0][:subjects]).to eq(policy_subjects)
          expect(filtered_list[0][:resource]).to eq(policy_resource)
          expect(filtered_list[0][:effect]).to eq("allow")
          expect(filtered_list[0][:id]).to_not be_nil
          expect(filtered_list[0][:created_at]).to_not be_nil
        end
      end

      context "when there is are multiple policies" do
        before do
          expect(create_request.http_status).to eq(200)
          expect(create_request2.http_status).to eq(200)
        end

        it "returns the empty list with with 200" do
          expect(list_request.http_status).to eq(200)
          expect(filtered_list.length).to eq(2)

          expect(filtered_list.any? { |policy|
            policy[:action] == policy_action &&
            policy[:subjects] == policy_subjects &&
            policy[:resource] == policy_resource &&
            policy[:effect] == "allow" &&
            !policy[:id].nil? &&
            !policy[:created_at].nil? &&
            policy[:updated_at].nil?
          }).to be(true)

          expect(filtered_list.any? { |policy|
            policy[:action] == policy_action2 &&
            policy[:subjects] == policy_subjects2 &&
            policy[:resource] == policy_resource2 &&
            policy[:effect] == "allow" &&
            !policy[:id].nil? &&
            !policy[:created_at].nil? &&
            policy[:updated_at].nil?
          }).to be(true)
        end
      end
    end

    describe "when deleting policies" do
      shared_examples "delete fails with a 404" do
        let(:delete_request) do
          automate_api_request(
            "/api/v0/auth/policies/db301683-6183-4209-bc25-7875a117e06c",
            http_method: 'DELETE'
          )
        end

        it "fails with a 404" do
          expect(delete_request.http_status).to eq(404)
          expect(delete_request.parsed_response_body[:error] || "").not_to eq("")
          expect(filtered_list.length).to eq(list_length)
        end
      end

      context "when no policies exist" do
        let(:list_length) { 0 }
        include_examples "delete fails with a 404"
      end

      context "when policies exist" do
        before do
          expect(create_request.http_status).to eq(200)
          expect(create_request2.http_status).to eq(200)
        end

        let(:list_length) { 2 }

        context "when we pass an invalid id" do
          include_examples "delete fails with a 404"
        end

        context "when we pass an unparsable id" do
          let(:delete_request) do
            automate_api_request(
              "/api/v0/auth/policies/not_a_uuid",
              http_method: 'DELETE'
            )
          end

          it "fails with a 400" do
            expect(delete_request.http_status).to eq(400)
            expect(delete_request.parsed_response_body[:error] || "").not_to eq("")
            expect(filtered_list.length).to eq(2)
          end
        end

        context "when we pass a valid id" do
          it "returns 200 and deleted the policy from the list" do
            expect(delete_request.http_status).to eq(200)
            expect(delete_request.parsed_response_body[:action]).to eq(policy_action)
            expect(delete_request.parsed_response_body[:subjects]).to eq(policy_subjects)
            expect(delete_request.parsed_response_body[:resource]).to eq(policy_resource)
            expect(delete_request.parsed_response_body[:effect]).to eq("allow")
            expect(delete_request.parsed_response_body[:id]).to_not be_nil
            expect(delete_request.parsed_response_body[:created_at]).to_not be_nil
            expect(filtered_list.length).to eq(1)
          end
        end
      end
    end
  end
end
