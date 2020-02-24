# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'iam REST API integration tests'

# TODO port this test to Cypress integration/api tests

control 'iam-1' do
  title 'iam endpoints'
  desc 'Verify behavior of all the endpoints under the "iam" namespace'

  TOKEN_ID = 'iam-1-token'
  TOKEN_NAME = 'iam REST API integration test token'
  
  describe "tokens API" do 
    after(:all) do
      resp = automate_api_request("/apis/iam/v2/policies/#{TOKEN_ID}", http_method: 'DELETE')
    end
    
    describe "POST /iam/v2/tokens" do
  
      it "returns a token ID and value" do
        response = automate_api_request("/apis/iam/v2/tokens",
                     http_method: 'POST',
                     request_body: {
                       id: TOKEN_ID,
                       name: TOKEN_NAME
                     }.to_json)
  
        expect(response.http_status).to eq 200
        [:id, :name, :value, :active, :created_at, :updated_at, :projects].each do |key|
          expect(response.parsed_response_body[:token].keys).to include key
          expect(response.parsed_response_body[:token][key]).not_to eq ''
        end
        expect(response.parsed_response_body[:token][:id]).to eq TOKEN_ID
        expect(response.parsed_response_body[:token][:name]).to eq TOKEN_NAME
        expect(response.parsed_response_body[:token][:active]).to eq true
        expect(response.parsed_response_body[:token][:projects]).to eq []
      end
  
      it "allows you to specify the ID and returns a 409 on conflict" do
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
    end
  
    describe "GET /iam/v2/tokens" do
  
      it "returns all the tokens, including the new one" do
        response = automate_api_request("/apis/iam/v2/tokens")
        expect(response.http_status).to eq 200
        found = response.parsed_response_body[:tokens].any? do |t|
          t[:id] == TOKEN_ID
        end
        expect(found).to eq true
      end
    end
  
    describe "GET /iam/v2/tokens/:id" do
  
      it "returns the new token" do
        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
        expect(response.http_status).to eq 200
        expect(response.parsed_response_body[:token][:id]).to eq TOKEN_ID
        expect(response.parsed_response_body[:token][:name]).to eq TOKEN_NAME
        expect(response.parsed_response_body[:token][:active]).to eq true
        expect(response.parsed_response_body[:token][:projects]).to eq []
      end
  
      it "returns 404 Not Found if the token does not exist" do
        response = automate_api_request("/apis/iam/v2/tokens/not-a-real-token-nope")
        expect(response.http_status).to eq 404
      end
    end
  
    describe "PUT /iam/v2/tokens/:id" do
      it "updates the token" do
        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
                                        http_method: 'PUT',
                                        request_body: {
                                          active: false
                                        }.to_json)
        expect(response.http_status).to eq 200
        expect(response.parsed_response_body[:token][:active]).to eq false
  
        tok = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}")
        expect(response.http_status).to eq 200
        expect(response.parsed_response_body[:token][:active]).to eq false
      end
  
          it "returns 404 Not Found if the token does not exist" do
        response = automate_api_request("/apis/iam/v2/tokens/not-a-real-token-nope",
                                        http_method: 'PUT',
                                        request_body: {
                                          active: false
                                        }.to_json)
        expect(response.http_status).to eq 404
      end
    end
  
    describe "DELETE /iam/v2/tokens/:id" do
  
      it "returns 200 OK" do
        response = automate_api_request("/apis/iam/v2/tokens/#{TOKEN_ID}",
                     http_method: 'DELETE')
        expect(response.http_status).to eq 200
      end
  
      it "returns 404 Not Found" do
        response = automate_api_request("/apis/iam/v2/tokens/not-a-real-token-nope",
                     http_method: 'DELETE')
        expect(response.http_status).to eq 404
        expect(response.parsed_response_body[:error]).to eq "No token record found"
      end
    end
  end
end
