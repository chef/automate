# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved
title 'auth REST API integration tests'

control 'auth-1' do
  title 'auth endpoints'
  desc 'Verify behavior of all the endpoints under the "auth" namespace'

  token_id = ''

  describe "POST /auth/tokens" do

    it "returns a token ID and value" do
      response = automate_api_request("/api/v0/auth/tokens",
                   http_method: 'POST',
                   request_body: {
                     description: 'authn-1 client token',
                     active: true
                   }.to_json)
      expect(response.http_status).to eq 200
      [:value, :id, :description, :created, :updated].each do |key|
        expect(response.parsed_response_body.keys).to include key
        expect(response.parsed_response_body[key]).not_to eq ''
      end
      token_id = response.parsed_response_body[:id]
    end

    it "allows you to specify the ID and returns a 409 on conflict" do
      response = automate_api_request("/api/v0/auth/tokens",
                   http_method: 'POST',
                   request_body: {
                     id: 'test-token',
                     description: 'we specified the id',
                     active: true
                   }.to_json)
      expect(response.http_status).to eq 200
      [:value, :id, :description, :created, :updated].each do |key|
        expect(response.parsed_response_body.keys).to include key
        expect(response.parsed_response_body[key]).not_to eq ''
      end

      response2 = automate_api_request("/api/v0/auth/tokens",
                    http_method: 'POST',
                    request_body: {
                      id: 'test-token',
                      description: 'this will conflict',
                      active: true
                    }.to_json)
      expect(response2.http_status).to eq 409
    end
  end

  describe "GET /auth/tokens" do

    it "returns all the tokens, including the new one" do
      response = automate_api_request("/api/v0/auth/tokens")
      expect(response.http_status).to eq 200
      found = response.parsed_response_body[:tokens].any? do |t|
        t[:id] == token_id
      end
      expect(found).to eq true
    end
  end

  describe "GET /auth/tokens/:id" do

    it "returns the new token" do
      response = automate_api_request("/api/v0/auth/tokens/#{token_id}")
      expect(response.http_status).to eq 200
      expect(response.parsed_response_body[:id]).to eq token_id
      expect(response.parsed_response_body[:active]).to eq true

      # only the v2beta tokens API has the projects field exposed
      expect(response.parsed_response_body[:projects]).to eq nil
    end
  end

  describe "PUT /auth/tokens/:id" do
    # TODO (tc) this should 404 if a token doesn't exist.
    it "updates the token" do
      response = automate_api_request("/api/v0/auth/tokens/#{token_id}",
                                      http_method: 'PUT',
                                      request_body: {
                                        active: false
                                      }.to_json)
      expect(response.http_status).to eq 200
      expect(response.parsed_response_body[:active]).to eq false

      tok = automate_api_request("/api/v0/auth/tokens/#{token_id}")
      expect(response.http_status).to eq 200
      expect(response.parsed_response_body[:active]).to eq false
    end
  end

  describe "DELETE /auth/tokens/:id" do

    it "returns 200 OK" do
      response = automate_api_request("/api/v0/auth/tokens/#{token_id}",
                   http_method: 'DELETE')
      expect(response.http_status).to eq 200
    end

    it "returns 404 Not Found" do
      response = automate_api_request("/api/v0/auth/tokens/not-real-token",
                   http_method: 'DELETE')
      expect(response.http_status).to eq 404
      expect(response.parsed_response_body[:error]).to eq "No token record found"
    end
  end
end
