title 'ingest-service REST API integration tests'

control 'ingest-status' do
  title 'check ingest status endpoints'
  desc 'Verifies existing status queries work similar to the A1 data-collector'

  describe 'GET status response' do
    before(:all) do
      test_token_request = automate_api_request(
        '/api/v0/auth/tokens',
        http_method: 'POST',
        request_body: {
          'description': 'ingest_test_token',
          'active': true
        }.to_json
      )
      INGEST_TOKEN = test_token_request.parsed_response_body[:value]
      INGEST_TOKEN_ID = test_token_request.parsed_response_body[:id]
      expect(test_token_request.http_status).to eq(200)
    end

    after(:all) do
      delete_token_request = automate_api_request(
        "/api/v0/auth/tokens/#{INGEST_TOKEN_ID}",
        http_method: 'DELETE',
      )
      expect(delete_token_request.http_status.to_s).to match(/200|404/)
    end

    it 'GET /data-collector/v0 returns the correct response for client' do
      expect(
        automate_client_api_request(
          '/data-collector/v0',
          INGEST_TOKEN,
          http_method: 'GET',
        ).parsed_response_body[:status]
      ).to eq('ok')
    end
  end
end
