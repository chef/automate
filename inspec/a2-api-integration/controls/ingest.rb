require_relative '../../constants'

title 'ingest-service REST API integration tests'

control 'ingest-status' do
  title 'check ingest status endpoints'
  desc 'Verifies existing status queries work similar to the A1 data-collector'

  describe 'GET status response' do
    
    before(:all) do
      INGEST_TOKEN_ID = "ingest_test_token-#{TIMESTAMP}"

      test_token_resp = automate_api_request(
        '/apis/iam/v2/tokens',
        http_method: 'POST',
        request_body: {
          'id': INGEST_TOKEN_ID,
          'name': 'ingest_test_token'
        }.to_json
      )
      expect(test_token_resp.http_status).to eq(200)

      INGEST_TOKEN = test_token_resp.parsed_response_body[:token][:value]

      expect(
        automate_api_request(
          "/apis/iam/v2/policies/#{INGEST_POLICY_ID}/members:add",
          http_method: 'POST',
          request_body: {
            'members': ["token:#{INGEST_TOKEN_ID}"]
          }.to_json
        ).http_status
      ).to eq(200)
    end

    after(:all) do
      delete_token_request = automate_api_request(
        "/apis/iam/v2/tokens/#{INGEST_TOKEN_ID}",
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
