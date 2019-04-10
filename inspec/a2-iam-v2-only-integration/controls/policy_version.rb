# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2 policy version access API integration tests'

control 'iam-v2-policy-version-1' do
  title 'v2-only access for policy-version'
  desc 'policy-version access for v2'

  describe 'policy-version access for admin user' do
    it 'retrieves value' do
      resp = automate_api_request("/apis/iam/v2beta/policy_version")
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:version][:major]).to eq "V2"
    end
  end

  describe 'policy-version access for non-admin user' do

    NON_ADMIN_USERNAME = 'inspec_test_non_admin'

    before(:all) do
      create_user_request = automate_api_request(
        '/apis/iam/v2beta/users',
        http_method: 'POST',
        request_body: {
          id: NON_ADMIN_USERNAME,
          name: NON_ADMIN_USERNAME,
          password: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate',
        }.to_json
      )
      expect(create_user_request.http_status.to_s).to match(/200|409/)
    end

    after(:all) do
      delete_user_request = automate_api_request(
        "/apis/iam/v2beta/users/#{NON_ADMIN_USERNAME}",
        http_method: 'DELETE',
      )
      expect(delete_user_request.http_status.to_s).to match(/200|404/)
    end

    it 'for non-admin user' do
      resp = automate_api_request(
        "/apis/iam/v2beta/policy_version",
        http_method: "GET",
        user: NON_ADMIN_USERNAME,
      )
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:version][:major]).to eq "V2"
    end
  end
end
