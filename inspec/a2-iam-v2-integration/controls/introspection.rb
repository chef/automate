# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'IAM v2 migration API authz introspection integration tests'

control 'introspection-iam-v2-1' do
  title 'authz introspection with IAM v2'
  desc 'these checks ensure that authz introspection works
    after having switched to IAM v2'

  describe 'introspect all' do
    it 'returns what we expect' do
      resp = automate_api_request('/api/v0/auth/introspect')
      expect(resp.http_status).to eq 200
    end
  end

  describe 'introspect some' do
    it 'returns what we expect' do
      resp = automate_api_request('/api/v0/auth/introspect_some',
                                  http_method: 'POST',
                                  request_body:  {
                                    paths: [
                                      '/compliance/reporting/stats/summary',
                                      '/compliance/reporting/stats/failures',
                                      '/compliance/reporting/stats/trend',
                                    ]
                                  }.to_json)
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:endpoints]).to eq(
        {
          '/compliance/reporting/stats/failures': {
            'get': false,
            'put': false,
            'post': true,
            'delete': false,
            'patch': false
          },
          '/compliance/reporting/stats/summary': {
            'get': false,
            'put': false,
            'post': true,
            'delete': false,
            'patch': false
          },
          '/compliance/reporting/stats/trend': {
            'get': false,
            'put': false,
            'post': true,
            'delete': false,
            'patch': false
          },
        }
      )
    end
  end

  describe 'introspect some with parameters' do
    it 'returns what we expect' do
      resp = automate_api_request('/api/v0/auth/introspect',
                                  http_method: 'POST',
                                  request_body:  {
                                    path: '/auth/policies/foo',
                                    parameters: [ 'foo' ],
                                  }.to_json)
      expect(resp.http_status).to eq 200
      expect(resp.parsed_response_body[:endpoints]).to eq(
        {
          '/auth/policies/foo': {
            'get': false,
            'put': false,
            'post': false,
            'delete': true,
            'patch': false
          },
        }
      )
    end
  end
end
