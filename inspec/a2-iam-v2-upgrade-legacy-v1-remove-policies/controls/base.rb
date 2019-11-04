control 'remove-legacy-policies' do
    title 'IAM v2 Policies'
    desc 'test removal of legacy policies post migration'

    describe 'migrated legacy v1 policies' do
        it 'legacy policies can be deleted' do
            resp = automate_api_request('/apis/iam/v2beta/policies')
            expect(resp.http_status).to eq 200

            all_policies = resp.parsed_response_body[:policies]
            legacy_policies = all_policies.select{ |p| /^\[Legacy\]/.match(p[:name]) }
            legacy_policies.map { |p| p[:id] }.each do |id|
                resp = automate_api_request("/apis/iam/v2beta/policies/#{id}", http_method: 'DELETE')
                expect(resp.http_status).to eq 200
            end
        end
    end
end