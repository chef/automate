# frozen_string_literal: true

# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'iam chef-managed teams'

control 'iam-chef-managed-teams-1' do
  title 'verifies existence of chef-managed teams, which correspond to chef-managed policies'

  describe 'teams' do
    ADMINS = 'admins'
    EDITORS = 'editors'
    VIEWERS = 'viewers'

    it 'list includes chef-managed teams' do
      resp = automate_api_request('/apis/iam/v2/teams')

      expect(resp.http_status).to eq 200
      teams = resp.parsed_response_body[:teams].map { |p| p[:id] }

      [ADMINS, EDITORS, VIEWERS].each do |team|
        expect(teams).to include(team)
      end
    end
  end
end
