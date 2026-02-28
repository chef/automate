# frozen_string_literal: true

# copyright: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
# license: All rights reserved

title 'iam chef-managed teams'

control 'iam-chef-managed-teams-1' do
  title 'verifies existence of chef-managed teams, which correspond to chef-managed policies'

  describe 'teams' do
    ADMINS = 'admins'
    EDITORS = 'editors'
    VIEWERS = 'viewers'

    it 'list includes chef-managed teams' do
      resp = automate_api_request({
        endpoint: '/apis/iam/v2/teams'
      })

      expect(resp.http_status).to eq 200
      teams = resp.parsed_response_body[:teams].map { |p| p[:id] }

      [ADMINS, EDITORS, VIEWERS].each do |team|
        expect(teams).to include(team)
      end
    end
  end
end
