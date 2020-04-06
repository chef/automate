# encoding: utf-8
# copyright: 2018, Chef Software, Inc.
# license: All rights reserved

title 'A1 Internal User to A2 Local User Migration Integration Tests'

control 'local-user-migration-1' do
  title 'local user migration'
  desc 'Attempt to login using the credentials set up in A1'

  describe 'GET own user information after login' do
    # Note: automate_api_request will do a login through dex -- this verifies
    # that the passed-in user/password combination works
    {
      'alice' => 'Alice Schmidt',
      'bob' => 'Bob Bobbikowsky',
      'test-admin' => 'test-admin',
    }.each do |user, displayname|
      it "returns #{user}'s information" do
        create_non_admin_request = automate_api_request(
          "/apis/iam/v2/users/#{user}",
          http_method: 'GET',
          user: user,
          pass: "#{user}-password",
        )

        expect(create_non_admin_request.http_status).to eq(200)
        expect(create_non_admin_request.parsed_response_body[:user][:id]).to eq(user)
        expect(create_non_admin_request.parsed_response_body[:user][:name]).to eq(displayname)
      end
    end
  end
end

control 'local-user-migration-2' do
  title 'team membership of A1 admin users'
  desc "The admins' team members need to contain the admin and test-admin users"

  describe "The 'admins' team" do
    admins_team_id = 'admins'

    it "exists and contains the admin users' IDs" do
      admin_user = automate_api_request('apis/iam/v2/users/admin')
      expect(admin_user.http_status).to eq(200)
      admin_membership_id = admin_user.parsed_response_body[:user][:membership_id]

      test_admin_user = automate_api_request('apis/iam/v2/users/test-admin')
      expect(test_admin_user.http_status).to eq(200)
      test_admin_membership_id = test_admin_user.parsed_response_body[:user][:membership_id]

      admins_team = automate_api_request("apis/iam/v2/teams/#{admins_team_id}/users")
      expect(admins_team.http_status).to eq(200)

      [admin_membership_id, test_admin_membership_id].each do |id| 
        expect(admins_team.parsed_response_body[:membership_ids]).to include(id)
      end
    end
  end
end

control 'local-user-migration-3' do
  title 'migration of the builder user'
  desc 'The A1 builder user should also be present in A2'

  describe 'builder user' do
    it 'is migrated' do
      builder_user_request = automate_api_request(
        "apis/iam/v2/users/builder",
        http_method: 'GET',
        user: 'builder',
        pass: ENV['A1_BUILDER_PASSWORD'],
      )

      expect(builder_user_request.http_status).to eq(200)
      expect(builder_user_request.parsed_response_body[:user][:id]).to eq('builder')
      expect(builder_user_request.parsed_response_body[:user][:name]).to eq('builder')
    end
  end
end
