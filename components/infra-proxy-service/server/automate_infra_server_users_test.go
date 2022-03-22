package server

import (
	"context"
	"reflect"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
)

func TestGetAutomateInfraOrgUsersList(t *testing.T) {
	type args struct {
		ctx    context.Context
		server Server
		req    *request.AutomateInfraOrgUsers
	}

	tests := []struct {
		name      string
		args      args
		wantError error
		want1     *response.AutomateInfraOrgUsers
	}{
		{name: "Test Get infra org users", args: args{ctx: context.Background(), server: Server{service: &service.Service{Storage: &testDB.TestDB{Type: "Read"}}}, req: &request.AutomateInfraOrgUsers{ServerId: "server1", OrgId: "org1"}}, wantError: nil, want1: &response.AutomateInfraOrgUsers{Users: []*response.AutomateInfraOrgUsersListItem{{UserId: 123, ServerId: "server1", OrgId: "org1", InfraServerUsername: "user1"}}}},
		{name: "Test Error from Get infra org users", args: args{ctx: context.Background(), server: Server{service: &service.Service{Storage: &testDB.TestDB{Type: "Read"}}}, req: &request.AutomateInfraOrgUsers{ServerId: "server1", OrgId: "org1"}}, wantError: errors.New("failed to fetch Org Users"), want1: &response.AutomateInfraOrgUsers{Users: []*response.AutomateInfraOrgUsersListItem{{UserId: 123, ServerId: "server1", OrgId: "org1", InfraServerUsername: "user1"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := tt.args.server.GetAutomateInfraOrgUsersList(tt.args.ctx, tt.args.req)
			if err != nil && err.Error() != tt.wantError.Error() {
				t.Errorf("GetAutomateInfraOrgUsersList() err = %v, want %v", err, tt.wantError)
			}
			if !reflect.DeepEqual(got.Users, tt.want1.Users) {
				t.Errorf("GetAutomateInfraOrgUsersList() got = %v, want %v", got.Users, tt.want1.Users)
			}

		})

	}
}
