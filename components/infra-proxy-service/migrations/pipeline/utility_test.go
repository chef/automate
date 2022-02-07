package pipeline

import (
	"context"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
	"github.com/golang/mock/gomock"
	"github.com/pkg/errors"
	"reflect"
	"testing"
)

func TestStoreOrg(t *testing.T) {
	type args struct {
		ctx       context.Context
		st        storage.Storage
		org       Org
		serverID  string
		authzMock *authz.MockProjectsServiceClient
	}
	tests := []struct {
		name             string
		errorFromProject bool
		args             args
		want             error
		want1            ActionOps
	}{
		{name: "Test Delete Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org3", FullName: "Org 1", ActionOps: Delete}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: Delete},
		{name: "Test Store Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org2", FullName: "Org 2", ActionOps: Insert}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: Insert},
		{name: "Test Edit Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org3", FullName: "Org 3", ActionOps: Update}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: Update},
		{name: "Test Create Project Error", errorFromProject: true, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: Org{Name: "org3", FullName: "Org 3", ActionOps: Insert}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: errors.New("Project already exists"), want1: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			projectResponse := &authz.CreateProjectResp{
				Project: &authz.Project{
					Id:     "testId",
					Name:   "test_name",
					Status: "test_status",
				},
			}
			if tt.errorFromProject {
				tt.args.authzMock.EXPECT().CreateProject(tt.args.ctx, gomock.Any(), gomock.Any()).Return(nil, errors.New("Project already exists"))
			} else {
				tt.args.authzMock.EXPECT().CreateProject(tt.args.ctx, gomock.Any(), gomock.Any()).Return(projectResponse, nil)
			}
			got, got1 := StoreOrg(tt.args.ctx, tt.args.st, tt.args.org, tt.args.serverID, tt.args.authzMock)
			if got != nil && got.Error() != tt.want.Error() {
				t.Errorf("StoreOrg() got = %v, want %v", got, tt.want)
			}
			if got1 != tt.want1 {
				t.Errorf("StoreOrg() got1 = %v, want %v", got1, tt.want1)
			}
		})

	}
}

func TestParseOrg(t *testing.T) {
	backupFolderDefault := "../../testdata/skipBackup"
	type args struct {
		ctx    context.Context
		st     storage.Storage
		mst    storage.MigrationStorage
		result Result
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     ActionOps
	}{
		{name: "Test Insert Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, mst: &testDB.MigrationDB{}, result: Result{Meta: Meta{UnzipFolder: "../../testdata/insertBackup/", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: Insert},
		{name: "Test Update Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Update"}, mst: &testDB.MigrationDB{}, result: Result{Meta: Meta{UnzipFolder: "../../testdata/updateBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: Update},
		{name: "Test Delete Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Delete"}, mst: &testDB.MigrationDB{}, result: Result{Meta: Meta{UnzipFolder: "../../testdata/deleteBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: Delete},
		{name: "Test Skip Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Skip"}, mst: &testDB.MigrationDB{}, result: Result{Meta: Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: Skip},
		{name: "Test Error from Org database", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, mst: &testDB.MigrationDB{}, result: Result{Meta: Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: errors.New("failed to fetch Orgs"), want1: Skip},
		{name: "Test Error from Status database", args: args{ctx: context.Background(), st: &testDB.TestDB{}, mst: &testDB.MigrationDB{NeedError: true}, result: Result{Meta: Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: errors.New("Failed to update status"), want1: Skip},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseOrgs(tt.args.ctx, tt.args.st, tt.args.mst, tt.args.result)
			if err != nil && err.Error() != tt.wantError.Error() {
				t.Errorf("ParseInfraServerOrgs() err = %v, want %v", err, tt.wantError)
			}
			for _, org := range got.ParsedResult.Orgs {
				if !reflect.DeepEqual(org.ActionOps, tt.want1) {
					t.Errorf("parseOrg() got = %v, want %v", org.ActionOps, tt.want1)
				}
			}

		})

	}
}
