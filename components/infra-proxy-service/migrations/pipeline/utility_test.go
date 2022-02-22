package pipeline

import (
	"context"
	"reflect"
	"testing"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
	"github.com/golang/mock/gomock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
)

func TestStoreOrg(t *testing.T) {
	type args struct {
		ctx       context.Context
		st        storage.Storage
		org       pipeline.Org
		serverID  string
		authzMock *authz.MockProjectsServiceClient
	}
	tests := []struct {
		name             string
		errorFromProject bool
		args             args
		want             error
		want1            pipeline.ActionOps
	}{
		{name: "Test Delete Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org3", FullName: "Org 1", ActionOps: pipeline.Delete}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: pipeline.Delete},
		{name: "Test Store Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org2", FullName: "Org 2", ActionOps: pipeline.Insert}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: pipeline.Insert},
		{name: "Test Edit Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org3", FullName: "Org 3", ActionOps: pipeline.Update}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: nil, want1: pipeline.Update},
		{name: "Test Create Project Error", errorFromProject: true, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org3", FullName: "Org 3", ActionOps: pipeline.Insert}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: errors.New("Project already exists"), want1: 0},
		{name: "Test Error from Delete Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org3", FullName: "Org 1", ActionOps: pipeline.Delete}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: errors.New("failed to delete org"), want1: pipeline.Delete},
		{name: "Test Error from Store Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{}, org: pipeline.Org{Name: "org2", FullName: "Org 2", ActionOps: pipeline.Insert}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: errors.New("failed to store org"), want1: pipeline.Insert},
		{name: "Test Error from Edit Org", errorFromProject: false, args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, org: pipeline.Org{Name: "org3", FullName: "Org 3", ActionOps: pipeline.Update}, serverID: "server1", authzMock: authz.NewMockProjectsServiceClient(gomock.NewController(t))}, want: errors.New("failed to edit org"), want1: pipeline.Update},
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
		result pipeline.Result
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     pipeline.ActionOps
	}{
		{name: "Test Insert Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/insertBackup/", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Insert},
		{name: "Test Update Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Update"}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/updateBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Update},
		{name: "Test Delete Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Delete"}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/deleteBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Delete},
		{name: "Test Skip Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Skip"}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Skip},
		{name: "Test Error from Org database", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: errors.New("failed to fetch Orgs"), want1: pipeline.Skip},
		{name: "Test Error from Status database", args: args{ctx: context.Background(), st: &testDB.TestDB{}, mst: &testDB.MigrationDB{NeedError: true}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: backupFolderDefault, ServerID: "server1", MigrationID: "mig1"}}}, wantError: errors.New("Failed to update status"), want1: pipeline.Skip},
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

func TestCreatePreview(t *testing.T) {
	type args struct {
		ctx    context.Context
		st     storage.Storage
		mst    storage.MigrationStorage
		result pipeline.Result
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     pipeline.Result
	}{
		{name: "Test Create preview", args: args{ctx: context.Background(), st: &testDB.TestDB{}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}},
		{name: "Test Error from staged database", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}}, wantError: errors.New("Failed to insert staged data"), want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}},
		{name: "Test Error from Status database", args: args{ctx: context.Background(), st: &testDB.TestDB{}, mst: &testDB.MigrationDB{NeedError: true}, result: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}}, wantError: errors.New("Failed to update status"), want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "Org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := CreatePreview(tt.args.ctx, tt.args.st, tt.args.mst, tt.args.result)
			if err != nil && err.Error() != tt.wantError.Error() {
				t.Errorf("CreatePreview() err = %v, want %v", err, tt.wantError)
			}
			if !reflect.DeepEqual(got, tt.want1) {
				t.Errorf("CreatePreview() got = %v, want %v", got, tt.want1)
			}
		})
	}
}

func TestValidateZip(t *testing.T) {
	type args struct {
		name        string
		ctx         context.Context
		st          storage.Storage
		mst         storage.MigrationStorage
		result      pipeline.Result
		requiredErr error
	}

	arg := []args{
		{
			name:        "Validated",
			ctx:         context.Background(),
			st:          &testDB.TestDB{},
			mst:         &testDB.MigrationDB{},
			result:      pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/backup", ServerID: "server1", MigrationID: "mig1"}},
			requiredErr: nil,
		},
		{
			name:        "Organization not found",
			ctx:         context.Background(),
			st:          &testDB.TestDB{},
			mst:         &testDB.MigrationDB{},
			result:      pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/orgnotfound", ServerID: "server1", MigrationID: "mig1"}},
			requiredErr: errors.New("cannot find organizations folder"),
		},
		{
			name:        "KeyDump not found",
			ctx:         context.Background(),
			st:          &testDB.TestDB{},
			mst:         &testDB.MigrationDB{},
			result:      pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/keydumpnotfound", ServerID: "server1", MigrationID: "mig1"}},
			requiredErr: errors.New("stat ../../testdata/keydumpnotfound/key_dump.json: no such file or directory"),
		},
	}

	for _, ar := range arg {
		t.Run(ar.name, func(t *testing.T) {
			res, err := ValidateZip(ar.ctx, ar.st, ar.mst, ar.result)
			if err != nil {
				require.Equal(t, err.Error(), ar.requiredErr.Error())
			} else {
				require.NoError(t, err)
				require.True(t, res.Meta.IsValid)
				require.NotEmpty(t, res.Meta.UnzipFolder)
			}
		})
	}
}

func TestUserOrgAssociation(t *testing.T) {
	deleteBackUp := "../../testdata/deleteBackup/"
	skipBackup := "../../testdata/skipBackup/"
	type args struct {
		ctx    context.Context
		st     storage.Storage
		result pipeline.Result
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     pipeline.Result
	}{
		{name: "Test Insert Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/insertBackup/", ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: true, ActionOps: pipeline.Insert}, {Username: "user2", IsAdmin: false, ActionOps: pipeline.Insert}}}}}}},
		{name: "Test Skip Org and Insert Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: skipBackup, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Skip}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Skip}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Skip}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: false, ActionOps: pipeline.Insert}, {Username: "user2", IsAdmin: true, ActionOps: pipeline.Insert}}}}}}},
		{name: "Test Skip or Update Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Skip"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: skipBackup, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Skip}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Skip}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Skip}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: false, ActionOps: pipeline.Skip}, {Username: "user2", IsAdmin: true, ActionOps: pipeline.Update}}}}}}},
		{name: "Test Delete Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Delete"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: deleteBackUp, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Delete}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Delete}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Delete}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: true, ActionOps: pipeline.Delete}}}}}}},
		{name: "Test Skip org and Delete Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Skip"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: deleteBackUp, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Skip}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Skip}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Skip}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: true, ActionOps: pipeline.Update}, {Username: "user2", ActionOps: pipeline.Delete}}}}}}},
		{name: "Test Skip Org and Insert Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: skipBackup, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Update}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Update}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org3", FullName: "Org1_infra", ActionOps: pipeline.Update}, Users: []pipeline.UserAssociation{{Username: "user1", ActionOps: pipeline.Insert}, {Username: "user2", IsAdmin: true, ActionOps: pipeline.Insert}}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseOrgUserAssociation(tt.args.ctx, tt.args.st, tt.args.result)
			if err != nil && err.Error() != tt.wantError.Error() {
				t.Errorf("ParseOrgUserAssociation() err = %v, want %v", err, tt.wantError)
			}
			if !reflect.DeepEqual(got.ParsedResult.OrgsUsers, tt.want1.ParsedResult.OrgsUsers) {
				t.Errorf("ParseOrgUserAssociation() got = %v, want %v", got.ParsedResult.OrgsUsers, tt.want1.ParsedResult.OrgsUsers)
			}

		})
	}
}
