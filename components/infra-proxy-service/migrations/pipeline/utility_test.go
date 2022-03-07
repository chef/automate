package pipeline

import (
	"context"
	"reflect"
	"testing"

	"github.com/chef/automate/api/interservice/local_user"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/testDB"
	"github.com/golang/mock/gomock"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
)

var insertBackup = "./../testdata/insertBackup/"

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
		{name: "Test Insert Org", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, mst: &testDB.MigrationDB{}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: insertBackup, ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Insert},
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

func TestGetUsersForBackup(t *testing.T) {
	// backupFolderDefault := "../../testdata/skipBackup"
	type args struct {
		ctx             context.Context
		st              storage.Storage
		localUserClient *local_user.MockUsersMgmtServiceClient
		result          pipeline.Result
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     pipeline.ActionOps
	}{
		{name: "Test Insert User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: insertBackup, ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Insert},
		{name: "Test Update User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Update"}, localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/updateBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Update},
		{name: "Test Delete User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Delete"}, localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/deleteBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Delete},
		{name: "Test Skip User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Skip"}, localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: "../../testdata/skipBackup", ServerID: "server1", MigrationID: "mig1"}}}, wantError: nil, want1: pipeline.Skip},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			res, err := GetUsersForBackup(tt.args.ctx, tt.args.st, tt.args.localUserClient, tt.args.result)
			require.NoError(t, err)
			require.Equal(t, err, tt.wantError)

			for _, usr := range res.ParsedResult.Users {
				require.Equal(t, usr.ActionOps, tt.want1)
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
		{name: "Test Insert Org User", args: args{ctx: context.Background(), st: &testDB.TestDB{Type: "Insert"}, result: pipeline.Result{Meta: pipeline.Meta{UnzipFolder: insertBackup, ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}}}}, wantError: nil, want1: pipeline.Result{Meta: pipeline.Meta{ServerID: "server1", MigrationID: "mig1"}, ParsedResult: pipeline.ParsedResult{Orgs: []pipeline.Org{{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}}, OrgsUsers: []pipeline.OrgsUsersAssociations{{OrgName: pipeline.Org{Name: "org1", FullName: "Org1_infra", ActionOps: pipeline.Insert}, Users: []pipeline.UserAssociation{{Username: "user1", IsAdmin: true, ActionOps: pipeline.Insert}, {Username: "user2", IsAdmin: false, ActionOps: pipeline.Insert}}}}}}},
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

func TestUserExists(t *testing.T) {
	noUserRecord := "No user record found"
	type args struct {
		ctx             context.Context
		localUserClient *local_user.MockUsersMgmtServiceClient
		User            pipeline.User
		ErrorType       string
	}
	tests := []struct {
		name      string
		args      args
		wantError error
		want1     bool
	}{
		{name: "Test User Exists", args: args{ctx: context.Background(), localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), User: pipeline.User{AutomateUsername: "user1234", Username: "user1234"}}, wantError: nil, want1: true},
		{name: "Test New User", args: args{ctx: context.Background(), localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), User: pipeline.User{AutomateUsername: "user1234", Username: "user1234"}, ErrorType: noUserRecord}, wantError: errors.New(noUserRecord), want1: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.wantError != nil {
				if tt.args.ErrorType == noUserRecord {
					tt.args.localUserClient.EXPECT().GetUser(tt.args.ctx, gomock.Any(), gomock.Any()).Return(nil, errors.New(noUserRecord))
				}
			} else {
				userResponse := &local_user.User{Id: "user1234", Name: "user1234"}
				tt.args.localUserClient.EXPECT().GetUser(tt.args.ctx, gomock.Any(), gomock.Any()).Return(userResponse, nil)
			}

			got := checkUserExist(tt.args.ctx, tt.args.localUserClient, tt.args.User)
			if got != tt.want1 {
				t.Errorf("checkUserExists() got1 = %v, want %v", got, tt.want1)
			}

		})

	}
}

func TestCreateNewUserInAutomate(t *testing.T) {
	type args struct {
		ctx             context.Context
		localUserClient *local_user.MockUsersMgmtServiceClient
		User            pipeline.User
		MockResult      *local_user.User
		ErrorWant       error
	}
	tests := []struct {
		name  string
		args  args
		want1 error
	}{
		{name: "Test New User", args: args{ctx: context.Background(), localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), User: pipeline.User{AutomateUsername: "test", Username: "test", HashPassword: "okokokokokokoko"}, MockResult: &local_user.User{Name: "test", Id: "test", Email: "test@ok"}, ErrorWant: nil}, want1: nil},
		{name: "Test Already User Exists", args: args{ctx: context.Background(), localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)), User: pipeline.User{AutomateUsername: "test", Username: "test", HashPassword: "okokokokokokoko"}, MockResult: nil, ErrorWant: errors.New("User already exists")}, want1: errors.New("User already exists")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.args.localUserClient.EXPECT().CreateUser(tt.args.ctx, gomock.Any()).Return(tt.args.MockResult, tt.args.ErrorWant)
			got := createLocalUser(tt.args.ctx, tt.args.localUserClient, tt.args.User)
			if got != nil && got.Error() != tt.want1.Error() {
				t.Errorf("createLocalUser() got = %v, want %v", got, tt.want1)
			}
		})
	}

}

func TestStoreOrgUserAssociation(t *testing.T) {
	type args struct {
		ctx                context.Context
		st                 storage.Storage
		serverID           string
		orgID              string
		orgUserAssociation pipeline.UserAssociation
	}
	tests := []struct {
		name  string
		args  args
		want  error
		want1 pipeline.ActionOps
	}{
		{name: "Test Delete Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{}, orgUserAssociation: pipeline.UserAssociation{Username: "user1", IsAdmin: false, ActionOps: pipeline.Delete}, serverID: "server1", orgID: "org1"}, want: nil, want1: pipeline.Delete},
		{name: "Test Store Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{}, orgUserAssociation: pipeline.UserAssociation{Username: "user2", IsAdmin: true, ActionOps: pipeline.Insert}, serverID: "server1", orgID: "org1"}, want: nil, want1: pipeline.Insert},
		{name: "Test Edit Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{}, orgUserAssociation: pipeline.UserAssociation{Username: "user1", IsAdmin: true, ActionOps: pipeline.Update}, serverID: "server1", orgID: "org1"}, want: nil, want1: pipeline.Update},
		{name: "Test Error from Delete Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, orgUserAssociation: pipeline.UserAssociation{Username: "user1", IsAdmin: false, ActionOps: pipeline.Delete}, serverID: "server1", orgID: "org1"}, want: errors.New("failed to delete org user association"), want1: pipeline.Delete},
		{name: "Test Error from Store Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, orgUserAssociation: pipeline.UserAssociation{Username: "user2", IsAdmin: true, ActionOps: pipeline.Insert}, serverID: "server1", orgID: "org1"}, want: errors.New("failed to store org user association"), want1: pipeline.Insert},
		{name: "Test Error from Edit Org user association", args: args{ctx: context.Background(), st: &testDB.TestDB{NeedError: true}, orgUserAssociation: pipeline.UserAssociation{Username: "user1", IsAdmin: true, ActionOps: pipeline.Update}, serverID: "server1", orgID: "org1"}, want: errors.New("failed to edit org user association"), want1: pipeline.Update},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {

			got, got1 := storeOrgUserAssociation(tt.args.ctx, tt.args.st, tt.args.serverID, tt.args.orgID, tt.args.orgUserAssociation)
			if got != nil && got.Error() != tt.want.Error() {
				t.Errorf("storeOrgUserAssociation() got = %v, want %v", got, tt.want)
			}
			if got1 != tt.want1 {
				t.Errorf("storeOrgUserAssociation() got1 = %v, want %v", got1, tt.want1)
func TestStoreUser(t *testing.T) {
	type args struct {
		ctx             context.Context
		st              storage.Storage
		user            pipeline.User
		serverID        string
		localUserClient *local_user.MockUsersMgmtServiceClient
	}
	tests := []struct {
		name             string
		errorFromProject bool
		args             args
		want             error
		want1            pipeline.ActionOps
	}{
		{
			name:             "Test Delete User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{},
				user:            pipeline.User{Username: "user1", Email: "user1@user.com", ActionOps: pipeline.Delete},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  nil,
			want1: pipeline.Delete,
		},
		{
			name:             "Test Store User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{},
				user:            pipeline.User{Username: "user2", Email: "user2@user.com", ActionOps: pipeline.Insert},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  nil,
			want1: pipeline.Insert,
		},
		{
			name:             "Test Edit User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{},
				user:            pipeline.User{Username: "user3", Email: "user3@user.com", ActionOps: pipeline.Update},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  nil,
			want1: pipeline.Update,
		},
		{
			name:             "Test Error from Delete User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{},
				user:            pipeline.User{Username: "user4", Email: "user4@user.com", ActionOps: pipeline.Delete},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  errors.New("failed to delete user"),
			want1: pipeline.Delete,
		},
		{
			name:             "Test Error from Store User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{},
				user:            pipeline.User{Username: "user5", Email: "user5@user.com", ActionOps: pipeline.Insert},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  errors.New("failed to store user"),
			want1: pipeline.Insert,
		},
		{
			name:             "Test Error from Edit User",
			errorFromProject: false,
			args: args{
				ctx:             context.Background(),
				st:              &testDB.TestDB{NeedError: true},
				user:            pipeline.User{Username: "user6", Email: "user6@user.com", ActionOps: pipeline.Update},
				serverID:        "server1",
				localUserClient: local_user.NewMockUsersMgmtServiceClient(gomock.NewController(t)),
			},
			want:  errors.New("failed to edit user"),
			want1: pipeline.Update,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			actionOps, err := StoreUser(tt.args.ctx, tt.args.st, tt.args.user, tt.args.serverID, tt.args.localUserClient)
			if err != nil && err.Error() != tt.want.Error() {
				t.Errorf("StoreUser() got = %v, want %v", err, tt.want)
			}
			if actionOps != tt.want1 {
				t.Errorf("StoreUser() got1 = %v, want %v", actionOps, tt.want1)
			}
		})

	}
}
