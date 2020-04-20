package server_test

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
	authz "github.com/chef/automate/api/interservice/authz/common"
	request "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/components/infra-proxy-service/constants"
	"github.com/chef/automate/components/infra-proxy-service/test"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/logger"
)

func TestOrgs(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	require.NoError(t, err)

	_, serviceRef, conn, close, authzMock := test.SetupInfraProxyService(ctx, t, l, *migrationConfig)
	secretsMock := serviceRef.Secrets.(*secrets.MockSecretsServiceClient)

	cl := infra_proxy.NewInfraProxyClient(conn)

	t.Helper()
	defer close()

	serverRes, err := cl.CreateServer(ctx, &request.CreateServer{
		Name:        "Chef infra server",
		Description: "Chef infra server",
		Fqdn:        "domain.com",
		IpAddress:   "0.0.0.0",
	})
	require.NoError(t, err)
	require.NotNil(t, serverRes)

	secretID := &secrets.Id{
		Id: "fake id",
	}
	newSecret := secrets.Secret{
		Name: "infra-proxy-service-admin-key",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: "admin"},
			{Key: "key", Value: "--KEY--"},
		},
	}
	secretWithID := newSecret
	secretWithID.Id = "fake id"

	t.Run("CreateOrg", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		t.Run("when a valid org is submitted, creates the new org successfully", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			org := resp.Org
			assert.Equal(t, req.Name, org.Name)
			assert.Equal(t, req.AdminUser, org.AdminUser)
			assert.Equal(t, req.AdminKey, secretWithID.Data[1].Value)
			assert.Equal(t, req.ServerId, org.ServerId)
			assert.Equal(t, 2, len(org.Projects))

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})

		t.Run("when no projects are passed, creates the new org successfully with empty projects", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			}

			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			org := resp.Org
			assert.Equal(t, req.Name, org.Name)
			assert.Equal(t, 0, len(org.Projects))

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})

		t.Run("when the org exists, raise the error org already exists", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.NoError(t, err)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp2, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})

		t.Run("when the org required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.Nil(t, resp)
			assert.Contains(t, err.Error(), "must supply org name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			resp2, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			assert.Contains(t, err.Error(), "must supply org name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the org required field server id is missing or empty, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				Projects:  []string{},
			})
			require.Nil(t, resp)
			assert.Contains(t, err.Error(), "must supply server id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			resp2, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  "",
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			assert.Contains(t, err.Error(), "must supply server id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server does not exist, raise server not found error", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			serverID := "97e01ea1-976e-4626-88c8-43345c5d934f"
			resp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverID,
				Projects:  []string{},
			})
			require.Nil(t, resp)
			require.Contains(t, err.Error(), fmt.Sprintf("no server found with ID \"%s\"", serverID))
			grpctest.AssertCode(t, codes.NotFound, err)

		})
	})

	t.Run("GetOrgs", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when no orgs exist returns empty list", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.Nil(t, err)
			require.NotNil(t, resp)

			assert.Equal(t, 0, len(resp.GetOrgs()))
		})

		t.Run("when the project filter is empty returns all orgs", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			}
			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)

			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.Equal(t, 1, len(orgs.GetOrgs()))

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})

		t.Run("when the project filter is filtered by * returns all orgs", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			ctx = test.InsertProjectsIntoNewContext([]string{"*"})
			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.Contains(t, orgs.Orgs, resp1.Org)
			assert.Contains(t, orgs.Orgs, resp2.Org)
			assert.Equal(t, 2, len(orgs.Orgs))

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when the project filter is filtered by (unassigned) returns all (unassigned) orgs", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"other_project"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			ctx = test.InsertProjectsIntoNewContext([]string{constants.UnassignedProjectID})
			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.Contains(t, orgs.Orgs, resp1.Org)
			assert.Equal(t, 1, len(orgs.Orgs))

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when the project filter has one project returns only the orgs with the same project as the filter", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			ctx = test.InsertProjectsIntoNewContext([]string{"project1"})
			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.Contains(t, orgs.Orgs, resp1.Org)
			assert.Equal(t, 1, len(orgs.Orgs))

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when the project filter has multiple projects returns only the orgs with at least one of the projects from the filter", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project3"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req = &request.CreateOrg{
				Name:      "another-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"other_project"},
			}
			resp3, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp3)

			ctx = test.InsertProjectsIntoNewContext([]string{"project1", "project3"})
			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.Contains(t, orgs.Orgs, resp1.Org)
			assert.Contains(t, orgs.Orgs, resp2.Org)
			assert.Equal(t, 2, len(orgs.Orgs))

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
			cleanupOrg(ctx, t, cl, resp3.Org.Id)
		})

		t.Run("when the project filter has one project and none of the orgs in the db have that project, returns empty orgs", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			ctx = test.InsertProjectsIntoNewContext([]string{"other_project"})
			orgs, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, orgs)
			assert.NotContains(t, orgs.Orgs, resp1.Org)
			assert.NotContains(t, orgs.Orgs, resp2.Org)
			assert.Equal(t, 0, len(orgs.Orgs))

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

	})

	t.Run("GetOrg", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when there is no id in the request, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: "",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the submitted ID is not a UUIDv4, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: "35bffbab-3a49-dd8a-94a1-9ea87ec5c3cc",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the org does not exists, return org not found", func(t *testing.T) {
			ctx := context.Background()
			orgID := "97e01ea1-976e-4626-88c8-43345c5d934f"
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: orgID,
			})

			require.Nil(t, resp)
			require.Contains(t, err.Error(), fmt.Sprintf("no org found with ID \"%s\"", orgID))
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the org exists, returns the org successfully", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)

			org, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: resp.Org.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, org)
			assert.Equal(t, resp.Org.Name, org.Org.Name)
			assert.Equal(t, resp.Org.Id, org.Org.Id)
			assert.Equal(t, resp.Org.AdminUser, org.Org.AdminUser)
			assert.Equal(t, resp.Org.AdminKey, secretWithID.GetId())
			assert.Equal(t, resp.Org.Projects, org.Org.Projects)

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})
	})

	t.Run("DeleteOrg", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when an existing org is deleted, deletes the org successfully", func(t *testing.T) {
			ctx := context.Background()

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:*" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected org name passed to PurgeSubjectFromPolicies")
			}

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			orgsBefore, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, 2, len(orgsBefore.Orgs))

			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       resp1.Org.Id,
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Org.Id, resp.Org.Id)
			assert.Equal(t, resp1.Org.Name, resp.Org.Name)

			orgsAfter, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, len(orgsAfter.Orgs), len(orgsBefore.Orgs)-1)
			assert.Contains(t, orgsAfter.Orgs, resp2.Org)

			authzMock.PurgeSubjectFromPoliciesFunc = test.DefaultMockPurgeFunc
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when an existing org is deleted and is in the project filter, deletes the org successfully", func(t *testing.T) {
			ctx := context.Background()
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:*" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected org name passed to PurgeSubjectFromPolicies")
			}

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			orgsBefore, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, 2, len(orgsBefore.Orgs))

			ctx = test.InsertProjectsIntoNewContext([]string{"project2"})

			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       resp1.Org.Id,
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Org.Id, resp.Org.Id)
			assert.Equal(t, resp1.Org.Name, resp.Org.Name)

			orgsAfter, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, len(orgsAfter.Orgs), len(orgsBefore.Orgs)-1)
			assert.Contains(t, orgsAfter.Orgs, resp2.Org)

			authzMock.PurgeSubjectFromPoliciesFunc = test.DefaultMockPurgeFunc
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when an existing org is deleted and the project filter is *, deletes the org successfully", func(t *testing.T) {
			ctx := context.Background()
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:*" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected org name passed to PurgeSubjectFromPolicies")
			}

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			orgsBefore, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, 2, len(orgsBefore.Orgs))

			ctx = test.InsertProjectsIntoNewContext([]string{"*"})

			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       resp1.Org.Id,
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Org.Id, resp.Org.Id)
			assert.Equal(t, resp1.Org.Name, resp.Org.Name)

			orgsAfter, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, len(orgsAfter.Orgs), len(orgsBefore.Orgs)-1)
			assert.Contains(t, orgsAfter.Orgs, resp2.Org)

			authzMock.PurgeSubjectFromPoliciesFunc = test.DefaultMockPurgeFunc
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when an existing org is deleted and the project filter is (unassigned), deletes the org successfully", func(t *testing.T) {
			ctx := context.Background()
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:*" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected org name passed to PurgeSubjectFromPolicies")
			}

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			orgsBefore, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, 2, len(orgsBefore.Orgs))

			ctx = test.InsertProjectsIntoNewContext([]string{constants.UnassignedProjectID})

			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       resp1.Org.Id,
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Org.Id, resp.Org.Id)
			assert.Equal(t, resp1.Org.Name, resp.Org.Name)

			orgsAfter, err := cl.GetOrgs(context.Background(), &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, len(orgsAfter.Orgs), len(orgsBefore.Orgs)-1)

			authzMock.PurgeSubjectFromPoliciesFunc = test.DefaultMockPurgeFunc
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when an existing org is filtered by projects return NotFound", func(t *testing.T) {
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:*" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected org name passed to PurgeSubjectFromPolicies")
			}

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp1, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp1)

			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())
			req = &request.CreateOrg{
				Name:      "other-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project2"},
			}
			resp2, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp2)

			orgsBefore, err := cl.GetOrgs(ctx, &request.GetOrgs{
				ServerId: serverRes.Server.Id,
			})
			require.NoError(t, err)
			assert.Equal(t, 2, len(orgsBefore.Orgs))

			ctx = test.InsertProjectsIntoNewContext([]string{"other_project"})

			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       resp1.Org.Id,
				ServerId: serverRes.Server.Id,
			})
			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err2)

			cleanupOrg(ctx, t, cl, resp1.Org.Id)
			cleanupOrg(ctx, t, cl, resp2.Org.Id)
		})

		t.Run("when the org to delete is does not exist, returns org not found", func(t *testing.T) {
			ctx := context.Background()
			resp, err2 := cl.DeleteOrg(ctx, &request.DeleteOrg{
				Id:       "97e01ea1-976e-4626-88c8-43345c5d934f",
				ServerId: serverRes.Server.Id,
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err2)
		})
	})

	t.Run("UpdateOrg", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when a valid org update request is submitted, updates the org successfully", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			req := &request.CreateOrg{
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)

			secretsMock.EXPECT().Update(gomock.Any(), &secretWithID, gomock.Any())
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)

			newName := "my-infra-org"
			updateReq := &request.UpdateOrg{
				Id:        resp.Org.Id,
				Name:      newName,
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			updatedOrgResp, err := cl.UpdateOrg(ctx, updateReq)
			require.NoError(t, err, "update org")
			require.NotNil(t, updatedOrgResp)
			assert.Equal(t, updateReq.Name, updatedOrgResp.Org.Name)
			assert.Equal(t, updateReq.Projects, updatedOrgResp.Org.Projects)

			cleanupOrg(ctx, t, cl, resp.Org.Id)
		})

		t.Run("when org to update does not exist, raise org not found", func(t *testing.T) {
			ctx := context.Background()
			updateReq := &request.UpdateOrg{
				Id:        "97e01ea1-976e-4626-88c8-43345c5d934f",
				Name:      "new-org-name",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			updateOrg, err := cl.UpdateOrg(ctx, updateReq)

			require.Nil(t, updateOrg)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the org Id is missing or empty, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Name:      "update-infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.Nil(t, resp)
			assert.Contains(t, err.Error(), "must supply org id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			resp2, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "",
				Name:      "update-infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			assert.Contains(t, err.Error(), "must supply org id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the submitted ID is not a UUIDv4, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "35bffbab-3a49-dd8a-94a1-9ea87ec5c3cc",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the org required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "23e01ea1-976e-4626-88c8-43345c5d912e",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.Nil(t, resp)
			assert.Contains(t, err.Error(), "must supply org name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			resp2, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "23e01ea1-976e-4626-88c8-43345c5d912e",
				Name:      "",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			assert.Contains(t, err.Error(), "must supply org name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the org required field server id is missing or empty, raise an invalid argument error", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "23e01ea1-976e-4626-88c8-43345c5d912e",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				Projects:  []string{},
			})
			require.Nil(t, resp)
			assert.Contains(t, err.Error(), "must supply server id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			resp2, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "23e01ea1-976e-4626-88c8-43345c5d912e",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  "",
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			assert.Contains(t, err.Error(), "must supply server id")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server does not exist, raise server not found error", func(t *testing.T) {
			ctx := context.Background()
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp, err := cl.UpdateOrg(ctx, &request.UpdateOrg{
				Id:        "23e01ea1-976e-4626-88c8-43345c5d912e",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  "97e01ea1-976e-4626-88c8-43345c5d934f",
				Projects:  []string{},
			})
			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})
	})
	cleanupServer(ctx, t, cl, serverRes.Server.Id)
}

func cleanupOrg(ctx context.Context, t *testing.T, cl infra_proxy.InfraProxyClient, orgID string) {
	t.Helper()
	deleteReq := request.DeleteOrg{Id: orgID}
	_, err := cl.DeleteOrg(context.Background(), &deleteReq)
	require.NoError(t, err)
}
