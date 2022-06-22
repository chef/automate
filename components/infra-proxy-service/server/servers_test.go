package server_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
	request "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/components/infra-proxy-service/test"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestServers(t *testing.T) {
	ctx := context.Background()
	_, serviceRef, conn, close, _, secretsMock := test.SetupInfraProxyService(ctx, t)
	cl := infra_proxy.NewInfraProxyServiceClient(conn)

	defer close()

	secretID := &secrets.Id{
		Id: "fake id",
	}
	newSecret := secrets.Secret{
		Name: "infra-proxy-service-admin-key",
		Type: "chef-server",
		Data: []*query.Kv{
			{Key: "key", Value: "--KEY--"},
		},
	}
	secretWithID := newSecret
	secretWithID.Id = "fake id"

	t.Run("CreateServer", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		t.Run("when a valid server is submitted, creates the new server successfully", func(t *testing.T) {
			req := &request.CreateServer{
				Id:        "chef-infra-server",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			}
			resp, err := cl.CreateServer(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, req.Id, resp.Server.Id)
			assert.Equal(t, req.Name, resp.Server.Name)
			assert.Equal(t, req.Fqdn, resp.Server.Fqdn)
			assert.Equal(t, req.IpAddress, resp.Server.IpAddress)

			cleanupServer(ctx, t, cl, resp.Server.Id)
		})

		/*
			t.Run("when a invalid server is submitted, raise invalid FQDN error", func(t *testing.T) {
				test.SetMockStatusChecker(mockServer, test.MockStatusFailedChecker{})
				req := &request.CreateServer{
					Id:        "chef-infra-server",
					Name:      "Chef infra server",
					Fqdn:      "example.com",
					IpAddress: "0.0.0.0",
				}
				resp, err := cl.CreateServer(ctx, req)
				assert.Nil(t, resp)
				assert.Error(t, err, "Not able to connect to the server")
			})
		*/

		t.Run("when the server ID is missing, raise invalid argument error", func(t *testing.T) {

			resp, err := cl.CreateServer(ctx, &request.CreateServer{
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			assert.Error(t, err, "must supply server ID")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server ID already exists, raise invalid argument error", func(t *testing.T) {
			// test.SetMockStatusChecker(mockServer, test.MockStatusChecker{})

			req1 := &request.CreateServer{
				Id:        "chef-infra-server",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			}
			resp1, err := cl.CreateServer(ctx, req1)
			require.NoError(t, err)
			require.NotNil(t, resp1)
			assert.Equal(t, req1.Id, resp1.Server.Id)

			req2 := &request.CreateServer{
				Id:        "chef-infra-server",
				Name:      "New chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			}
			resp2, err := cl.CreateServer(ctx, req2)
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})

		t.Run("when the server name is missing, raise invalid argument error", func(t *testing.T) {
			resp, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			assert.Error(t, err, "must supply server name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("GetServers", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when there are no servers in db, return empty list", func(t *testing.T) {
			list, err := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err)
			require.Nil(t, list.Servers)
			assert.Equal(t, 0, len(list.Servers))
		})

		t.Run("when there are some servers in db, return all the servers successfully", func(t *testing.T) {
			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			resp2, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server2",
				Name:      "Chef infra server",
				Fqdn:      "api.chef.io",
				IpAddress: "",
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			list, err := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Servers, resp1.Server)
			assert.Contains(t, list.Servers, resp2.Server)
			assert.Equal(t, 2, len(list.Servers))

			cleanupServer(ctx, t, cl, resp1.Server.Id)
			cleanupServer(ctx, t, cl, resp2.Server.Id)
		})

		t.Run("when the server exists with orgs, return servers list with org count", func(t *testing.T) {
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			respOrg, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Id:        "infra-org-id",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  resp1.Server.Id,
				Projects:  []string{},
			})
			require.NoError(t, err)
			require.NotNil(t, respOrg)

			list, err := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Equal(t, 1, len(list.Servers))
			assert.EqualValues(t, 1, list.Servers[0].OrgsCount)

			cleanupOrg(ctx, t, cl, respOrg.Org.Id, respOrg.Org.ServerId)
			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})
	})

	t.Run("GetServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when there is no ID in the request, raise an invalid argument error", func(t *testing.T) {
			resp, err := cl.GetServer(ctx, &request.GetServer{
				Id: "",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server does not exist, return server not found", func(t *testing.T) {
			serverID := "97e01ea1-976e-4626-88c8-43345c5d934f"
			resp, err := cl.GetServer(ctx, &request.GetServer{
				Id: serverID,
			})

			require.Nil(t, resp)
			require.Contains(t, err.Error(), fmt.Sprintf("no server found with ID \"%s\"", serverID))
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the server exists, return the server successfully", func(t *testing.T) {
			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			server1, err := cl.GetServer(ctx, &request.GetServer{
				Id: resp1.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, server1)
			assert.Equal(t, resp1.Server.Id, server1.Server.Id)
			assert.Equal(t, resp1.Server.Name, server1.Server.Name)
			assert.Equal(t, resp1.Server.Fqdn, server1.Server.Fqdn)
			assert.Equal(t, resp1.Server.IpAddress, server1.Server.IpAddress)
			assert.EqualValues(t, 0, server1.Server.OrgsCount)

			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})

		t.Run("when the server exists with orgs, return the server with org count", func(t *testing.T) {
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			respOrg, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Id:        "infra-org-id",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  resp1.Server.Id,
				Projects:  []string{},
			})
			require.NoError(t, err)
			require.NotNil(t, respOrg)

			server1, err := cl.GetServer(ctx, &request.GetServer{
				Id: resp1.Server.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, server1)
			assert.Equal(t, resp1.Server.Id, server1.Server.Id)
			assert.Equal(t, resp1.Server.Name, server1.Server.Name)
			assert.Equal(t, resp1.Server.Fqdn, server1.Server.Fqdn)
			assert.Equal(t, resp1.Server.IpAddress, server1.Server.IpAddress)
			assert.EqualValues(t, 1, server1.Server.OrgsCount)

			cleanupOrg(ctx, t, cl, respOrg.Org.Id, respOrg.Org.ServerId)
			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})
	})

	t.Run("DeleteServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when an existing server is deleted, deletes the server successfully", func(t *testing.T) {
			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "api.chef.io",
				IpAddress: "",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			serverListBefore, err := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err)
			assert.Equal(t, 1, len(serverListBefore.Servers))

			resp, err2 := cl.DeleteServer(ctx, &request.DeleteServer{Id: resp1.Server.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Server.Name, resp.Server.Name)

			serverListAfter, err3 := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err3)
			assert.Equal(t, len(serverListBefore.Servers)-1, len(serverListAfter.Servers))
		})

		t.Run("when the server exists with orgs, raise server can not be deleted error", func(t *testing.T) {
			secretsMock.EXPECT().Create(gomock.Any(), &newSecret, gomock.Any()).Return(secretID, nil)
			secretsMock.EXPECT().Read(gomock.Any(), secretID, gomock.Any()).Return(&secretWithID, nil)
			secretsMock.EXPECT().Delete(gomock.Any(), secretID, gomock.Any())

			resp1, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server1",
				Name:      "Chef infra server",
				Fqdn:      "api.chef.io",
				IpAddress: "",
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)

			respOrg, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Id:        "infra-org-id",
				Name:      "infra-org",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  resp1.Server.Id,
				Projects:  []string{},
			})
			require.NoError(t, err)
			require.NotNil(t, respOrg)

			serverListBefore, err := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err)
			assert.Equal(t, 1, len(serverListBefore.Servers))

			resp, err2 := cl.DeleteServer(ctx, &request.DeleteServer{Id: resp1.Server.Id})
			require.Error(t, err2)
			require.Nil(t, resp)
			assert.Regexp(t, "cannot delete server.*still has organizations attached", err2.Error())
			grpctest.AssertCode(t, codes.FailedPrecondition, err2)

			serverListAfter, err3 := cl.GetServers(ctx, &request.GetServers{})
			require.NoError(t, err3)
			assert.Equal(t, len(serverListBefore.Servers), len(serverListAfter.Servers))

			cleanupOrg(ctx, t, cl, respOrg.Org.Id, respOrg.Org.ServerId)
			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})

		t.Run("when the server ID for the server to delete is empty, raise an invalid argument error", func(t *testing.T) {
			resp, err := cl.GetServer(ctx, &request.GetServer{
				Id: "",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server to delete does not exist, returns server not found", func(t *testing.T) {
			resp, err := cl.GetServer(ctx, &request.GetServer{
				Id: "97e01ea1-976e-4626-88c8-43345c5d934f",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})
	})

	t.Run("UpdateServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when a valid server is submitted, updates the server successfully", func(t *testing.T) {
			resp, err := cl.CreateServer(ctx, &request.CreateServer{
				Id:        "chef-infra-server",
				Name:      "Chef infra server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, resp)

			updateReq := &request.UpdateServer{
				Id:        resp.Server.Id,
				Name:      "new-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			}

			updatedSerResp, err := cl.UpdateServer(ctx, updateReq)
			require.NoError(t, err, "update server")
			require.NotNil(t, updatedSerResp)
			assert.Equal(t, updateReq.Name, updatedSerResp.Server.Name)

			cleanupServer(ctx, t, cl, resp.Server.Id)
		})

		t.Run("when the server ID for the server to update is empty, raise invalid argument error", func(t *testing.T) {
			resp, err := cl.UpdateServer(ctx, &request.UpdateServer{
				Id:        "",
				Name:      "new-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			assert.Error(t, err, "must supply server ID")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server ID for the server to update is missing, raise invalid argument error", func(t *testing.T) {
			resp, err := cl.UpdateServer(ctx, &request.UpdateServer{
				Name:      "chef-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			assert.Error(t, err, "must supply server ID")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server name for the server to update is missing, raise invalid argument error", func(t *testing.T) {
			resp, err := cl.UpdateServer(ctx, &request.UpdateServer{
				Id:        "chef-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			assert.Error(t, err, "must supply server name")
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the server to update does not exist, raise server not found", func(t *testing.T) {
			resp, err := cl.UpdateServer(ctx, &request.UpdateServer{
				Id:        "no-chef-infra-server-id",
				Name:      "chef-infra-server",
				Fqdn:      "example.com",
				IpAddress: "0.0.0.0",
			})
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})
	})
}

func cleanupServer(ctx context.Context, t *testing.T, cl infra_proxy.InfraProxyServiceClient, serverID string) {
	t.Helper()
	deleteReq := request.DeleteServer{Id: serverID}
	_, err := cl.DeleteServer(ctx, &deleteReq)
	require.NoError(t, err)
}
