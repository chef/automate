package integration_test

import (
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetOrgs(t *testing.T) {

	t.Run("Get Orgs which is present by default", func(t *testing.T) {
		req := &request.GetOrgs{
			ServerId: autoDeployedChefServerID,
		}
		orgs, err := infraProxy.GetOrgs(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, orgs)
		assert.Equal(t, 1, len(orgs.Orgs))
	})

	t.Run("Get Org when no server id is present", func(t *testing.T) {
		req := &request.GetOrgs{}
		orgs, err := infraProxy.GetOrgs(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, orgs)
	})

	t.Run("Get Orgs after creating an org in Server", func(t *testing.T) {
		testServerId := "test_server_id"

		//Creating Server for a test server
		createServerReq := &request.CreateServer{
			Id:   testServerId,
			Name: testServerId,
			Fqdn: "test.example.com",
		}

		infraProxy.CreateServer(ctx, createServerReq)

		createOrgReq := &request.CreateOrg{
			Id:        "Test_org",
			ServerId:  testServerId,
			Name:      "Test_org",
			AdminUser: "Test_user",
			AdminKey:  " ------------ Test Key ----------",
		}

		createOrgRes, err := infraProxy.CreateOrg(ctx, createOrgReq)
		assert.NotNil(t, createOrgRes)
		assert.NoError(t, err)

		req := &request.GetOrgs{
			ServerId: testServerId,
		}
		orgs, err := infraProxy.GetOrgs(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, orgs)
		assert.Equal(t, 1, len(orgs.Orgs))

		//Deleting Org and server for further test cases
		_, _ = infraProxy.DeleteOrg(ctx, &request.DeleteOrg{
			ServerId: testServerId,
			Id:       "Test_org",
		})

		_, _ = infraProxy.DeleteServer(ctx, &request.DeleteServer{
			Id: testServerId,
		})

	})

}

func TestGetOrg(t *testing.T) {
	t.Run("Get Org which is present by default Server", func(t *testing.T) {
		req := &request.GetOrg{
			Id:       autoDeployedChefOrganizationID,
			ServerId: autoDeployedChefServerID,
		}

		res, err := infraProxy.GetOrg(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, autoDeployedChefOrganizationID, res.GetOrg().Id)
		assert.Equal(t, "test", res.GetOrg().Name)
    })

	t.Run("Get Org when server Id is not present", func(t *testing.T) {
		req := &request.GetOrg{
			Id:       autoDeployedChefOrganizationID,
			
		}
        res, err := infraProxy.GetOrg(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
		
    })

	t.Run("Get Org when Id is not present but server id is present", func(t *testing.T) {
		req := &request.GetOrg{
			ServerId: autoDeployedChefServerID,
		}
        res, err := infraProxy.GetOrg(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
		
    })

}