package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetRoles(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the roles search based on these records.
	// rpc GetRoles (request.Roles) returns (response.Roles)
	ctx := context.Background()

	//Adds roles
	addRoles(ctx, 10)

	req := &request.Roles{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	t.Run("Roles list without a search params", func(t *testing.T) {
		// Default per_page 1000 is set in backend
		perPage := int32(1000)
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, int(res.Page), 0)
		if res.Total <= perPage {
			assert.Equal(t, int(res.Total), len(res.Roles))
		} else {
			assert.Equal(t, perPage, len(res.Roles))
		}
	})

	t.Run("Roles list with a per_page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 1, len(res.Roles))
	})

	t.Run("Roles list with a page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "*:*",
			Page:    1,
			PerPage: 1,
		}
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, int(res.Page))
	})

	t.Run("Roles list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Roles))
	})

	t.Run("Roles list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Roles))
	})

	t.Run("Roles list with a valid query search param", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-%d", time.Now().Nanosecond())
		createReq := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
		}
	    role, err := infraProxy.CreateRole(ctx, createReq)
		assert.NoError(t, err)
		assert.NotNil(t, role)

		req.SearchQuery = &request.SearchQuery{
			Q:       fmt.Sprintf("name:%s", name),
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetRoles(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 1, int(res.Total))
		assert.Equal(t, name, res.Roles[0].GetName())
	})
}

// Adds roles records
func addRoles(ctx context.Context, n int) int {
	total := 0
	for i := 0; i < n; i++ {
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        fmt.Sprintf("chef-load-role-%d", time.Now().Nanosecond()),
			Description: "auto generated role",
		}
		_, err := infraProxy.CreateRole(ctx, req)

		if err == nil {
			total++
		}
	}

	return total
}
