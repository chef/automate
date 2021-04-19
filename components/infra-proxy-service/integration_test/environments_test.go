package integration_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/protobuf/types/known/structpb"
)

func TestGetEnvironments(t *testing.T) {
	// rpc GetEnvironments (request.Environments) returns (response.Environments)
	req := &request.Environments{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	t.Run("Environments list without a search params", func(t *testing.T) {
		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.GreaterOrEqual(t, int(res.Total), 0)

	})

	t.Run("Environments list with a per_page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}

		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 1, len(res.Environments))
		assert.GreaterOrEqual(t, int(res.Total), 0)
	})

	t.Run("Environments list with a page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
			Page:    1,
		}

		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, int(res.Page))
		assert.Equal(t, 1, len(res.Environments))
		assert.GreaterOrEqual(t, int(res.Total), 0)
	})

	t.Run("Environments list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Environments))
	})

	t.Run("Environments list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
	})

	t.Run("Environments list with a valid query search param", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		createReq := &request.CreateEnvironment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		env, err := infraProxy.CreateEnvironment(ctx, createReq)
		assert.NoError(t, err)
		assert.NotNil(t, env)

		req.SearchQuery = &request.SearchQuery{
			Q:       fmt.Sprintf("name:%s", name),
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetEnvironments(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 1, int(res.Total))
		assert.Equal(t, name, res.Environments[0].GetName())
	})
}

func TestGetEnvironment(t *testing.T) {
	// rpc GetEnvironment (request.Environment) returns (response.Environment)
	t.Run("when the environment exists, return the environment successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		creReq := &request.CreateEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef environment",
			CookbookVersions: map[string]string{
				"audit":       ">= 9.5.0",
				"chef-client": "= 12.3.3",
			},
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}
		envRes, err := infraProxy.CreateEnvironment(ctx, creReq)
		assert.NoError(t, err)
		assert.NotNil(t, envRes)

		req := &request.Environment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.GetEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, "auto generate chef environment", res.GetDescription())
		assert.Equal(t, creReq.CookbookVersions, res.CookbookVersions)
	})
}

func TestCreateEnvironment(t *testing.T) {
	// rpc GetEnvironment (request.Environment) returns (response.Environment)
	t.Run("when a valid environment is submitted, creates the new environment successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		req := &request.CreateEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef environment",
			CookbookVersions: map[string]string{
				"audit":       ">= 9.5.0",
				"chef-client": "= 12.3.3",
			},
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
			OverrideAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "override"}},
				},
			},
		}
		res, err := infraProxy.CreateEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, "auto generate chef environment", res.GetDescription())
		assert.Equal(t, req.CookbookVersions, res.CookbookVersions)
		assert.Equal(t, "{\"attribute1\":\"value\"}", res.DefaultAttributes)
		assert.Equal(t, "{\"attribute1\":\"override\"}", res.OverrideAttributes)
	})

	t.Run("when the environment exists, raise the error environment already exists", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		req := &request.CreateEnvironment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.CreateEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

		nextRes, err := infraProxy.CreateEnvironment(ctx, req)
		assert.Nil(t, nextRes)
		assert.Error(t, err, "Environment already exists")
		grpctest.AssertCode(t, codes.AlreadyExists, err)
	})

	t.Run("when the environment required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		req := &request.CreateEnvironment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.CreateEnvironment(ctx, req)
		assert.Nil(t, res)
		assert.Error(t, err, "must supply environment name")
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})
}

func TestUpdateEnvironment(t *testing.T) {
	// rpc UpdateEnvironment (request.UpdateEnvironment) returns (response.Environment)
	t.Run("when a valid environment is submitted, updates the environment successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		req := &request.CreateEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef environment",
			CookbookVersions: map[string]string{
				"audit":       ">= 9.5.0",
				"chef-client": "= 12.3.3",
			},
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}
		res, err := infraProxy.CreateEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, "auto generate chef environment", res.GetDescription())
		assert.Equal(t, req.CookbookVersions, res.CookbookVersions)

		updateReq := &request.UpdateEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "updated chef environment",
			CookbookVersions: map[string]string{
				"audit":       ">= 9.5.0",
				"chef-client": "= 12.3.3",
				"aix":         "> 1.3.3",
			},
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}

		updateEnv, err := infraProxy.UpdateEnvironment(ctx, updateReq)
		assert.NoError(t, err)
		assert.NotNil(t, updateEnv)
		assert.Equal(t, name, updateEnv.GetName())
		assert.Equal(t, updateReq.Description, updateEnv.GetDescription())
		assert.Equal(t, updateReq.CookbookVersions, updateEnv.CookbookVersions)
	})
}

func TestDeleteEnvironment(t *testing.T) {
	// rpc DeleteEnvironment (request.Environment) returns (response.Environment)
	t.Run("when a valid environment is submitted, deletes the environment successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond())
		req := &request.CreateEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef environment",
			CookbookVersions: map[string]string{
				"audit":       ">= 9.5.0",
				"chef-client": "= 12.3.3",
			},
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}
		res, err := infraProxy.CreateEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		// Deletes the created environment
		delRes, err := infraProxy.DeleteEnvironment(ctx, &request.Environment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.NoError(t, err)
		assert.NotNil(t, delRes)
	})
}

// Adds environments records
func addEnvironments(n int) int {
	total := 0
	for i := 0; i < n; i++ {
		req := &request.CreateEnvironment{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     fmt.Sprintf("chef-environment-%d", time.Now().Nanosecond()),
		}
		_, err := infraProxy.CreateEnvironment(ctx, req)

		if err == nil {
			total++
		}
	}

	return total
}
