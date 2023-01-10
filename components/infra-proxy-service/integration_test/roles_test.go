package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/protobuf/types/known/structpb"
)

func TestGetRoles(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the roles search based on these records.
	// rpc GetRoles (request.Roles) returns (response.Roles)
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
		name := fmt.Sprintf("chef-load-3-role-%d", time.Now().Nanosecond())
		createReq := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
		}
		role, err := infraProxy.CreateRole(ctx, createReq)
		assert.NoError(t, err)
		assert.NotNil(t, role)
		time.Sleep(2 * time.Second)

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

func TestGetRole(t *testing.T) {
	ctx := context.Background()
	t.Run("when the role exists, returns the org successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-2-role-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
			RunList:     []string{"recipe[audit::default]", "recipe[chef-client::default]"},
		}
		role, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, role)

		res, err := infraProxy.GetRole(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.NoError(t, err)
		assert.NotNil(t, res)
	})

	t.Run("when the role required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		resp, err := infraProxy.GetRole(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "",
		})
		assert.Error(t, err)
		assert.Nil(t, resp)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

	t.Run("when the role does not exists, return role not found", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-2-role-%d", time.Now().Nanosecond())
		res, err := infraProxy.GetRole(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.Error(t, err)
		assert.Nil(t, res)
		grpctest.AssertCode(t, codes.NotFound, err)
	})
}

func TestGetRoleEnvironments(t *testing.T) {
	t.Run("when the role exists, returns the role environments successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-4-%d", time.Now().Nanosecond())
		envRunlist := &request.EnvRunList{
			Name:    "production",
			RunList: []string{"recipe[audit::default]", "recipe[chef-client::default]"},
		}
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
			RunList:     []string{"recipe[audit::default]", "recipe[chef-client::default]"},
			EnvRunLists: []*request.EnvRunList{envRunlist},
		}
		role, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, role)

		res, err := infraProxy.GetRoleEnvironments(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Contains(t, res.Environments, "_default")
	})

	t.Run("when the role created without specific environment runlist, returns _default environment successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-5-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
			RunList:     []string{"recipe[audit::default]", "recipe[chef-client::default]"},
		}
		role, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, role)

		res, err := infraProxy.GetRoleEnvironments(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Contains(t, res.Environments, "_default")
	})

	t.Run("when the role required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		resp, err := infraProxy.GetRoleEnvironments(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "",
		})
		assert.Error(t, err)
		assert.Nil(t, resp)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

	t.Run("when the role does not exists, return role not found", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-6-%d", time.Now().Nanosecond())
		res, err := infraProxy.GetRoleEnvironments(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.Error(t, err)
		assert.Nil(t, res)
		grpctest.AssertCode(t, codes.NotFound, err)
	})
}

func TestGetRoleExpandedRunList(t *testing.T) {
	t.Run("when the role exists, returns the role environments successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-7-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generated role",
			RunList:     []string{"recipe[audit::default]", "recipe[chef-client::default]"},
		}
		role, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, role)

		// _default environment expanded runlist
		res, err := infraProxy.GetRoleExpandedRunList(ctx, &request.ExpandedRunList{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "_default",
		})
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "_default", res.Id)
		assert.Equal(t, len(res.RunList), 2)
	})

	t.Run("when the role required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		resp, err := infraProxy.GetRoleExpandedRunList(ctx, &request.ExpandedRunList{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "",
		})
		assert.Error(t, err)
		assert.Nil(t, resp)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

	t.Run("when the role does not exists, return role not found", func(t *testing.T) {
		name := fmt.Sprintf("chef-load-role-%d", time.Now().Nanosecond())
		res, err := infraProxy.GetRoleExpandedRunList(ctx, &request.ExpandedRunList{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "_default",
		})
		assert.Error(t, err)
		assert.Nil(t, res)
		grpctest.AssertCode(t, codes.NotFound, err)
	})
}

func TestCreateRole(t *testing.T) {
	t.Run("when a valid role is submitted, creates the new role successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-role-8-test-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef role",
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
		res, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, "auto generate chef role", res.GetDescription())
		assert.Equal(t, "{\"attribute1\":\"value\"}", res.DefaultAttributes)
		assert.Equal(t, "{\"attribute1\":\"override\"}", res.OverrideAttributes)
	})

	t.Run("when the role exists, raise the error role already exists", func(t *testing.T) {
		name := fmt.Sprintf("chef-role-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

		nextRes, err := infraProxy.CreateRole(ctx, req)
		assert.Nil(t, nextRes)
		assert.Error(t, err, "Role already exists")
		grpctest.AssertCode(t, codes.AlreadyExists, err)
	})

	t.Run("when the role required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		req := &request.CreateRole{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.CreateRole(ctx, req)
		assert.Nil(t, res)
		assert.Error(t, err, "must supply role name")
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})
}

func TestUpdateRole(t *testing.T) {
	// rpc UpdateRole (request.UpdateRole) returns (response.UpdateRole)
	t.Run("when a valid role is submitted, updates the role successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-role-9-test%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef role",
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}
		res, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, "auto generate chef role", res.GetDescription())

		updateReq := &request.UpdateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "updated chef role",
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}

		updateEnv, err := infraProxy.UpdateRole(ctx, updateReq)
		assert.NoError(t, err)
		assert.NotNil(t, updateEnv)
		assert.Equal(t, name, updateEnv.GetName())
		assert.Equal(t, updateReq.Description, updateEnv.GetDescription())
	})
}

func TestDeleteRole(t *testing.T) {
	// rpc DeleteRole (request.Role) returns (response.Role)
	t.Run("when a valid role is submitted, deletes the role successfully", func(t *testing.T) {
		name := fmt.Sprintf("chef-role-%d", time.Now().Nanosecond())
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Description: "auto generate chef role",
			DefaultAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attribute1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value"}},
				},
			},
		}
		res, err := infraProxy.CreateRole(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		// Deletes the created role
		delRes, err := infraProxy.DeleteRole(ctx, &request.Role{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		})
		assert.NoError(t, err)
		assert.NotNil(t, delRes)
	})
}

// Adds roles records
func addRoles(n int) int {
	total := 0
	for i := 0; i < n; i++ {
		req := &request.CreateRole{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        fmt.Sprintf("chef-load-role-%d", time.Now().Nanosecond()),
			Description: "auto generated role",
			RunList:     []string{"recipe[audit::default]", "recipe[chef-client::default]"},
		}
		_, err := infraProxy.CreateRole(ctx, req)

		if err == nil {
			total++
		}
	}

	return total
}
