package integration_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/protobuf/types/known/structpb"
)

func TestGetNodes(t *testing.T) {
	req := &request.Nodes{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	t.Run("nodes list with a per_page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}

		res, err := infraProxy.GetNodes(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.GreaterOrEqual(t, len(res.Nodes), 0)
		assert.GreaterOrEqual(t, int(res.Total), 0)
	})

	t.Run("Nodes list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetNodes(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Nodes))
	})

	t.Run("Nodes list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetNodes(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
	})
}
func TestGetNode(t *testing.T) {
	t.Run("when the node exists, return the node successfully", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.Node{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.GetNode(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, res.Name, name)
	})

	t.Run("when the node does not exist, return node not found", func(t *testing.T) {
		name := fmt.Sprintf("no-node-%d", time.Now().Nanosecond())
		req := &request.Node{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		res, err := infraProxy.GetNode(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
		require.Contains(t, err.Error(), fmt.Sprintf("node '%s' not found", name))
		grpctest.AssertCode(t, codes.NotFound, err)
	})
}

func TestUpdateNodeTags(t *testing.T) {
	t.Run("when add, adds values to the current group of tags", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Add tag3 into existing tags
		req.Tags = []string{"tag3"}
		req.Action = "add"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2", "tag3"}, res.Tags)
	})

	t.Run("when set, replaces tags with specified values", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Set the tags with the tag reset1
		req.Tags = []string{"reset1"}
		req.Action = "set"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"reset1"}, res.Tags)
	})

	t.Run("when delete, removes tags from the current group of values", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Deletes the tag2
		req.Tags = []string{"tag2"}
		req.Action = "delete"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1"}, res.Tags)
	})

	t.Run("when attempting to delete a non-existent tag, leaves tag group unchanged", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Deletes the non-existent tag
		req.Tags = []string{"non-existent"}
		req.Action = "delete"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)
	})

	t.Run("when attempting to delete all tags, updates tag group to the empty list", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Delete all the existing tags
		req.Tags = []string{"tag1", "tag2"}
		req.Action = "delete"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{}, res.Tags)
	})

	t.Run("when attempting to delete some existing and some non-existing tags simultaneously, deletes the existing tags only", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Deletes the existing tag tag1 and non-existing tag non-existent
		req.Tags = []string{"tag1", "non-existent"}
		req.Action = "delete"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag2"}, res.Tags)
	})

	t.Run("when attempting to set tags to empty list, updates tag group to the empty list", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeTags{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Action:   "add",
			Tags:     []string{"tag1", "tag2"},
		}
		res, err := infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2"}, res.Tags)

		// Set tags to empty list,
		req.Tags = []string{}
		req.Action = "set"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{}, res.Tags)
	})
}

func TestUpdateNodeEnvironment(t *testing.T) {
	t.Run("when valid the environment value is set, updates the existing node environment", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "_default",
		}
		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)
		assert.Equal(t, "_default", node.Environment)

		req := &request.UpdateNodeEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "dev",
		}
		res, err := infraProxy.UpdateNodeEnvironment(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

		assert.Equal(t, "dev", res.Environment)
	})

	t.Run("when attempting to set the environment to empty value, raise invalid argument error", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "_default",
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeEnvironment{
			ServerId:    autoDeployedChefServerID,
			OrgId:       autoDeployedChefOrganizationID,
			Name:        name,
			Environment: "",
		}
		res, err := infraProxy.UpdateNodeEnvironment(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

}
func TestUpdateNodeAttributes(t *testing.T) {
	t.Run("when valid the attributes value is set, updates the node attributes", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeAttributes{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Attributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attr1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value1"}},
				},
			},
		}
		res, err := infraProxy.UpdateNodeAttributes(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "{\"attr1\":\"value1\"}", res.Attributes)
	})

	t.Run("when attempting to set the attributes to empty value, updates attributes with empty value", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			NormalAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attr1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value1"}},
				},
			},
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeAttributes{
			ServerId:   autoDeployedChefServerID,
			OrgId:      autoDeployedChefOrganizationID,
			Name:       name,
			Attributes: &structpb.Struct{},
		}
		res, err := infraProxy.UpdateNodeAttributes(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "{}", res.Attributes)
	})

	t.Run("when attempting to set the attributes to nil, updates attributes with empty value", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeDetails{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			NormalAttributes: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"attr1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: "value1"}},
				},
			},
		}

		node, err := infraProxy.CreateNode(ctx, reqNode)
		assert.NoError(t, err)
		assert.NotNil(t, node)

		req := &request.UpdateNodeAttributes{
			ServerId:   autoDeployedChefServerID,
			OrgId:      autoDeployedChefOrganizationID,
			Name:       name,
			Attributes: nil,
		}
		res, err := infraProxy.UpdateNodeAttributes(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "{}", res.Attributes)
	})
}
