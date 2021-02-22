package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetNode(t *testing.T) {
	ctx := context.Background()
	t.Run("when the node exists, return the node successfully", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())

		reqNode := &request.NodeObject{
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
		req := &request.Node{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     fmt.Sprintf("no-node-%d", time.Now().Nanosecond()),
		}

		res, err := infraProxy.GetNode(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)
	})
}

func TestUpdateNodeTags(t *testing.T) {
	ctx := context.Background()
	t.Run("when add tags request with valid params", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeObject{
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

		// Add tag3 into existing the tags
		req.Tags = []string{"tag3"}
		req.Action = "add"
		res, err = infraProxy.UpdateNodeTags(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, []string{"tag1", "tag2", "tag3"}, res.Tags)
	})

	t.Run("when set tags request with valid params", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeObject{
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

	t.Run("when delete tags request with valid params", func(t *testing.T) {
		name := fmt.Sprintf("node-%d", time.Now().Nanosecond())
		reqNode := &request.NodeObject{
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
}
