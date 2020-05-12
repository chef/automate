package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRolloutCreate(t *testing.T) {
	ctx := context.Background()
	t.Run("create fails when the request is blank", func(t *testing.T) {
		req := request.CreateRollout{}
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
	})

	reqWithAllRequiredFields := request.CreateRollout{
		PolicyName:       "example-policy-name",
		PolicyNodeGroup:  "example-policy-node-group",
		PolicyRevisionId: "abc123",
		PolicyDomainUrl:  "https://chef-server.example/organizations/example_org",
	}

	t.Run("create fails when the request has a blank PolicyName", func(t *testing.T) {
		req := reqWithAllRequiredFields
		req.PolicyName = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_name\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyNodeGroup", func(t *testing.T) {
		req := reqWithAllRequiredFields
		req.PolicyNodeGroup = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_node_group\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyRevisionId", func(t *testing.T) {
		req := reqWithAllRequiredFields
		req.PolicyRevisionId = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_revision_id\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyDomainUrl", func(t *testing.T) {
		req := reqWithAllRequiredFields
		req.PolicyDomainUrl = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_domain_url\" were blank")
	})
}
