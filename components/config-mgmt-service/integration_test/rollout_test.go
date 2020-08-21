package integration_test

import (
	"context"
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/api/external/cfgmgmt/response"
	ireq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCreateRolloutFromMetadataAPI(t *testing.T) {
	ctx := context.Background()

	cleanup := func(t *testing.T) {
		err := cfgmgmt.ClearPg()
		require.NoError(t, err)
	}

	t.Run("create fails when the request is blank", func(t *testing.T) {
		cleanup(t)
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
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyName = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_name\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyNodeGroup", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyNodeGroup = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_node_group\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyRevisionId", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyRevisionId = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_revision_id\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyDomainUrl", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyDomainUrl = ""
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_domain_url\" were blank")
	})

	t.Run("create succeeds with a minimum valid request", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		created, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)
		assert.Equal(t, "example-policy-name", created.PolicyName)
		assert.Equal(t, "example-policy-node-group", created.PolicyNodeGroup)
		assert.Equal(t, "abc123", created.PolicyRevisionId)
		assert.Equal(t, "https://chef-server.example/organizations/example_org", created.PolicyDomainUrl)
		s, err := time.Parse(time.RFC3339, created.StartTime)
		require.NoError(t, err)
		assert.WithinDuration(t, time.Now().UTC(), s, 1*time.Minute)
		assert.Empty(t, created.EndTime)
	})

	t.Run("after creating a rollout, fetch by id succeeds", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		created, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)
		fetched, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: created.Id})
		assert.Equal(t, "example-policy-name", fetched.PolicyName)
		assert.Equal(t, "example-policy-node-group", fetched.PolicyNodeGroup)
		assert.Equal(t, "abc123", fetched.PolicyRevisionId)
		assert.Equal(t, "https://chef-server.example/organizations/example_org", fetched.PolicyDomainUrl)
		s, err := time.Parse(time.RFC3339, fetched.StartTime)
		require.NoError(t, err)
		assert.WithinDuration(t, time.Now().UTC(), s, 1*time.Minute)
		assert.Empty(t, fetched.EndTime)
	})

	t.Run("create succeeds with all available fields filled", func(t *testing.T) {
		cleanup(t)
		req := request.CreateRollout{
			PolicyName:           "example-policy-name-full",
			PolicyNodeGroup:      "example-policy-node-group",
			PolicyRevisionId:     "abc123",
			PolicyDomainUrl:      "https://chef-server.example/organizations/example_org",
			PolicyDomainUsername: "bobo",
			ScmType:              request.SCMType_SCM_TYPE_GIT,
			ScmWebType:           request.SCMWebType_SCM_WEB_TYPE_GITHUB,
			ScmAuthorName:        "Bobo Tiberius Clown",
			ScmAuthorEmail:       "bobo@example.com",
			PolicyScmUrl:         "git@github.com:chef/automate.git",
			PolicyScmWebUrl:      "https://github.com/chef/automate",
			PolicyScmCommit:      "a2a344e6804629de85ffa50e84caad18ac42cf50",
			Description:          "install winamp",
			CiJobId:              "buildkite/chef-automate-master-verify#11875",
			CiJobUrl:             "https://buildkite.com/chef-oss/chef-automate-master-verify/builds/11875",
		}
		createResponse, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)
		assert.NotEmpty(t, createResponse.Id)
		_, err = strconv.Atoi(createResponse.Id)
		assert.NoError(t, err)
		checkFields := func(res *response.Rollout) {
			assert.Equal(t, "example-policy-name-full", res.PolicyName)
			assert.Equal(t, "example-policy-node-group", res.PolicyNodeGroup)
			assert.Equal(t, "abc123", res.PolicyRevisionId)
			assert.Equal(t, "https://chef-server.example/organizations/example_org", res.PolicyDomainUrl)
			assert.Equal(t, "bobo", res.PolicyDomainUsername)
			assert.Equal(t, response.SCMType_SCM_TYPE_GIT, res.ScmType)
			assert.Equal(t, response.SCMWebType_SCM_WEB_TYPE_GITHUB, res.ScmWebType)
			assert.Equal(t, "git@github.com:chef/automate.git", res.PolicyScmUrl)
			assert.Equal(t, "https://github.com/chef/automate", res.PolicyScmWebUrl)
			assert.Equal(t, "a2a344e6804629de85ffa50e84caad18ac42cf50", res.PolicyScmCommit)
			assert.Equal(t, "Bobo Tiberius Clown", res.ScmAuthorName)
			assert.Equal(t, "bobo@example.com", res.ScmAuthorEmail)
			assert.Equal(t, "install winamp", res.Description)
			assert.Equal(t, "buildkite/chef-automate-master-verify#11875", res.CiJobId)
			assert.Equal(t, "https://buildkite.com/chef-oss/chef-automate-master-verify/builds/11875", res.CiJobUrl)
			assert.NotEmpty(t, res.StartTime)
			s, err := time.Parse(time.RFC3339, res.StartTime)
			require.NoError(t, err)
			assert.WithinDuration(t, time.Now().UTC(), s, 1*time.Minute)
			assert.Empty(t, res.EndTime)
		}
		checkFields(createResponse)
		fetchedById, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: createResponse.Id})
		require.NoError(t, err)
		checkFields(fetchedById)
	})

	t.Run("creating a second rollout with a different target node segment doesn't set end_time", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		req2.PolicyName = "example-policy-two"
		created1, err := cfgmgmt.CreateRollout(ctx, &req1)
		require.NoError(t, err)
		created2, err := cfgmgmt.CreateRollout(ctx, &req2)
		require.NoError(t, err)

		fetched1, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: created1.Id})
		require.NoError(t, err)
		fetched2, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: created2.Id})
		require.NoError(t, err)

		assert.Empty(t, fetched1.EndTime)
		assert.Empty(t, fetched2.EndTime)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 2)
	})
	t.Run("creating a second rollout with the same target node segment sets end_time on the older one", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		req2.PolicyRevisionId = "def456"
		created1, err := cfgmgmt.CreateRollout(ctx, &req1)
		require.NoError(t, err)
		created2, err := cfgmgmt.CreateRollout(ctx, &req2)
		require.NoError(t, err)

		fetched1, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: created1.Id})
		require.NoError(t, err)
		fetched2, err := cfgmgmt.GetRolloutById(ctx, &request.RolloutById{RolloutId: created2.Id})
		require.NoError(t, err)

		assert.NotEmpty(t, fetched1.EndTime, "EndTime was not set on the rollout when it was replaced with a newer one")
		assert.Empty(t, fetched2.EndTime)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 2)
	})
	t.Run("creating a rollout with the same target node segment and policy revision updates the attributes", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := request.CreateRollout{
			PolicyName:           req1.PolicyName,
			PolicyNodeGroup:      req1.PolicyNodeGroup,
			PolicyRevisionId:     req1.PolicyRevisionId,
			PolicyDomainUrl:      req1.PolicyDomainUrl,
			PolicyDomainUsername: "bobo",
			ScmType:              request.SCMType_SCM_TYPE_GIT,
			ScmWebType:           request.SCMWebType_SCM_WEB_TYPE_GITHUB,
			ScmAuthorName:        "Bobo Tiberius Clown",
			ScmAuthorEmail:       "bobo@example.com",
			PolicyScmUrl:         "git@github.com:chef/automate.git",
			PolicyScmWebUrl:      "https://github.com/chef/automate",
			PolicyScmCommit:      "a2a344e6804629de85ffa50e84caad18ac42cf50",
			Description:          "install winamp",
			CiJobId:              "buildkite/chef-automate-master-verify#11875",
			CiJobUrl:             "https://buildkite.com/chef-oss/chef-automate-master-verify/builds/11875",
		}
		initialRolloutValues, err := cfgmgmt.CreateRollout(ctx, &req1)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req2)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 1)

		updated := list.Rollouts[0]

		// These four things are the "natural key" of the rollout, should not change
		assert.Equal(t, req1.PolicyName, updated.PolicyName)
		assert.Equal(t, req1.PolicyNodeGroup, updated.PolicyNodeGroup)
		assert.Equal(t, req1.PolicyRevisionId, updated.PolicyRevisionId)
		assert.Equal(t, req1.PolicyDomainUrl, updated.PolicyDomainUrl)

		// These are all added by the second request
		assert.Equal(t, "bobo", updated.PolicyDomainUsername)
		assert.Equal(t, response.SCMType_SCM_TYPE_GIT, updated.ScmType)
		assert.Equal(t, response.SCMWebType_SCM_WEB_TYPE_GITHUB, updated.ScmWebType)
		assert.Equal(t, "git@github.com:chef/automate.git", updated.PolicyScmUrl)
		assert.Equal(t, "https://github.com/chef/automate", updated.PolicyScmWebUrl)
		assert.Equal(t, "a2a344e6804629de85ffa50e84caad18ac42cf50", updated.PolicyScmCommit)
		assert.Equal(t, "Bobo Tiberius Clown", updated.ScmAuthorName)
		assert.Equal(t, "bobo@example.com", updated.ScmAuthorEmail)
		assert.Equal(t, "install winamp", updated.Description)
		assert.Equal(t, "buildkite/chef-automate-master-verify#11875", updated.CiJobId)
		assert.Equal(t, "https://buildkite.com/chef-oss/chef-automate-master-verify/builds/11875", updated.CiJobUrl)

		// Updating the other attributes should not modify the start time
		assert.NotEmpty(t, updated.StartTime)
		assert.Equal(t, initialRolloutValues.StartTime, updated.StartTime)

	})
	t.Run("rolling back to an old policy revision works", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		req2.PolicyRevisionId = "def456"
		req3 := reqWithAllRequiredFields

		_, err := cfgmgmt.CreateRollout(ctx, &req1)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req2)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req3)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 3)
	})

	t.Run("rolling back and forth several times works", func(t *testing.T) {
		// This test checks that our database constraints allow more than one rollback
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		req2.PolicyRevisionId = "def456"
		req3 := reqWithAllRequiredFields
		req4 := reqWithAllRequiredFields
		req4.PolicyRevisionId = "def456"
		req5 := reqWithAllRequiredFields

		_, err := cfgmgmt.CreateRollout(ctx, &req1)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req2)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req3)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req4)
		require.NoError(t, err)
		_, err = cfgmgmt.CreateRollout(ctx, &req5)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 5)
	})
}

func TestCreateRolloutFromAction(t *testing.T) {
	ctx := context.Background()

	cleanup := func(t *testing.T) {
		err := cfgmgmt.ClearPg()
		require.NoError(t, err)
	}

	t.Run("create fails when the request is blank", func(t *testing.T) {
		cleanup(t)
		req := ireq.PolicyUpdateAction{}
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
	})

	reqWithAllRequiredFields := ireq.PolicyUpdateAction{
		PolicyName:         "example-policy-name",
		PolicyGroup:        "example-policy-node-group",
		PolicyRevisionId:   "abc123",
		ChefServerFqdn:     "chef-server.example",
		ChefServerOrgname:  "example_org",
		ChefServerUsername: "exampleUser",
	}

	t.Run("create fails when the request has a blank PolicyName", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyName = ""
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_name\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyNodeGroup", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyGroup = ""
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_group\" were blank")
	})

	t.Run("create fails when the request has a blank PolicyRevisionId", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.PolicyRevisionId = ""
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"policy_revision_id\" were blank")
	})

	t.Run("create fails when the request has a blank ChefServerFqdn", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.ChefServerFqdn = ""
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"chef_server_fqdn\" were blank")
	})

	t.Run("create fails when the request has a blank ChefServerOrgname", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		req.ChefServerOrgname = ""
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.Error(t, err)
		assert.Contains(t, err.Error(), "required field(s) \"chef_server_orgname\" were blank")
	})

	t.Run("create succeeds with a minimum valid request", func(t *testing.T) {
		cleanup(t)
		req := reqWithAllRequiredFields
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 1)

		created := list.Rollouts[0]
		assert.Equal(t, "example-policy-name", created.PolicyName)
		assert.Equal(t, "example-policy-node-group", created.PolicyNodeGroup)
		assert.Equal(t, "abc123", created.PolicyRevisionId)
		assert.Equal(t, "https://chef-server.example/organizations/example_org", created.PolicyDomainUrl)
		s, err := time.Parse(time.RFC3339, created.StartTime)
		require.NoError(t, err)
		assert.WithinDuration(t, time.Now().UTC(), s, 1*time.Minute)
		assert.Empty(t, created.EndTime)
	})

	t.Run("creating a rollout with the same target node segment and policy revision is a successful no-op", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req1)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 1)
		firstFetchedRollout := list.Rollouts[0]

		_, err = cfgmgmt.HandlePolicyUpdateAction(ctx, &req2)
		// any error here would just spam the Chef Server logs, we should only do
		// that for a problem that needs attention.
		require.NoError(t, err)

		list2, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list2.Rollouts, 1)
		secondFetchedRollout := list2.Rollouts[0]

		assert.Equal(t, firstFetchedRollout, secondFetchedRollout)
	})

	t.Run("creating a second rollout with the same target node segment sets end_time on the older one", func(t *testing.T) {
		cleanup(t)
		req1 := reqWithAllRequiredFields
		req2 := reqWithAllRequiredFields
		req2.PolicyRevisionId = "def456"
		_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, &req1)
		require.NoError(t, err)
		_, err = cfgmgmt.HandlePolicyUpdateAction(ctx, &req2)
		require.NoError(t, err)

		list, err := cfgmgmt.GetRollouts(ctx, &request.Rollouts{})
		require.NoError(t, err)
		assert.Len(t, list.Rollouts, 2)

		newestRollout := list.Rollouts[0]
		olderRollout := list.Rollouts[1]

		assert.NotEmpty(t, olderRollout.EndTime, "EndTime was not set on the rollout when it was replaced with a newer one")
		assert.Empty(t, newestRollout.EndTime)

	})
}
