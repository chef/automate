package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	internalReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
)

var (
	now         = time.Now()
	nowPlus1Min = time.Now().Add(time.Second * 60)
	nodeID      = newUUID()
)

const (
	chefServer1            = "chef1.example"
	chefOrg                = "integration_test_org"
	policyName1            = "one-testing-policyName"
	policyGroup1           = "one-testing-policyGroup"
	policyGroup2           = "two-testing-policyGroup"
	excludedPolicyName     = "policy-not-using-rollouts-pn"
	excludedPolicyGroup    = "policy-not-using-rollouts-pg"
	rollout1revID          = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	rollout2revID          = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
	rollout3revID          = "ccccccccccccccccccccccccccccccccccccccccc"
	rollout4revID          = "ddddddddddddddddddddddddddddddddddddddddd"
	rollout5revID          = "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
	rollout6revID          = "fffffffffffffffffffffffffffffffffffffffff"
	excludedPolicyRevision = "00000000000000000000000000000000000000000"
)

// validation and cleanup of Chef Server URLs especially, but also pname/group
// need some way to do a diagnostic on the matching-ness of the CCR vs. pg rollout data

func TestListNodeSegmentsWithRolloutProgress(t *testing.T) {
	ctx := context.Background()

	cleanup := func(t *testing.T) {
		err := cfgmgmt.ClearPg()
		require.NoError(t, err)
		suite.DeleteAllDocuments()
	}

	rolloutWithAllRequiredFields := request.CreateRollout{
		PolicyName:       policyName1,
		PolicyNodeGroup:  policyGroup1,
		PolicyRevisionId: rollout1revID,
		PolicyDomainUrl:  fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg),
	}
	/*
		Fresh installs, not using infra, etc.
	*/

	t.Run("with no CCR data and no rollout records", func(t *testing.T) {
		// return is empty, no error
		cleanup(t)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)
		assert.Len(t, res.NodeSegmentRolloutProgress, 0)
	})

	/*
		Automate Infra in-use but not for change tracking
	*/
	t.Run("with non-policyfile CCR data and no rollout records", func(t *testing.T) {
		// return is empty, no error
		cleanup(t)
		suite.DeleteAllDocuments()

		ccrDataIn := newCCR()
		ccrDataIn.PolicyName = ""
		ccrDataIn.PolicyGroup = ""
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)
		assert.Len(t, res.NodeSegmentRolloutProgress, 0)
	})

	t.Run("with policyfile CCR data and no rollout records", func(t *testing.T) {
		// return is empty, no error
		cleanup(t)

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = "abcdef123456"
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)
		assert.Len(t, res.NodeSegmentRolloutProgress, 0)
	})

	/*
	  Various cases of having rollouts with no related CCR data
	*/
	t.Run("with one rollout, no CCR data at all", func(t *testing.T) {
		// response has the rollout but the node counts are all zero
		cleanup(t)

		req := rolloutWithAllRequiredFields
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, req.PolicyRevisionId)
		assert.Len(t, rolloutProgress.PreviousRollouts, 0)
	})

	t.Run("with one rollout, there are CCR records but they aren't related", func(t *testing.T) {
		// response has the rollout but the node counts are all zero
		cleanup(t)

		req := rolloutWithAllRequiredFields
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision
		ccrDataIn.PolicyName = excludedPolicyName
		ccrDataIn.PolicyGroup = excludedPolicyGroup
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, req.PolicyRevisionId)
		assert.Len(t, rolloutProgress.PreviousRollouts, 0)
	})

	t.Run("with 5 rollouts in one node segment, but the only unrelated CCRs in the db", func(t *testing.T) {
		// response has 5 rollouts correctly grouped in the node segment
		cleanup(t)

		revIdList := []string{rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		for _, id := range revIdList {
			req := rolloutWithAllRequiredFields
			req.PolicyRevisionId = id
			_, err := cfgmgmt.CreateRollout(ctx, &req)
			require.NoError(t, err)
		}

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision
		ccrDataIn.PolicyName = excludedPolicyName
		ccrDataIn.PolicyGroup = excludedPolicyGroup
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress.PreviousRollouts, 4)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
	})

	t.Run("with 6 rollouts in one node segment, but the only unrelated CCRs in the db", func(t *testing.T) {
		// response has the 5 most recent rollouts correctly grouped in the node segment
		cleanup(t)

		revIdList := []string{rollout6revID, rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		for _, id := range revIdList {
			req := rolloutWithAllRequiredFields
			req.PolicyRevisionId = id
			_, err := cfgmgmt.CreateRollout(ctx, &req)
			require.NoError(t, err)
		}

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision
		ccrDataIn.PolicyName = excludedPolicyName
		ccrDataIn.PolicyGroup = excludedPolicyGroup
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress.PreviousRollouts, 4)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
	})

	t.Run("2 segments with 6 rollouts each, but the only unrelated CCRs in the db", func(t *testing.T) {
		// response has 2 rollouts with the 5 most recent rollouts correctly grouped for each
		cleanup(t)

		revIdList := []string{rollout6revID, rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		policyGroups := []string{policyGroup1, policyGroup2}

		for _, pgroup := range policyGroups {
			for _, id := range revIdList {
				req := rolloutWithAllRequiredFields
				req.PolicyRevisionId = id
				req.PolicyNodeGroup = pgroup
				_, err := cfgmgmt.CreateRollout(ctx, &req)
				require.NoError(t, err)
			}
		}

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision
		ccrDataIn.PolicyName = excludedPolicyName
		ccrDataIn.PolicyGroup = excludedPolicyGroup
		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 2)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress.PreviousRollouts, 4)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)

		rolloutProgress2 := res.NodeSegmentRolloutProgress[1]
		assert.Equal(t, rolloutProgress2.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress2.PolicyNodeGroup, policyGroup2)
		assert.Equal(t, rolloutProgress2.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress2.TotalNodes, int32(0))
		assert.Equal(t, rolloutProgress2.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress2.PreviousRollouts, 4)
		assert.Equal(t, rolloutProgress2.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress2.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress2.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress2.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
	})

	/*
	  Various Cases of Rollouts with related CCRs
	*/

	t.Run("with one rollout with one CCR and unrelated CCRs in the same node segment", func(t *testing.T) {
		// response has the one rollout with the one ccr
		cleanup(t)

		req := rolloutWithAllRequiredFields
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)

		ccrDataIn := newCCR()

		ccrDataIn2 := newCCR()
		ccrDataIn2.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision

		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn, ccrDataIn2})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(1))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress.PreviousRollouts, 0)
	})

	t.Run("with one node segment, 5 rollouts, one node has a CCR for each of them", func(t *testing.T) {
		// the stats should be zero for the old rollouts
		cleanup(t)

		revIdList := []string{rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		ccrsToIngest := []*iBackend.ChefClientRun{}

		for _, id := range revIdList {
			req := rolloutWithAllRequiredFields
			req.PolicyRevisionId = id
			_, err := cfgmgmt.CreateRollout(ctx, &req)
			require.NoError(t, err)

			ccrDataIn := newCCR()
			ccrDataIn.NodePayload.Automatic["policy_revision"] = id
			ccrsToIngest = append(ccrsToIngest, ccrDataIn)
		}

		ingestCCRs(t, ccrsToIngest)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(1))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.NodeCount, int32(1))
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].LatestRunNodeCount, int32(0))
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].LatestRunNodeCount, int32(0))
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].LatestRunNodeCount, int32(0))
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].LatestRunNodeCount, int32(0))
	})

	t.Run("with one node segment, 6 rollouts, one node in each", func(t *testing.T) {
		// i.e., node 1 has a CCR for each rollout, node 2 does runs 1-4,
		// node 3 does runs 1-3, etc.
		cleanup(t)

		revIdList := []string{rollout6revID, rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		ccrsToIngest := []*iBackend.ChefClientRun{}

		for _, id := range revIdList {
			req := rolloutWithAllRequiredFields
			req.PolicyRevisionId = id
			_, err := cfgmgmt.CreateRollout(ctx, &req)
			require.NoError(t, err)

			ccrDataIn := newCCR()
			ccrDataIn.NodePayload.Automatic["policy_revision"] = id
			ccrDataIn.EntityUUID = newUUID()
			ccrsToIngest = append(ccrsToIngest, ccrDataIn)
		}

		ingestCCRs(t, ccrsToIngest)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(6))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.NodeCount, int32(1))
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].LatestRunNodeCount, int32(1))
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].LatestRunNodeCount, int32(1))
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].LatestRunNodeCount, int32(1))
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].LatestRunNodeCount, int32(1))
	})

	t.Run("with one node segment, 5 rollouts, some failed CCRs in each", func(t *testing.T) {
		// the failed vs. success stats for latest rollout are correct,
		// totals for the older ones are correct
		cleanup(t)

		revIdList := []string{rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		ccrsToIngest := []*iBackend.ChefClientRun{}

		for _, id := range revIdList {
			req := rolloutWithAllRequiredFields
			req.PolicyRevisionId = id
			_, err := cfgmgmt.CreateRollout(ctx, &req)
			require.NoError(t, err)

			for _, status := range []string{"success", "success", "success", "failure", "failure"} {
				ccrDataIn := newCCR()
				ccrDataIn.NodePayload.Automatic["policy_revision"] = id
				ccrDataIn.EntityUUID = newUUID()
				ccrDataIn.Status = status
				ccrsToIngest = append(ccrsToIngest, ccrDataIn)
			}
		}

		ingestCCRs(t, ccrsToIngest)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.TotalNodes, int32(25))
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.NodeCount, int32(5))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunSuccessfulCount, int32(3))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunErroredCount, int32(2))
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].LatestRunNodeCount, int32(5))
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].LatestRunNodeCount, int32(5))
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].LatestRunNodeCount, int32(5))
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].LatestRunNodeCount, int32(5))
	})

	t.Run("with multiple node segments each with multiple rollouts", func(t *testing.T) {
		// correct stats in each
		cleanup(t)

		revIdList := []string{rollout6revID, rollout5revID, rollout4revID, rollout3revID, rollout2revID, rollout1revID}
		policyGroups := []string{policyGroup1, policyGroup2}
		ccrsToIngest := []*iBackend.ChefClientRun{}

		for _, pgroup := range policyGroups {
			for n, id := range revIdList {
				req := rolloutWithAllRequiredFields
				req.PolicyRevisionId = id
				req.PolicyNodeGroup = pgroup
				_, err := cfgmgmt.CreateRollout(ctx, &req)
				require.NoError(t, err)

				for i := 0; i < n; i++ {
					ccrDataIn := newCCR()
					ccrDataIn.PolicyGroup = pgroup
					ccrDataIn.NodePayload.Automatic["policy_revision"] = id
					ccrDataIn.EntityUUID = newUUID()
					ccrsToIngest = append(ccrsToIngest, ccrDataIn)
				}
			}
		}
		ingestCCRs(t, ccrsToIngest)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 2)

		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.TotalNodes, int32(15))
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.NodeCount, int32(5))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunSuccessfulCount, int32(5))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunErroredCount, int32(0))
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[0].LatestRunNodeCount, int32(4))
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[1].LatestRunNodeCount, int32(3))
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[2].LatestRunNodeCount, int32(2))
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
		assert.Equal(t, rolloutProgress.PreviousRollouts[3].LatestRunNodeCount, int32(1))

		rolloutProgress1 := res.NodeSegmentRolloutProgress[1]
		assert.Equal(t, rolloutProgress1.TotalNodes, int32(15))
		assert.Equal(t, rolloutProgress1.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress1.PolicyNodeGroup, policyGroup2)
		assert.Equal(t, rolloutProgress1.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress1.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		assert.Equal(t, rolloutProgress1.CurrentRolloutProgress.NodeCount, int32(5))
		assert.Equal(t, rolloutProgress1.CurrentRolloutProgress.LatestRunSuccessfulCount, int32(5))
		assert.Equal(t, rolloutProgress1.CurrentRolloutProgress.LatestRunErroredCount, int32(0))
		assert.Equal(t, rolloutProgress1.PreviousRollouts[0].Rollout.PolicyRevisionId, rollout2revID)
		assert.Equal(t, rolloutProgress1.PreviousRollouts[0].LatestRunNodeCount, int32(4))
		assert.Equal(t, rolloutProgress1.PreviousRollouts[1].Rollout.PolicyRevisionId, rollout3revID)
		assert.Equal(t, rolloutProgress1.PreviousRollouts[1].LatestRunNodeCount, int32(3))
		assert.Equal(t, rolloutProgress1.PreviousRollouts[2].Rollout.PolicyRevisionId, rollout4revID)
		assert.Equal(t, rolloutProgress1.PreviousRollouts[2].LatestRunNodeCount, int32(2))
		assert.Equal(t, rolloutProgress1.PreviousRollouts[3].Rollout.PolicyRevisionId, rollout5revID)
		assert.Equal(t, rolloutProgress1.PreviousRollouts[3].LatestRunNodeCount, int32(1))
	})

	t.Run("1 rollout with nodes marked missing", func(t *testing.T) {
		// failed vs. success stats are correct, missing is ignored
		cleanup(t)

		req := rolloutWithAllRequiredFields
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)

		ccrDataIn := newCCR()
		ccrDataIn.NodePayload.Automatic["policy_revision"] = req.PolicyRevisionId
		ccrDataIn.StartTime = time.Now().AddDate(0, 0, -25)
		ccrDataIn.EndTime = time.Now().AddDate(0, 0, -25).Add(time.Minute * 5)

		// Need to manually handle ingestion to make the node missing
		node, err := ccrDataIn.ToNode()
		require.NoError(t, err)
		node.Status = "missing"

		run, err := ccrDataIn.ToNodeRun()
		require.NoError(t, err)

		suite.IngestRuns([]iBackend.Run{run})
		suite.IngestNodes([]iBackend.Node{node})

		// Verify the node is missing
		nodesReq := &internalReq.Nodes{Filter: []string{"status:missing"}}
		nodesRes, err := cfgmgmt.GetNodes(ctx, nodesReq)
		require.NoError(t, err)
		assert.Len(t, nodesRes.Values, 1)

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(1))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		// This is the important part, the missing node should still be counted in success/errored
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunSuccessfulCount, int32(1))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.LatestRunErroredCount, int32(0))
		require.Len(t, rolloutProgress.PreviousRollouts, 0)
	})

	t.Run("with variations in the Chef Server URL", func(t *testing.T) {
		cleanup(t)

		req := rolloutWithAllRequiredFields
		// domain is HTTP, no `S`, has a trailing slash and user:password and ?queryParams
		// We expect all that stuff to get removed
		req.PolicyDomainUrl = fmt.Sprintf("http://user:password@%s/organizations/%s/?why=noIdea", chefServer1, chefOrg)
		_, err := cfgmgmt.CreateRollout(ctx, &req)
		require.NoError(t, err)

		ccrDataIn := newCCR()

		ccrDataIn2 := newCCR()
		ccrDataIn2.NodePayload.Automatic["policy_revision"] = excludedPolicyRevision

		ingestCCRs(t, []*iBackend.ChefClientRun{ccrDataIn, ccrDataIn2})

		res, err := cfgmgmt.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
		require.NoError(t, err)

		require.Len(t, res.NodeSegmentRolloutProgress, 1)
		rolloutProgress := res.NodeSegmentRolloutProgress[0]
		assert.Equal(t, rolloutProgress.PolicyName, policyName1)
		assert.Equal(t, rolloutProgress.PolicyNodeGroup, policyGroup1)
		assert.Equal(t, rolloutProgress.PolicyDomainUrl, fmt.Sprintf("https://%s/organizations/%s", chefServer1, chefOrg))
		assert.Equal(t, rolloutProgress.TotalNodes, int32(1))
		assert.Equal(t, rolloutProgress.CurrentRolloutProgress.Rollout.PolicyRevisionId, rollout1revID)
		require.Len(t, rolloutProgress.PreviousRollouts, 0)
	})

}

func newCCR() *iBackend.ChefClientRun {
	runID := newUUID()
	return &iBackend.ChefClientRun{
		ChefServerFqdn:       chefServer1,
		EntityUUID:           nodeID,
		ID:                   runID,
		MessageVersion:       "1",
		MessageType:          "run_converge",
		NodeName:             nodeID,
		OrganizationName:     chefOrg,
		Resources:            nil,
		RunID:                runID,
		RunList:              nil,
		StartTime:            now,
		EndTime:              nowPlus1Min,
		Source:               "chef_delivery",
		Status:               "success",
		TotalResourceCount:   10,
		UpdatedResourceCount: 10,
		Error:                iBackend.ChefError{},
		PolicyName:           policyName1,
		PolicyGroup:          policyGroup1,
		Deprecations:         nil,
		Tags:                 nil,
		NodePayload: iBackend.NodePayload{
			Normal:    make(map[string]interface{}),
			Default:   make(map[string]interface{}),
			Override:  make(map[string]interface{}),
			Automatic: make(map[string]interface{}),
		},
	}
}

func ingestCCRs(t *testing.T, ccrs []*iBackend.ChefClientRun) {
	nodes := make([]iBackend.Node, len(ccrs))
	runs := make([]iBackend.Run, len(ccrs))

	for i, ccr := range ccrs {
		node, err := ccr.ToNode()
		require.NoError(t, err)
		run, err := ccr.ToNodeRun()
		require.NoError(t, err)
		nodes[i] = node
		runs[i] = run
	}

	suite.IngestNodes(nodes)
	suite.IngestRuns(runs)
}
