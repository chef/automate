package grpcserver

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/api/external/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/postgres"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func (s *CfgMgmtServer) CreateRolloutTest(ctx context.Context, req *request.CreateRolloutTest) (*response.CreateRolloutTest, error) {
	return &response.CreateRolloutTest{}, nil
}

func (s *CfgMgmtServer) CreateRollout(ctx context.Context, req *request.CreateRollout) (*response.Rollout, error) {
	if req == nil {
		return nil, status.Error(codes.InvalidArgument, "empty request")
	}

	fieldsToCheck := [][]string{
		{req.PolicyName, "policy_name"},
		{req.PolicyNodeGroup, "policy_node_group"},
		{req.PolicyRevisionId, "policy_revision_id"},
		{req.PolicyDomainUrl, "policy_domain_url"},
	}
	blankFields := []string{}
	for _, field := range fieldsToCheck {
		if field[0] == "" {
			blankFields = append(blankFields, field[1])
		}
	}
	if len(blankFields) > 0 {
		message := fmt.Sprintf("required field(s) \"%s\" were blank", strings.Join(blankFields, "\", \""))
		return nil, status.Error(codes.InvalidArgument, message)
	}

	created, err := s.pg.CreateRollout(ctx, dbNewRolloutReq(req))
	if err != nil {
		return nil, err
	}

	return rolloutToAPI(created), nil
}

func (s *CfgMgmtServer) GetRolloutById(ctx context.Context, req *request.RolloutById) (*response.Rollout, error) {
	if req == nil {
		return nil, status.Error(codes.InvalidArgument, "empty request")
	}
	requestedId, err := strconv.Atoi(req.RolloutId)
	if err != nil {
		message := fmt.Sprintf("invalid request_id: %s", err.Error())
		return nil, status.Error(codes.InvalidArgument, message)
	}
	rollout, err := s.pg.FindRolloutByID(ctx, int32(requestedId))
	if err != nil {
		return nil, err
	}

	return rolloutToAPI(rollout), nil
}

func (s *CfgMgmtServer) GetRollouts(ctx context.Context, req *request.Rollouts) (*response.Rollouts, error) {
	if len(req.GetFilter()) > 0 {
		return nil, status.Error(codes.Unimplemented, "NOT IMPLEMENTED")
	}
	rolloutsInDB, err := s.pg.GetRollouts(ctx)
	if err != nil {
		return nil, err
	}
	resRollouts := make([]*response.Rollout, len(rolloutsInDB))
	for i, r := range rolloutsInDB {
		resRollouts[i] = rolloutToAPI(r)
	}

	return &response.Rollouts{Rollouts: resRollouts}, nil
}

func (s *CfgMgmtServer) ListNodeSegmentsWithRolloutProgress(ctx context.Context, req *request.ListNodeSegmentsWithRolloutProgress) (*response.NodeSegmentsWithRolloutProgress, error) {
	if len(req.GetFilter()) > 0 {
		return nil, status.Error(codes.Unimplemented, "NOT IMPLEMENTED")
	}
	nodeSegmentsRollouts, err := s.pg.ListNodeSegmentsForRolloutProgress()
	if err != nil {
		return nil, err
	}

	nodeSegmentsCCRs, err := s.client.GetLatestRunRolloutBreakdownCounts()
	if err != nil {
		return nil, err
	}

	res := response.NodeSegmentsWithRolloutProgress{}

	for _, nsRollouts := range nodeSegmentsRollouts {
		nodeSegmentKey := backend.NodeSegment{
			PolicyName:      nsRollouts.PolicyName,
			PolicyNodeGroup: nsRollouts.PolicyNodeGroup,
			PolicyDomainURL: nsRollouts.PolicyDomainURL,
		}

		ccrData, haveSegmentData := nodeSegmentsCCRs.BySegment[nodeSegmentKey]
		// no CCR data for this node segment
		if !haveSegmentData {
			ccrData = &backend.NodeSegmentRevisionsStatus{
				NodeSegment:      nodeSegmentKey,
				ByPolicyRevision: make(map[string]*backend.PolicyRevisionNodeStatus),
			}
		}

		rolloutProgress := mergeCCRDataIntoRollouts(nsRollouts, ccrData)
		res.NodeSegmentRolloutProgress = append(res.NodeSegmentRolloutProgress, rolloutProgress)
	}

	return &res, nil
}

func mergeCCRDataIntoRollouts(ns *postgres.NodeSegmentWithRollouts, ccrs *backend.NodeSegmentRevisionsStatus) *response.NodeSegmentRolloutProgress {
	// We should not have a case were there are zero rollouts
	currentRollout := ns.Rollouts[0]
	currentRevisionID := currentRollout.PolicyRevisionId

	currentRolloutCCR, haveCurrentRolloutCCR := ccrs.ByPolicyRevision[currentRevisionID]
	if !haveCurrentRolloutCCR {
		currentRolloutCCR = &backend.PolicyRevisionNodeStatus{
			PolicyRevisionID: currentRevisionID,
		}
	}

	currentRolloutRes := &response.CurrentRolloutProgress{
		Rollout:                  rolloutToAPI(currentRollout),
		NodeCount:                int32(currentRolloutCCR.Total),
		LatestRunSuccessfulCount: int32(currentRolloutCCR.Success),
		LatestRunErroredCount:    int32(currentRolloutCCR.Errored),
	}

	var pastRollouts []*response.PastRolloutProgress

	if len(ns.Rollouts) > 1 {
		for _, rollout := range ns.Rollouts[1:] {
			rolloutRevisionID := rollout.PolicyRevisionId
			ccrsForRollout, haveCCRData := ccrs.ByPolicyRevision[rolloutRevisionID]

			if !haveCCRData {
				ccrsForRollout = &backend.PolicyRevisionNodeStatus{}
			}

			pastRollout := &response.PastRolloutProgress{
				Rollout:            rolloutToAPI(rollout),
				LatestRunNodeCount: int32(ccrsForRollout.Total),
			}

			pastRollouts = append(pastRollouts, pastRollout)
		}
	}

	return &response.NodeSegmentRolloutProgress{
		PolicyName:             ns.PolicyName,
		PolicyNodeGroup:        ns.PolicyNodeGroup,
		PolicyDomainUrl:        ns.PolicyDomainURL,
		TotalNodes:             ccrs.NodesInSegment,
		CurrentRolloutProgress: currentRolloutRes,
		PreviousRollouts:       pastRollouts,
	}
}

func dbNewRolloutReq(r *request.CreateRollout) *postgres.NewRollout {
	return &postgres.NewRollout{
		PolicyName:           r.PolicyName,
		PolicyNodeGroup:      r.PolicyNodeGroup,
		PolicyRevisionId:     r.PolicyRevisionId,
		PolicyDomainURL:      r.PolicyDomainUrl,
		PolicyDomainUsername: r.PolicyDomainUsername,
		SCMType:              r.ScmType.String(),
		SCMWebType:           r.ScmWebType.String(),
		PolicySCMURL:         r.PolicyScmUrl,
		PolicySCMWebURL:      r.PolicyScmWebUrl,
		PolicySCMCommit:      r.PolicyScmCommit,
		SCMAuthorName:        r.ScmAuthorName,
		SCMAuthorEmail:       r.ScmAuthorEmail,
		Description:          r.Description,
		CiJobId:              r.CiJobId,
		CiJobUrl:             r.CiJobUrl,
	}
}

func rolloutToAPI(r *postgres.Rollout) *response.Rollout {
	return &response.Rollout{
		Id:                   strconv.Itoa(int(r.Id)),
		StartTime:            maybeTimeToString(r.StartTime),
		EndTime:              maybeTimeToString(r.EndTime),
		PolicyName:           r.PolicyName,
		PolicyNodeGroup:      r.PolicyNodeGroup,
		PolicyRevisionId:     r.PolicyRevisionId,
		PolicyDomainUrl:      r.PolicyDomainURL,
		PolicyDomainUsername: r.PolicyDomainUsername,
		ScmType:              scmTypeToAPI(r.SCMType),
		ScmWebType:           scmWebTypeToAPI(r.SCMWebType),
		PolicyScmUrl:         r.PolicySCMURL,
		PolicyScmWebUrl:      r.PolicySCMWebURL,
		PolicyScmCommit:      r.PolicySCMCommit,
		ScmAuthorName:        r.SCMAuthorName,
		ScmAuthorEmail:       r.SCMAuthorEmail,
		Description:          r.Description,
		CiJobId:              r.CiJobId,
		CiJobUrl:             r.CiJobUrl,
	}
}

func scmTypeToAPI(scmType string) response.SCMType {
	return response.SCMType(response.SCMType_value[scmType])
}

func scmWebTypeToAPI(scmWebType string) response.SCMWebType {
	return response.SCMWebType(response.SCMWebType_value[scmWebType])
}

func maybeTimeToString(t *time.Time) string {
	if t == nil {
		return ""
	}
	return t.Format(time.RFC3339)
}
