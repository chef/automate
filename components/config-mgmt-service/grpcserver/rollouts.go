package grpcserver

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/api/external/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend/postgres"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

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

func dbNewRolloutReq(r *request.CreateRollout) *postgres.NewRollout {
	return &postgres.NewRollout{
		PolicyName:       r.PolicyName,
		PolicyNodeGroup:  r.PolicyNodeGroup,
		PolicyRevisionId: r.PolicyRevisionId,
		PolicyDomainURL:  r.PolicyDomainUrl,
		SCMType:          r.ScmType.String(),
		SCMWebType:       r.ScmWebType.String(),
		PolicySCMURL:     r.PolicyScmUrl,
		PolicySCMWebURL:  r.PolicyScmWebUrl,
		PolicySCMCommit:  r.PolicyScmCommit,
		Description:      r.Description,
		CiJobId:          r.CiJobId,
		CiJobUrl:         r.CiJobUrl,
	}
}

func rolloutToAPI(r *postgres.Rollout) *response.Rollout {
	return &response.Rollout{
		Id:               strconv.Itoa(int(r.Id)),
		StartTime:        maybeTimeToString(r.StartTime),
		EndTime:          maybeTimeToString(r.EndTime),
		PolicyName:       r.PolicyName,
		PolicyNodeGroup:  r.PolicyNodeGroup,
		PolicyRevisionId: r.PolicyRevisionId,
		PolicyDomainUrl:  r.PolicyDomainURL,
		ScmType:          scmTypeToAPI(r.SCMType),
		ScmWebType:       scmWebTypeToAPI(r.SCMWebType),
		PolicyScmUrl:     r.PolicySCMURL,
		PolicyScmWebUrl:  r.PolicySCMWebURL,
		PolicyScmCommit:  r.PolicySCMCommit,
		Description:      r.Description,
		CiJobId:          r.CiJobId,
		CiJobUrl:         r.CiJobUrl,
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
