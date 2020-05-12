package grpcserver

import (
	"context"
	"fmt"
	"strings"

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

	s.pg.CreateRollout(dbNewRolloutReq(req))

	return nil, nil
}

func (s *CfgMgmtServer) GetRollouts(ctx context.Context, in *request.Rollouts) (*response.Rollouts, error) {
	return nil, nil
}

func dbNewRolloutReq(apiReq *request.CreateRollout) *postgres.NewRollout {
	return &postgres.NewRollout{*apiReq}
}
