package grpcserver

import (
	"context"
	"fmt"
	"strings"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/postgres"
)

func (s *CfgMgmtServer) HandlePolicyUpdateAction(ctx context.Context, req *request.PolicyUpdateAction) (*response.PolicyUpdateAction, error) {
	fieldsToCheck := [][]string{
		{req.PolicyName, "policy_name"},
		{req.PolicyGroup, "policy_group"},
		{req.PolicyRevisionId, "policy_revision_id"},
		{req.ChefServerFqdn, "chef_server_fqdn"},
		{req.ChefServerOrgname, "chef_server_orgname"},
		{req.ChefServerUsername, "chef_server_username"},
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

	newRollout := &postgres.NewRollout{
		PolicyRevisionId: req.PolicyRevisionId,
		PolicyName:       req.PolicyName,
		PolicyNodeGroup:  req.PolicyGroup,
		PolicyDomainURL:  fmt.Sprintf("https://%s/organizations/%s", req.ChefServerFqdn, req.ChefServerOrgname),
	}

	err := s.pg.CreateRolloutFromChefAction(ctx, newRollout)
	if err != nil {
		return nil, err
	}

	cookbooks := make([]backend.PolicyCookbookLock, len(req.Cookbooks))

	for index, cookbook := range req.Cookbooks {
		cookbooks[index] = backend.PolicyCookbookLock{
			PolicyID:     cookbook.PolicyId,
			CookbookName: cookbook.CookbookName,
		}
	}

	err = s.pg.AddPolicyCookbooks(ctx, cookbooks, req.PolicyRevisionId, req.PolicyName)
	if err != nil {
		return nil, err
	}

	return &response.PolicyUpdateAction{}, nil
}
