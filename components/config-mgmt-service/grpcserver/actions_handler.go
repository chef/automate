package grpcserver

import (
	"context"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
)

func (s *CfgMgmtServer) HandlePolicyUpdateAction(ctx context.Context, req *request.PolicyUpdateAction) (*response.PolicyUpdateAction, error) {
	return &response.PolicyUpdateAction{}, nil
}
