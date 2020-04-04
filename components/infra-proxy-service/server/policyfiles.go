package server

import (
	"context"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetPolicyfiles get policy files list
func (s *Server) GetPolicyfiles(ctx context.Context, req *request.Policyfiles) (*response.Policyfiles, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	policyGroups, err := client.PolicyGroups.List()

	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Policyfiles{
		Policies: fromAPIToListPolicyfiles(policyGroups),
	}, nil
}

// GetPolicyfile get the policy file
func (s *Server) GetPolicyfile(ctx context.Context, req *request.Policyfile) (*response.Policyfile, error) {
	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	policy, err := client.Policies.GetRevisionDetails(req.Name, req.RevisionId)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Policyfile{
		RevisionId: policy.RevisionID,
		Name:       policy.Name,
		RunList:    policy.RunList,
	}, nil

}

// fromAPIToListPolicyfile a response Policyfile from a struct of policies list
func fromAPIToListPolicyfiles(list chef.PolicyGroupGetResponse) []*response.PolicyfileListItem {
	var policies []*response.PolicyfileListItem

	for pg, policyList := range list {
		for p, rev := range policyList.Policies {
			item := &response.PolicyfileListItem{
				Name:        p,
				PolicyGroup: pg,
				RevisionId:  rev["revision_id"],
			}
			policies = append(policies, item)
		}
	}

	return policies
}
