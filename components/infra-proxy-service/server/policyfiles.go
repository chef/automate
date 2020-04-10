package server

import (
	"context"
	"encoding/json"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetPolicyfiles get policy files list
func (s *Server) GetPolicyfiles(ctx context.Context, req *request.Policyfiles) (*response.Policyfiles, error) {

	c, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	policyGroups, err := c.client.PolicyGroups.List()

	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Policyfiles{
		Policies: fromAPIToListPolicyfiles(policyGroups),
	}, nil
}

// GetPolicyfile get the policy file
func (s *Server) GetPolicyfile(ctx context.Context, req *request.Policyfile) (*response.Policyfile, error) {
	c, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	policy, err := c.client.Policies.GetRevisionDetails(req.Name, req.RevisionId)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	defaultAttrs, err := json.Marshal(policy.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttrs, err := json.Marshal(policy.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Policyfile{
		RevisionId:          policy.RevisionID,
		Name:                policy.Name,
		RunList:             policy.RunList,
		NamedRunList:        fromAPINamedRunList(policy.NamedRunList),
		IncludedPolicyLocks: fromAPIIncludedPolicyLocks(policy.IncludedPolicyLocks),
		CookbookLocks:       fromAPICookbookLocks(policy.CookbookLocks),
		DefaultAttributes:   string(defaultAttrs),
		OverrideAttributes:  string(overrideAttrs),
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

// fromAPINamedRunList a response named runlist
func fromAPINamedRunList(nRunList map[string][]string) []*response.NamedRunList {
	nr := make([]*response.NamedRunList, len(nRunList))
	var index int
	for name, runList := range nRunList {
		nr[index] = &response.NamedRunList{
			Name:    name,
			RunList: runList,
		}
		index++
	}

	return nr
}

// fromAPINamedRunList a response included policy locks
func fromAPIIncludedPolicyLocks(pLocks []chef.IncludedPolicyLocks) []*response.IncludedPolicyLock {
	pl := make([]*response.IncludedPolicyLock, len(pLocks))
	for i, p := range pLocks {
		pl[i] = &response.IncludedPolicyLock{
			Name:       p.Name,
			RevisionId: p.RevisionID,
			SourceOptions: &response.SourceOptions{
				Path: p.SourceOptions["path"],
			},
		}
	}

	return pl
}

// fromAPINamedRunList a response cookbook locks
func fromAPICookbookLocks(cLocks map[string]chef.CookbookLock) []*response.CookbookLock {
	cl := make([]*response.CookbookLock, len(cLocks))
	var index int
	for name, cb := range cLocks {
		cl[index] = &response.CookbookLock{
			Name:             name,
			Version:          cb.Version,
			Identifier:       cb.Identifier,
			DottedIdentifier: cb.DottedIdentifier,
			Source:           cb.Source,

			SCMDetail: &response.SCMDetail{
				Name:                       cb.SCM.Name,
				Remote:                     cb.SCM.Remote,
				Revision:                   cb.SCM.Revision,
				WorkingTreeClean:           cb.SCM.WorkingTreeClean,
				Published:                  cb.SCM.Published,
				SynchronizedRemoteBranches: cb.SCM.SynchronizedRemoteBranches,
			},

			SourceOptions: &response.SourceOptions{
				Path: cb.SourceOptions["path"],
			},
		}
		index++
	}

	return cl
}
