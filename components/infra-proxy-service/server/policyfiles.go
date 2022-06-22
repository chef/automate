package server

import (
	"context"
	"encoding/json"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
	"strings"
)

// GetPolicyfiles gets a list of all policy files
func (s *Server) GetPolicyfiles(ctx context.Context, req *request.Policyfiles) (*response.Policyfiles, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	policyGroups, err := c.client.PolicyGroups.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Policyfiles{
		Policies: fromAPIToListPolicyfiles(policyGroups),
	}, nil
}

// GetPolicyfile gets a policy file
func (s *Server) GetPolicyfile(ctx context.Context, req *request.Policyfile) (*response.Policyfile, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	policy, err := c.client.Policies.GetRevisionDetails(req.Name, req.RevisionId)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	defaultAttrs, err := json.Marshal(policy.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttrs, err := json.Marshal(policy.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	result, err := c.SearchRoles(&request.SearchQuery{})
	if err != nil {
		return nil, ParseAPIError(err)
	}

	runList, err := GetExpandRunlistFromRole(policy.RunList, &result)
	if err != nil {
		return nil, err
	}

	eRunList := &response.ExpandedRunList{
		Id:      "_default",
		RunList: runList,
	}

	return &response.Policyfile{
		RevisionId:          policy.RevisionID,
		Name:                policy.Name,
		RunList:             policy.RunList,
		NamedRunList:        fromAPINamedRunList(policy.NamedRunList),
		ExpandedRunList:     []*response.ExpandedRunList{eRunList},
		IncludedPolicyLocks: fromAPIIncludedPolicyLocks(policy.IncludedPolicyLocks),
		CookbookLocks:       fromAPICookbookLocks(policy.CookbookLocks),
		DefaultAttributes:   string(defaultAttrs),
		OverrideAttributes:  string(overrideAttrs),
		SolutionDependecies: FromAPIIncludedSolutionDependencies(policy.SolutionDependencies),
	}, nil

}

func FromAPIIncludedSolutionDependencies(sp chef.SolutionDep) []*response.SolutionDependencies {
	dData := make([]*response.SolutionDependencies, 0)
	for _, p := range sp.PolicyFile {
		if len(p) == 0 {
			continue
		}
		dependencies := make([]*response.DepedenciesData, 0)
		name := strings.TrimSpace(p[0])
		ver := "0.0.0"
		if len(p) == 2 {
			ver = strings.TrimSpace(p[1])
		}

		value, ok := sp.Dependencies.(map[string]interface{})
		// Check for type conversion failure
		if ok {
			for key, v := range value {
				val, ok := v.([]interface{})
				if !ok {
					continue
				}
				cookBookName := strings.Split(key, " ")[0]
				if cookBookName == name {
					for _, value := range val {
						cookDeps := value.([]interface{})
						if len(cookDeps) == 0 {
							continue
						}
						dependencyName, ok := cookDeps[0].(string)
						if !ok {
							continue
						}
						dependencyVersion := "0.0.0"
						if len(cookDeps) > 1 {
							dependencyVersion = cookDeps[1].(string)
						}
						item1 := &response.DepedenciesData{
							Name:    dependencyName,
							Version: dependencyVersion,
						}
						dependencies = append(dependencies, item1)
					}
				}
			}

		}
		solDep := &response.SolutionDependencies{
			Name:         name,
			Version:      ver,
			Dependencies: dependencies,
		}
		dData = append(dData, solDep)
	}
	return dData

}

// fromAPIToListPolicyfiles gets a response PolicyFile from a struct of policies list
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

			ScmDetail: &response.SCMDetail{
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

// DeletePolicyfile deletes the policyfile
func (s *Server) DeletePolicyfile(ctx context.Context, req *request.DeletePolicyfile) (*response.DeletePolicyfile, error) {
	err := validation.New(validation.Options{
		Target:          "policyfile",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	_, err = c.client.Policies.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DeletePolicyfile{
		Name: req.GetName(),
	}, nil
}

// GetPolicyfileRevisions gets a policy file revisions
func (s *Server) GetPolicyfileRevisions(ctx context.Context, req *request.PolicyfileRevisions) (*response.PolicyfileRevisions, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	policyfileRevision, err := c.client.Policies.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.PolicyfileRevisions{
		Revisions: fromAPIIncludedPolicyfileRevisions(policyfileRevision),
	}, nil
}

// fromAPIIncludedPolicyfileRevisions a response included policyfile revision
func fromAPIIncludedPolicyfileRevisions(p chef.PolicyGetResponse) []*response.PolicyfileRevision {
	var revisions []*response.PolicyfileRevision

	for _, rev := range p {
		for key, _ := range rev {
			revision := &response.PolicyfileRevision{
				RevisionId: key,
			}
			revisions = append(revisions, revision)
		}
	}

	return revisions
}

// GetPolicygroup gets a policy group
func (s *Server) GetPolicygroup(ctx context.Context, req *request.Policygroup) (*response.Policygroup, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	policygroup, err := c.client.PolicyGroups.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Policygroup{
		Name:     req.GetName(),
		Uri:      policygroup.Uri,
		Policies: fromAPIGroupPolicies(policygroup),
	}, nil

}

// fromAPIGroupPolicies a response included policy revision
func fromAPIGroupPolicies(pg chef.PolicyGroup) []*response.GroupPolicy {
	policies := make([]*response.GroupPolicy, 0)

	for p, rev := range pg.Policies {
		item := &response.GroupPolicy{
			Name:       p,
			RevisionId: rev["revision_id"],
		}
		policies = append(policies, item)
	}

	return policies
}
