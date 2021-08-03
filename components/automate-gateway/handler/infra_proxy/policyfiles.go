package infra_proxy

import (
	"context"
	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
	"strconv"
)

// GetPolicyfiles fetches an array of existing policies
func (a *InfraProxyServer) GetPolicyfiles(ctx context.Context, r *gwreq.Policyfiles) (*gwres.Policyfiles, error) {
	req := &infra_req.Policyfiles{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetPolicyfiles(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Policyfiles{
		Policies: fromUpstreamPolicyfiles(res.Policies),
	}, nil
}

// GetPolicyfile fetches the policy file details
func (a *InfraProxyServer) GetPolicyfile(ctx context.Context, r *gwreq.Policyfile) (*gwres.Policyfile, error) {
	req := &infra_req.Policyfile{
		OrgId:      r.OrgId,
		ServerId:   r.ServerId,
		Name:       r.Name,
		RevisionId: r.RevisionId,
	}
	res, err := a.client.GetPolicyfile(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Policyfile{
		Name:                res.GetName(),
		RevisionId:          res.GetRevisionId(),
		RunList:             res.GetRunList(),
		NamedRunList:        fromUpstreamNamedRunList(res.GetNamedRunList()),
		IncludedPolicyLocks: fromUpstreamIncludedPolicyLocks(res.GetIncludedPolicyLocks()),
		CookbookLocks:       fromUpstreamCookbookLocks(res.GetCookbookLocks()),
		DefaultAttributes:   res.GetDefaultAttributes(),
		OverrideAttributes:  res.GetOverrideAttributes(),
		SolutionDependecies: fromUpstreamIncludeSolutionDependecies(res.SolutionDependecies),
	}, nil
}

func fromUpstreamIncludeSolutionDependecies(sp *infra_res.SolutionDependencies) *gwres.SolutionDependencies {
	var sol_d *gwres.SolutionDependencies
	var policyfile []*gwres.Dependencies
	var dependencies []*gwres.Dependencies

	for _, p := range sp.Policyfile {
		for key, _ := range sp.CookbookDependencies {
			if strconv.Itoa(key) == p.Name {
				item1 := &gwres.Dependencies{
					Name: strconv.Itoa(key),
					Version: "",
				}
				dependencies = append(dependencies, item1)
			}
		}
		item := &gwres.Dependencies{
			Name: p.Name,
			Version: p.Version,

		}
		policyfile = append(policyfile, item)
	}

	sol_d = &gwres.SolutionDependencies{
		Policyfile: policyfile,
		CookbookDependencies: dependencies,
	}

	return sol_d
}


func fromUpstreamPolicyfiles(policies []*infra_res.PolicyfileListItem) []*gwres.PolicyfileListItem {
	ts := make([]*gwres.PolicyfileListItem, len(policies))

	for i, c := range policies {
		ts[i] = &gwres.PolicyfileListItem{
			Name:        c.GetName(),
			PolicyGroup: c.GetPolicyGroup(),
			RevisionId:  c.GetRevisionId(),
		}
	}

	return ts
}

func fromUpstreamNamedRunList(nRunList []*infra_res.NamedRunList) []*gwres.NamedRunList {
	nr := make([]*gwres.NamedRunList, len(nRunList))
	for i, r := range nRunList {
		nr[i] = &gwres.NamedRunList{
			Name:    r.GetName(),
			RunList: r.GetRunList(),
		}
	}
	return nr
}

func fromUpstreamIncludedPolicyLocks(pLocks []*infra_res.IncludedPolicyLock) []*gwres.IncludedPolicyLock {
	pl := make([]*gwres.IncludedPolicyLock, len(pLocks))
	for i, p := range pLocks {
		pl[i] = &gwres.IncludedPolicyLock{
			Name:       p.GetName(),
			RevisionId: p.GetRevisionId(),
			SourceOptions: &gwres.SourceOptions{
				Path: p.SourceOptions.GetPath(),
			},
		}
	}

	return pl
}

func fromUpstreamCookbookLocks(cLocks []*infra_res.CookbookLock) []*gwres.CookbookLock {
	cl := make([]*gwres.CookbookLock, len(cLocks))
	for i, cb := range cLocks {
		cl[i] = &gwres.CookbookLock{
			Name:             cb.GetName(),
			Version:          cb.GetVersion(),
			Identifier:       cb.GetIdentifier(),
			DottedIdentifier: cb.GetDottedIdentifier(),
			Source:           cb.GetSource(),
			CacheKey:         cb.GetCacheKey(),
			SCMDetail: &gwres.SCMDetail{
				Name:                       cb.GetScmDetail().GetName(),
				Remote:                     cb.GetScmDetail().GetRemote(),
				Revision:                   cb.GetScmDetail().GetRevision(),
				WorkingTreeClean:           cb.GetScmDetail().GetWorkingTreeClean(),
				Published:                  cb.GetScmDetail().GetPublished(),
				SynchronizedRemoteBranches: cb.GetScmDetail().GetSynchronizedRemoteBranches(),
			},
			SourceOptions: &gwres.SourceOptions{
				Path: cb.SourceOptions.GetPath(),
			},
		}
	}

	return cl
}

// DeletePolicyfiles deletes the policy
func (a *InfraProxyServer) DeletePolicyfile(ctx context.Context, r *gwreq.DeletePolicyfile) (*gwres.DeletePolicyfile, error) {
	req := &infra_req.DeletePolicyfile{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeletePolicyfile(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DeletePolicyfile{
		Name: res.GetName(),
	}, nil
}

// GetPolicyfileRevisions fetches the policyfile revisions
func (a *InfraProxyServer) GetPolicyfileRevisions(ctx context.Context, r *gwreq.PolicyfileRevisions) (*gwres.PolicyfileRevisions, error) {
	req := &infra_req.PolicyfileRevisions{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}

	res, err := a.client.GetPolicyfileRevisions(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.PolicyfileRevisions{
		Revisions: fromUpstreamPolicyfileRevision(res.Revisions),
	}, nil
}

func fromUpstreamPolicyfileRevision(revisions []*infra_res.PolicyfileRevision) []*gwres.PolicyfileRevision {
	r := make([]*gwres.PolicyfileRevision, len(revisions))

	for i, c := range revisions {
		r[i] = &gwres.PolicyfileRevision{
			RevisionId: c.GetRevisionId(),
		}
	}
	return r
}

// GetPolicygroup fetches the policy group details
func (a *InfraProxyServer) GetPolicygroup(ctx context.Context, r *gwreq.Policygroup) (*gwres.Policygroup, error) {
	req := &infra_req.Policygroup{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetPolicygroup(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Policygroup{
		Name:     res.GetName(),
		Uri:      res.GetUri(),
		Policies: fromUpstreamGroupPolicyfiles(res.Policies),
	}, nil
}

func fromUpstreamGroupPolicyfiles(policies []*infra_res.GroupPolicy) []*gwres.GroupPolicy {
	gp := make([]*gwres.GroupPolicy, len(policies))

	for i, c := range policies {
		gp[i] = &gwres.GroupPolicy{
			Name:       c.GetName(),
			RevisionId: c.GetRevisionId(),
		}
	}

	return gp
}
