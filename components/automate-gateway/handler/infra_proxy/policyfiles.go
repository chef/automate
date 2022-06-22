package infra_proxy

import (
	"context"
	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
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
		SolutionDependecies: FromUpstreamIncludeSolutionDependecies(res.SolutionDependecies),
	}, nil
}

func FromUpstreamIncludeSolutionDependecies(sp []*infra_res.SolutionDependencies) []*gwres.SolutionDependencies {
	sol_d_data := make([]*gwres.SolutionDependencies, 0)
	for _, cb := range sp {
		d_data := make([]*gwres.DepedenciesData, 0)
		for _, cbb := range cb.Dependencies {
			data1 := &gwres.DepedenciesData{
				Name:    cbb.Name,
				Version: cbb.Version,
			}
			d_data = append(d_data, data1)
		}
		data2 := &gwres.SolutionDependencies{
			Name:         cb.GetName(),
			Version:      cb.GetVersion(),
			Dependencies: d_data,
		}
		sol_d_data = append(sol_d_data, data2)
	}

	return sol_d_data
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
