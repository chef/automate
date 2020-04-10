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

// GetPolicyfile fetch the policy file detail
func (a *InfraProxyServer) GetPolicyfile(ctx context.Context, r *gwreq.Policyfile) (*gwres.Policyfile, error) {
	req := &infra_req.Policyfile{
		OrgId:      r.OrgId,
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
	}, nil
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
				Name:                       cb.GetSCMDetail().GetName(),
				Remote:                     cb.GetSCMDetail().GetRemote(),
				Revision:                   cb.GetSCMDetail().GetRevision(),
				WorkingTreeClean:           cb.GetSCMDetail().GetWorkingTreeClean(),
				Published:                  cb.GetSCMDetail().GetPublished(),
				SynchronizedRemoteBranches: cb.GetSCMDetail().GetSynchronizedRemoteBranches(),
			},
			SourceOptions: &gwres.SourceOptions{
				Path: cb.SourceOptions.GetPath(),
			},
		}
	}

	return cl
}
