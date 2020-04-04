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
		Name:       res.GetName(),
		RevisionId: res.GetRevisionId(),
		RunList:    res.GetRunList(),
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
