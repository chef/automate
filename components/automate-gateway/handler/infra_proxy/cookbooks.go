package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetCookbooks fetches an array of existing cookbooks
func (a *InfraProxyServer) GetCookbooks(ctx context.Context, r *gwreq.Cookbooks) (*gwres.Cookbooks, error) {
	req := &infra_req.Cookbooks{
		OrgId: r.OrgId,
	}
	res, err := a.client.GetCookbooks(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Cookbooks{
		Cookbooks: fromUpstreamCookbooks(res.Cookbooks),
	}, nil
}

// GetCookbooksAvailableVersions fetches an array of existing cookbooks
func (a *InfraProxyServer) GetCookbooksAvailableVersions(ctx context.Context, r *gwreq.CookbooksAvailableVersions) (*gwres.CookbooksAvailableVersions, error) {
	req := &infra_req.CookbooksAvailableVersions{
		OrgId: r.OrgId,
	}
	res, err := a.client.GetCookbooksAvailableVersions(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CookbooksAvailableVersions{
		Cookbooks: fromUpstreamCookbooksAvailableVersions(res.Cookbooks),
	}, nil
}

func fromUpstreamCookbook(t *infra_res.CookbookVersion) *gwres.CookbookVersion {
	return &gwres.CookbookVersion{
		Name:    t.GetName(),
		Version: t.GetVersion(),
	}
}

func fromUpstreamCookbookVersions(t *infra_res.CookbookAllVersion) *gwres.CookbookAllVersion {
	return &gwres.CookbookAllVersion{
		Name:           t.GetName(),
		CurrentVersion: t.GetCurrentVersion(),
		Versions:       t.GetVersions(),
	}
}

func fromUpstreamCookbooks(cookbooks []*infra_res.CookbookVersion) []*gwres.CookbookVersion {
	ts := make([]*gwres.CookbookVersion, len(cookbooks))

	for i, cookbook := range cookbooks {
		ts[i] = fromUpstreamCookbook(cookbook)
	}

	return ts
}

func fromUpstreamCookbooksAvailableVersions(cookbooks []*infra_res.CookbookAllVersion) []*gwres.CookbookAllVersion {
	ts := make([]*gwres.CookbookAllVersion, len(cookbooks))

	for i, cookbook := range cookbooks {
		ts[i] = fromUpstreamCookbookVersions(cookbook)
	}

	return ts
}
