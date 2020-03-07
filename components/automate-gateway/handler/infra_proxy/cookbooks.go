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

// GetCookbook fetches an array of existing cookbooks
func (a *InfraProxyServer) GetCookbook(ctx context.Context, r *gwreq.Cookbook) (*gwres.Cookbook, error) {
	req := &infra_req.Cookbook{
		OrgId:   r.OrgId,
		Name:    r.Name,
		Version: r.Version,
	}
	res, err := a.client.GetCookbook(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Cookbook{
		CookbookName: res.GetCookbookName(),
		Name:         res.GetName(),
		Version:      res.GetVersion(),
		ChefType:     res.GetChefType(),
		Frozen:       res.GetFrozen(),
		JsonClass:    res.GetJsonClass(),
		Files:        parseCookbookItems(res.GetFiles()),
		Templates:    parseCookbookItems(res.GetTemplates()),
		Attributes:   parseCookbookItems(res.GetAttributes()),
		Recipes:      parseCookbookItems(res.GetRecipes()),
		Definitions:  parseCookbookItems(res.GetDefinitions()),
		Libraries:    parseCookbookItems(res.GetLibraries()),
		Providers:    parseCookbookItems(res.GetProviders()),
		Resources:    parseCookbookItems(res.GetResources()),
		RootFiles:    parseCookbookItems(res.GetRootFiles()),
		Metadata:     parseCookbookMetadata(res.GetMetadata()),
		Access:       parseCookbookAccess(res.GetAccess()),
	}, nil
}

// GetCookbookAffectedNodes get the nodes using cookbook
func (a *InfraProxyServer) GetCookbookAffectedNodes(ctx context.Context, r *gwreq.Cookbook) (*gwres.CookbookAffectedNodes, error) {

	req := &infra_req.Cookbook{
		OrgId:   r.OrgId,
		Name:    r.Name,
		Version: r.Version,
	}
	res, err := a.client.GetCookbookAffectedNodes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CookbookAffectedNodes{
		Nodes: parseNodeAttributeFromRes(res.Nodes),
	}, nil
}

func fromUpstreamCookbookVersion(t *infra_res.CookbookVersion) *gwres.CookbookVersion {
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
		ts[i] = fromUpstreamCookbookVersion(cookbook)
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

func parseCookbookItems(items []*infra_res.CookbookItem) []*gwres.CookbookItem {
	cl := make([]*gwres.CookbookItem, len(items))
	for i, c := range items {
		cl[i] = &gwres.CookbookItem{
			Url:         c.Url,
			Path:        c.Path,
			Specificity: c.Specificity,
			Name:        c.Name,
			Checksum:    c.Checksum,
		}
	}
	return cl
}

func parseCookbookMetadata(md *infra_res.CookbookMeta) *gwres.CookbookMeta {
	return &gwres.CookbookMeta{
		Name:            md.Name,
		Version:         md.Version,
		Description:     md.Description,
		LongDescription: md.LongDescription,
		Maintainer:      md.Maintainer,
		MaintainerEmail: md.MaintainerEmail,
		License:         md.License,
	}
}

func parseCookbookAccess(cc *infra_res.CookbookAccess) *gwres.CookbookAccess {
	return &gwres.CookbookAccess{
		Read:   cc.Read,
		Create: cc.Create,
		Grant:  cc.Grant,
		Update: cc.Grant,
		Delete: cc.Delete,
	}
}

func parseNodeAttributeFromRes(nodes []*infra_res.NodeAttribute) []*gwres.NodeAttribute {
	nl := make([]*gwres.NodeAttribute, len(nodes))

	for i, node := range nodes {
		nl[i] = &gwres.NodeAttribute{
			Name:        node.Name,
			CheckIn:     node.CheckIn,
			ChefGuid:    node.ChefGuid,
			Environment: node.Environment,
			Platform:    node.Platform,
			PolicyGroup: node.PolicyGroup,
			Uptime:      node.Uptime,
		}
	}

	return nl
}
