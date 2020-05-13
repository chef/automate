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
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetCookbooks(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Cookbooks{
		Cookbooks: fromUpstreamCookbooks(res.Cookbooks),
	}, nil
}

// GetCookbookVersions fetches an array of existing cookbook versions
func (a *InfraProxyServer) GetCookbookVersions(ctx context.Context, r *gwreq.CookbookVersions) (*gwres.CookbookVersions, error) {
	req := &infra_req.CookbookVersions{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetCookbookVersions(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CookbookVersions{
		Name:     res.GetName(),
		Versions: res.GetVersions(),
	}, nil
}

// GetCookbook fetches the detail of existing cookbook
func (a *InfraProxyServer) GetCookbook(ctx context.Context, r *gwreq.Cookbook) (*gwres.Cookbook, error) {
	req := &infra_req.Cookbook{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Version:  r.Version,
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

// GetCookbookFileContent get the nodes using cookbook
func (a *InfraProxyServer) GetCookbookFileContent(ctx context.Context, r *gwreq.CookbookFileContent) (*gwres.CookbookFileContent, error) {

	req := &infra_req.CookbookFileContent{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Version:  r.Version,
		Url:      r.Url,
	}
	res, err := a.client.GetCookbookFileContent(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CookbookFileContent{
		Content: res.GetContent(),
	}, nil
}

func fromUpstreamCookbooks(cookbooks []*infra_res.CookbookVersion) []*gwres.CookbookVersion {
	ts := make([]*gwres.CookbookVersion, len(cookbooks))
	for i, cookbook := range cookbooks {
		ts[i] = &gwres.CookbookVersion{
			Name:    cookbook.GetName(),
			Version: cookbook.GetVersion(),
		}
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
