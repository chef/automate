package infra_proxy

import (
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

func fromUpsteamRunList(runList []*infra_res.RunList) []*gwres.RunList {
	resRunList := make([]*gwres.RunList, len(runList))
	for i, item := range runList {
		resRunListItem := gwres.RunList{
			Type:     item.GetType(),
			Name:     item.GetName(),
			Version:  item.GetVersion(),
			Skipped:  item.GetSkipped(),
			Position: item.GetPosition(),
			Error:    item.GetError(),
			Children: fromUpsteamRunList(item.GetChildren()),
		}
		resRunList[i] = &resRunListItem
	}

	return resRunList
}
