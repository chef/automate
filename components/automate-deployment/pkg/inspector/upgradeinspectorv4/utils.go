package upgradeinspectorv4

import (
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

type UpgradeV4Utils interface {
	IsExternalElasticSearch() bool
}

type UpgradeV4UtilsImp struct{}

func (cu *UpgradeV4UtilsImp) IsExternalElasticSearch() bool {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		return false
	}
	return res.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
}
