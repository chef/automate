package majorupgrade_utils

import "github.com/chef/automate/components/automate-deployment/pkg/client"

const (
	INDICES_TOTAL_SHARD_DEFAULT = 2000
	V3_ES_SETTING_FILE          = "/hab/svc/deployment-service/old_es_v3_settings.json"
)

func IsExternalElasticSearch(timeout int64) bool {
	res, err := client.GetAutomateConfig(timeout)
	if err != nil {
		return false
	}
	return res.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
}
