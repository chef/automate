package majorupgrade_utils

import (
	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/load_balancer"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

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

func SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	enable := wrapperspb.Bool(status)
	cfg := deployment.NewUserOverrideConfig()
	cfg.LoadBalancer = &load_balancer.ConfigRequest{
		V1: &load_balancer.ConfigRequest_V1{
			Sys: &load_balancer.ConfigRequest_V1_System{
				Service: &load_balancer.ConfigRequest_V1_System_Service{
					MaintenanceMode: enable,
				},
			},
		},
	}

	cw := NewCustomWriter()

	if err := client.PatchAutomateConfig(timeout, cfg, cw.CliWriter); err != nil {
		return "", "", err
	}
	return cw.WriteBuffer.String(), cw.ErrorBuffer.String(), nil
}
