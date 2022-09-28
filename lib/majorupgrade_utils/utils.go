package majorupgrade_utils

import (
	"context"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/load_balancer"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
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

func EnsureStatus() (bool, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return false, err
	}

	res, err := connection.Status(context.Background(), &api.StatusRequest{})
	if err != nil {
		return false, errors.Wrap(
			err,
			"Request to obtain Chef Automate status information failed",
		)
	}
	return res.ServiceStatus.AllHealthy(), nil
}
