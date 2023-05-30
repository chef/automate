package externalpostgresqlchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
)

type ExternalPostgresCheck struct {
	log  logger.Logger
	port string
}

func NewExternalPostgresCheck(log logger.Logger, port string) *ExternalPostgresCheck {
	return &ExternalPostgresCheck{
		log:  log,
		port: port,
	}
}

func (epc *ExternalPostgresCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return runCheckForPostgresql(config, constants.EXTERNAL_POSTGRESQL_API_PATH, epc.port, epc.log)
}

func runCheckForPostgresql(config models.Config, path string, port string, log logger.Logger) []models.CheckTriggerResponse {
	log.Debug("Trigger Postgresql check for automate and chef server nodes")
	req := getPostgresRequest(config.ExternalPG)
	var result []models.CheckTriggerResponse
	outputCh := make(chan models.CheckTriggerResponse)
	count := 0
	for _, ip := range config.Hardware.AutomateNodeIps {
		log.Debugf("Trigger Postgresql check for automate ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, path)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.AUTOMATE, http.MethodPost, outputCh, req)
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		log.Debugf("Trigger Postgresql check for chefserver ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, path)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.CHEF_INFRA_SERVER, http.MethodPost, outputCh, req)

	}

	for i := 0; i < count; i++ {
		res := <-outputCh
		result = append(result, res)
	}

	close(outputCh)
	return result

}

func getPostgresRequest(details models.ExternalPG) models.ExternalPgRequest {
	return models.ExternalPgRequest{
		PostgresqlInstanceUrl:       details.PGInstanceURL,
		PostgresqlInstancePort:      details.PGPort,
		PostgresqlSuperUserUserName: details.PGSuperuserName,
		PostgresqlSuperUserPassword: details.PGSuperuserPassword,
		PostgresqlDbUserUserName:    details.PGDbUserName,
		PostgresqlDbUserPassword:    details.PGDbUserPassword,
		PostgresqlRootCert:          details.PGRootCert,
	}

}
