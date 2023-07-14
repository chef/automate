package externalpostgresqlchecktrigger

import (
	"net"
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

func (epc *ExternalPostgresCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	// Check for nil or empty req body
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.EXTERNAL_POSTGRESQL, false, false, false)
	}
	if config.ExternalPG == nil {
		return externalPGNillResp(config, constants.EXTERNAL_POSTGRESQL, "Using Chef Managed PostgreSQL")
	}
	if isEmptyExternalPG(config.ExternalPG) {
		return externalPGEmptyResp(config, constants.EXTERNAL_POSTGRESQL)
	}

	return runCheckForPostgresql(config, epc.port, epc.log)
}

func (ss *ExternalPostgresCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

func runCheckForPostgresql(config *models.Config, port string, log logger.Logger) []models.CheckTriggerResponse {
	log.Debug("Trigger Postgresql check for automate and chef server nodes")
	req := getPostgresRequest(config.ExternalPG)
	var result []models.CheckTriggerResponse
	outputCh := make(chan models.CheckTriggerResponse)
	count := 0
	for _, ip := range config.Hardware.AutomateNodeIps {
		log.Debugf("Trigger Postgresql check for automate ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, constants.EXTERNAL_POSTGRESQL_API_PATH)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.AUTOMATE, http.MethodPost, outputCh, req)
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		log.Debugf("Trigger Postgresql check for chefserver ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, constants.EXTERNAL_POSTGRESQL_API_PATH)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.CHEF_INFRA_SERVER, http.MethodPost, outputCh, req)

	}

	for i := 0; i < count; i++ {
		res := <-outputCh
		result = append(result, res)
	}

	close(outputCh)
	return result

}

func getPostgresRequest(details *models.ExternalPG) models.ExternalPgRequest {
	host, port, _ := net.SplitHostPort(details.PGInstanceURL)
	return models.ExternalPgRequest{
		PostgresqlInstanceUrl:       host,
		PostgresqlInstancePort:      port,
		PostgresqlSuperUserUserName: details.PGSuperuserName,
		PostgresqlSuperUserPassword: details.PGSuperuserPassword,
		PostgresqlDbUserUserName:    details.PGDbUserName,
		PostgresqlDbUserPassword:    details.PGDbUserPassword,
		PostgresqlRootCert:          details.PGRootCert,
	}
}

func externalPGNillResp(config *models.Config, checkType, message string) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, trigger.SkippedTriggerCheckResp(ip, checkType, constants.AUTOMATE, message))
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, trigger.SkippedTriggerCheckResp(ip, checkType, constants.CHEF_INFRA_SERVER, message))
	}

	return triggerResps
}

func isEmptyExternalPG(externalPG *models.ExternalPG) bool {
	return externalPG.PGInstanceURL == "" ||
		externalPG.PGSuperuserName == "" ||
		externalPG.PGSuperuserPassword == "" ||
		externalPG.PGDbUserName == "" ||
		externalPG.PGDbUserPassword == "" ||
		externalPG.PGRootCert == ""
}

func externalPGEmptyResp(config *models.Config, checkType string) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse
	count := 0

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, trigger.ErrTriggerCheckResp(ip, checkType, constants.AUTOMATE, constants.PG_DETAILS_MISSING))
		count++
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, trigger.ErrTriggerCheckResp(ip, checkType, constants.CHEF_INFRA_SERVER, constants.PG_DETAILS_MISSING))
		count++
	}

	return triggerResps
}
