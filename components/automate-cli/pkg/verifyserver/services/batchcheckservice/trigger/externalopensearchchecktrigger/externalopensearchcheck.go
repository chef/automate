package externalopensearchchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
)

type ExternalOpensearchCheck struct {
	log  logger.Logger
	port string
}

func NewExternalOpensearchCheck(log logger.Logger, port string) *ExternalOpensearchCheck {
	return &ExternalOpensearchCheck{
		log:  log,
		port: port,
	}
}

func (eoc *ExternalOpensearchCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	// Check for nil or empty req body
	if config.Hardware == nil {
		return trigger.NilRespForA2CS(constants.EXTERNAL_OPENSEARCH)
	}
	if config.ExternalOS == nil {
		return externalOSNillResp(config, constants.EXTERNAL_OPENSEARCH)
	}

	if isEmptyExternalOS(config.ExternalOS) {
		return externalOSEmptyResp(config, constants.EXTERNAL_OPENSEARCH)
	}

	return runCheckForOpensearch(config, eoc.port, eoc.log)
}

func (ss *ExternalOpensearchCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

func runCheckForOpensearch(config *models.Config, port string, log logger.Logger) []models.CheckTriggerResponse {
	log.Debug("Trigger Opensearch check for automate and chef server nodes")
	req := getOpensearchRequest(config.ExternalOS)
	var resultChan []models.CheckTriggerResponse
	outCh := make(chan models.CheckTriggerResponse)
	count := 0
	for _, ip := range config.Hardware.AutomateNodeIps {
		log.Debugf("Trigger Opensearch check for automate ip %s", ip)
		endPoint := checkutils.PrepareEndPoint(ip, port, constants.EXTERNAL_OPENSEARCH_API_PATH)
		count++
		go trigger.TriggerCheckAPI(endPoint, ip, constants.AUTOMATE, http.MethodPost, outCh, req)
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		endPoint := checkutils.PrepareEndPoint(ip, port, constants.EXTERNAL_OPENSEARCH_API_PATH)
		log.Debugf("Trigger Opensearch check for chef-infra-server ip %s", ip)
		count++
		go trigger.TriggerCheckAPI(endPoint, ip, constants.CHEF_INFRA_SERVER, http.MethodPost, outCh, req)

	}

	for i := 0; i < count; i++ {
		result := <-outCh
		resultChan = append(resultChan, result)
	}

	close(outCh)
	return resultChan

}

func getOpensearchRequest(details *models.ExternalOS) models.ExternalOS {
	return models.ExternalOS{
		OSDomainName:   details.OSDomainName,
		OSDomainURL:    details.OSDomainURL,
		OSUsername:     details.OSUsername,
		OSUserPassword: details.OSUserPassword,
		OSCert:         details.OSCert,
	}

}

func externalOSNillResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, GetSkippedTriggerCheckResp(ip, checktype, constants.AUTOMATE))
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, GetSkippedTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER))
	}

	return triggerResps
}

func GetSkippedTriggerCheckResp(ip, checkType, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		NodeType:  nodeType,
		CheckType: checkType,
		Result: models.ApiResult{
			Passed:  false,
			Skipped: true,
		},
		Host: ip,
	}
}

func isEmptyExternalOS(externalOS *models.ExternalOS) bool {
	return externalOS.OSDomainName == "" ||
		externalOS.OSDomainURL == "" ||
		externalOS.OSUsername == "" ||
		externalOS.OSUserPassword == "" ||
		externalOS.OSCert == "" ||
		externalOS.OSRoleArn == ""
}

func externalOSEmptyResp(config *models.Config, checkType string) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse
	count := 0

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, checkType, constants.AUTOMATE))
		count++
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, checkType, constants.CHEF_INFRA_SERVER))
		count++
	}

	return triggerResps
}

func createErrorResponse(ip, checkType, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		Host:      ip,
		NodeType:  nodeType,
		CheckType: checkType,
		Result: models.ApiResult{
			Passed: false,
			Error: &fiber.Error{
				Code:    http.StatusBadRequest,
				Message: "OS configuration is missing",
			},
		},
	}
}
