package configutils

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

func GetIps(config models.Config) []string {
	var ipArray []string

	ipArray = append(ipArray, config.Hardware.AutomateNodeIps...)
	ipArray = append(ipArray, config.Hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, config.Hardware.PostgresqlNodeIps...)
	ipArray = append(ipArray, config.Hardware.OpenSearchNodeIps...)
	return ipArray
}

func GetNodeTypeMap(config models.Config) map[string][]string {
	hostMap := make(map[string][]string)

	for _, ip := range config.Hardware.AutomateNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.AUTOMATE)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.CHEF_INFRA_SERVER)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.POSTGRESQL)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.OPENSEARCH)
	}
	return hostMap
}

func PrepareTriggerResponse(resp *models.CheckTriggerResponse, host, nodeType, errorString, check, msg string, isError bool) models.CheckTriggerResponse {
	if isError {
		return models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed:  false,
				Check:   check,
				Message: msg,
				Error:   fiber.NewError(fiber.StatusServiceUnavailable, errorString),
			},
		}
	} else {
		return models.CheckTriggerResponse{
			Status:   resp.Status,
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed:  IsPassed(resp.Result.Checks),
				Check:   check,
				Message: msg,
				Checks:  resp.Result.Checks,
			},
		}
	}
}

func IsPassed(checks []models.Checks) bool {
	isPassed := true
	for _, check := range checks {
		if !check.Passed {
			isPassed = false
		}
	}
	return isPassed
}
