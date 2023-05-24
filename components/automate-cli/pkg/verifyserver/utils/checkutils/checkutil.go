package checkutils

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

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
	}
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

func IsPassed(checks []models.Checks) bool {
	isPassed := true
	for _, check := range checks {
		if !check.Passed {
			isPassed = false
		}
	}
	return isPassed
}
