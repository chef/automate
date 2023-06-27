package checkutils

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber/v2"
)

func PrepareTriggerResponse(resp *models.CheckTriggerResponse, host, nodeType, errorString, check, msg string, isError bool, statusCode int) models.CheckTriggerResponse {
	if isError {
		return models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed:  false,
				Check:   check,
				Message: msg,
				Error:   fiber.NewError(statusCode, errorString),
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

func PrepareEndPoint(ip, port, path string) string {
	return fmt.Sprintf("http://%s:%s%s", ip, port, path)
}
