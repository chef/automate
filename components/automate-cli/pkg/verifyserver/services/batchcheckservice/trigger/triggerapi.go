package trigger

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
)

func RunCheck(config models.Config, log logger.Logger, port string, path string, depState string) map[string]models.CheckTriggerResponse {
	result := make(map[string]models.CheckTriggerResponse)
	count := config.Hardware.AutomateNodeCount +
		config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount +
		config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse, count)

	ipNodeMap := fiberutils.ConstructIpAndNodeTypeMap(config)

	for ip, nodeType := range ipNodeMap {
		endPoint := ""

		if path == constants.SOFTWARE_VERSION_CHECK_API_PATH {
			endPoint = fmt.Sprintf("http://%s:%s%s?node_type=%s", ip, port, path, nodeType)

		} else if path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
			endPoint = fmt.Sprintf("http://%s:%s%s?node_type=%s&deployment_state=%s", ip, port, path, nodeType, depState)

		} else if path == constants.SYSTEM_USER_CHECK_API_PATH {
			endPoint = fmt.Sprintf("http://%s:%s%s", ip, port, path)
		}

		go triggerCheckAPI(endPoint, ip, outputCh)
	}

	for i := 0; i < count; i++ {
		select {
		case res := <-outputCh:
			result[res.Host] = res
		}
	}
	close(outputCh)

	return result
}

func triggerCheckAPI(endPoint, host string, output chan<- models.CheckTriggerResponse) {
	var ctr models.CheckTriggerResponse

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, endPoint, nil)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: "error while creating the request",
			},
		}
		return
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: "error while connecting to the endpoint",
			},
		}
		return
	}

	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    resp.StatusCode,
				Message: "error while connecting to the endpoint",
			},
		}
		return
	}

	if err := json.NewDecoder(resp.Body).Decode(&ctr); err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: "error while parsing the response data",
			},
		}
		return
	}

	ctr.Host = host
	output <- ctr
}
