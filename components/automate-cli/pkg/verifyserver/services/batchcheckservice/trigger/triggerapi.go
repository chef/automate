package trigger

import (
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func RunCheck(config models.Config, log logger.Logger, port string, path string, depState string) map[string]models.CheckTriggerResponse {
	result := make(map[string]models.CheckTriggerResponse)
	count := config.Hardware.AutomateNodeCount +
		config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount +
		config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse)

	// TODO:: Run the Check for bastion as well
	// added one for bastion node
	//if path == constants.SOFTWARE_VERSION_CHECK_API_PATH || path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
	//	count = count + 1
	//	endpoint := prepareEndpoint(path, "127.0.0.1", port, "bastion", depState)
	//	go triggerCheckAPI(endpoint, "127.0.0.1", outputCh)
	//}

	for _, ip := range config.Hardware.AutomateNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "automate", depState)
		go triggerCheckAPI(endpoint, ip, outputCh)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "chef-infra-server", depState)
		go triggerCheckAPI(endpoint, ip, outputCh)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "postgresql", depState)
		go triggerCheckAPI(endpoint, ip, outputCh)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "opensearch", depState)
		go triggerCheckAPI(endpoint, ip, outputCh)
	}

	for i := 0; i < count; i++ {
		select {
		case res := <-outputCh:
			result[res.Host] = res
		}
	}

	return result
}

func prepareEndpoint(path, ip, port, nodeType, depState string) string {
	endPoint := ""
	if path == constants.SOFTWARE_VERSION_CHECK_API_PATH {
		endPoint = fmt.Sprintf("http://%s:%s%s?node_type=%s", ip, port, path, nodeType)

	} else if path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
		endPoint = fmt.Sprintf("http://%s:%s%s?node_type=%s&deployment_state=%s", ip, port, path, nodeType, depState)

	} else if path == constants.SYSTEM_USER_CHECK_API_PATH {
		endPoint = fmt.Sprintf("http://%s:%s%s", ip, port, path)
	}

	return endPoint
}

func triggerCheckAPI(endPoint, host string, output chan<- models.CheckTriggerResponse) {
	var ctr models.CheckTriggerResponse

	req, err := http.NewRequest(http.MethodGet, endPoint, nil)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while creating the request:%s", err.Error()),
			},
		}
		return
	}

	client := http.Client{
		Timeout: 5 * time.Second,
	}

	resp, err := client.Do(req)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while connecting to the endpoint:%s", err.Error()),
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
				Message: "error while connecting to the endpoint, received invalid status code",
			},
		}
		return
	}

	if err := json.NewDecoder(resp.Body).Decode(&ctr); err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while parsing the response data:%s", err.Error()),
			},
		}
		return
	}

	ctr.Host = host
	output <- ctr
}
