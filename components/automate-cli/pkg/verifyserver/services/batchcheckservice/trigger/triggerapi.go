package trigger

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func RunCheck(config models.Config, log logger.Logger, port string, path string, depState string, method string, reqBody interface{}) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	count := config.Hardware.AutomateNodeCount +
		config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount +
		config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse)

	if path == constants.HARDWARE_RESOURCE_CHECK_API_PATH {
		endpoint := prepareEndpoint(path, "127.0.0.1", port, "bastion", depState)
		go triggerCheckAPI(endpoint, "127.0.0.1", "bastion", method, outputCh, reqBody)

		res := <-outputCh
		result = append(result, res)
		return result
	}
	// added one for bastion node
	if path == constants.SOFTWARE_VERSION_CHECK_API_PATH || path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
		count = count + 1
		endpoint := prepareEndpoint(path, "127.0.0.1", port, "bastion", depState)
		go triggerCheckAPI(endpoint, "127.0.0.1", "bastion", method, outputCh, reqBody)
	}

	for _, ip := range config.Hardware.AutomateNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "automate", depState)
		go triggerCheckAPI(endpoint, ip, "automate", method, outputCh, reqBody)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "chef-infra-server", depState)
		go triggerCheckAPI(endpoint, ip, "chef-infra-server", method, outputCh, reqBody)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "opensearch", depState)
		go triggerCheckAPI(endpoint, ip, "opensearch", method, outputCh, reqBody)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		endpoint := prepareEndpoint(path, ip, port, "postgresql", depState)
		go triggerCheckAPI(endpoint, ip, "postgresql", method, outputCh, reqBody)
	}

	for i := 0; i < count; i++ {
		select {
		case res := <-outputCh:
			result = append(result, res)
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

	} else {
		endPoint = fmt.Sprintf("http://%s:%s%s", ip, port, path)
	}
	return endPoint
}

// triggerCheckAPI prepares interface request body to io.Reader and triggers the API where where response and error is passed into
// the output channel and func exits
func triggerCheckAPI(endPoint, host, nodeType, method string, output chan<- models.CheckTriggerResponse, reqBody interface{}) {
	var ctr models.CheckTriggerResponse

	reader, err := interfaceToIOReader(reqBody)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Error: &fiber.Error{
				Code:    http.StatusBadRequest,
				Message: fmt.Sprintf("error while reading the request body: %s", err.Error()),
			},
			Host:     host,
			NodeType: nodeType,
		}
		return
	}

	req, err := http.NewRequest(method, endPoint, reader)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while creating the request:%s", err.Error()),
			},
			NodeType: nodeType,
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
			NodeType: nodeType,
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
			NodeType: nodeType,
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
			NodeType: nodeType,
		}
		return
	}

	ctr.Host = host
	ctr.NodeType = nodeType
	output <- ctr
}

func interfaceToIOReader(body interface{}) (io.Reader, error) {
	var reader io.Reader
	if body != nil {
		bx, err := json.Marshal(body)
		if err != nil {
			return nil, err
		}

		reader = bytes.NewBuffer(bx)

	}
	return reader, nil
}
