package trigger

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func RunCheck(config *models.Config, log logger.Logger, port string, path string, depState string) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	count := config.Hardware.AutomateNodeCount +
		config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount +
		config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse)

	// added one for bastion node
	if path == constants.SOFTWARE_VERSION_CHECK_API_PATH || path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
		count = count + 1
		endpoint := prepareEndpoint(path, "127.0.0.1", port, constants.BASTION, depState)
		go TriggerCheckAPI(endpoint, "127.0.0.1", constants.BASTION, http.MethodGet, outputCh, nil)
	}

	for _, ip := range config.Hardware.AutomateNodeIps {
		endpoint := prepareEndpoint(path, ip, port, constants.AUTOMATE, depState)
		go TriggerCheckAPI(endpoint, ip, constants.AUTOMATE, http.MethodGet, outputCh, nil)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		endpoint := prepareEndpoint(path, ip, port, constants.CHEF_INFRA_SERVER, depState)
		go TriggerCheckAPI(endpoint, ip, constants.CHEF_INFRA_SERVER, http.MethodGet, outputCh, nil)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		endpoint := prepareEndpoint(path, ip, port, constants.OPENSEARCH, depState)
		go TriggerCheckAPI(endpoint, ip, constants.OPENSEARCH, http.MethodGet, outputCh, nil)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		endpoint := prepareEndpoint(path, ip, port, constants.POSTGRESQL, depState)
		go TriggerCheckAPI(endpoint, ip, constants.POSTGRESQL, http.MethodGet, outputCh, nil)
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

	} else if path == constants.SYSTEM_USER_CHECK_API_PATH {
		endPoint = fmt.Sprintf("http://%s:%s%s", ip, port, path)
	}

	return endPoint
}

// TriggerCheckAPI triggers the API with provided enpoint,host,nodetype,method and reqbody.
// EndPoint should be string
// The method can be GET,POST,PUT
// Return an channel for output
// In case of error, error is stored in output channel itself
func TriggerCheckAPI(endPoint, host, nodeType, method string, output chan<- models.CheckTriggerResponse, reqBody interface{}) {
	var ctr models.CheckTriggerResponse

	reader, err := interfaceToIOReader(reqBody)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    http.StatusBadRequest,
					Message: fmt.Sprintf("error while reading the request body: %s", err.Error()),
				},
			},
		}
		return
	}

	req, err := http.NewRequest(method, endPoint, reader)
	req.Header.Set("Content-Type", "application/json")
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    http.StatusInternalServerError,
					Message: fmt.Sprintf("error while creating the request:%s", err.Error()),
				},
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
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    http.StatusInternalServerError,
					Message: fmt.Sprintf("error while connecting to the endpoint:%s", err.Error()),
				},
			},
		}
		return
	}

	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		output <- models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    resp.StatusCode,
					Message: "error while connecting to the endpoint, received invalid status code",
				},
			},
		}
		return
	}

	if err := json.NewDecoder(resp.Body).Decode(&ctr); err != nil {
		output <- models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    http.StatusInternalServerError,
					Message: fmt.Sprintf("error while parsing the response data:%s", err.Error()),
				},
			},
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

func CheckEmptyOrNilExternalConfig(config *models.Config) ([]models.CheckTriggerResponse, bool) {
	if config.ExternalOS == nil {
		return ExternalOSPGNillResp(config), true
	}

	if config.ExternalPG == nil {
		return ExternalOSPGNillResp(config), true
	}

	if IsEmptyExternalOS(config.ExternalOS) {
		return ExternalOSPGEmptyResp(config), true
	}

	if IsEmptyExternalPG(config.ExternalPG) {
		return ExternalOSPGEmptyResp(config), true
	}

	return nil, false
}

func ExternalOSPGNillResp(config *models.Config) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse
	count := 0

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, createNilResponse(ip, constants.AUTOMATE))
		count++
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, createNilResponse(ip, constants.CHEF_INFRA_SERVER))
		count++
	}

	for _, ip := range config.Hardware.OpenSearchNodeIps {
		triggerResps = append(triggerResps, createNilResponse(ip, constants.OPENSEARCH))
		count++
	}

	for _, ip := range config.Hardware.PostgresqlNodeIps {
		triggerResps = append(triggerResps, createNilResponse(ip, constants.POSTGRESQL))
		count++
	}

	if count == 0 {
		triggerResps = append(triggerResps, createNilResponse("127.0.0.1", constants.BASTION))
	}

	return triggerResps
}

func createNilResponse(host, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		NodeType: constants.AUTOMATE,
		Result: models.ApiResult{
			Skipped: true,
		},
		Host: host,
	}
}

func ExternalOSPGEmptyResp(config *models.Config) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse
	count := 0

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, constants.AUTOMATE))
		count++
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, constants.CHEF_INFRA_SERVER))
		count++
	}

	for _, ip := range config.Hardware.OpenSearchNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, constants.OPENSEARCH))
		count++
	}

	for _, ip := range config.Hardware.PostgresqlNodeIps {
		triggerResps = append(triggerResps, createErrorResponse(ip, constants.POSTGRESQL))
		count++
	}

	if count == 0 {
		triggerResps = append(triggerResps, createErrorResponse("127.0.0.1", constants.BASTION))
	}

	return triggerResps
}

func createErrorResponse(host, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		Host:     host,
		NodeType: nodeType,
		Result: models.ApiResult{
			Passed: false,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: "External OS or PG configuration is missing",
			},
		},
	}
}

func IsEmptyExternalOS(externalOS *models.ExternalOS) bool {
	return externalOS.OSDomainName == "" ||
		externalOS.OSDomainURL == "" ||
		externalOS.OSUsername == "" ||
		externalOS.OSUserPassword == "" ||
		externalOS.OSCert == "" ||
		externalOS.OSRoleArn == ""
}

func IsEmptyExternalPG(externalPG *models.ExternalPG) bool {
	return externalPG.PGInstanceURL == "" ||
		externalPG.PGSuperuserName == "" ||
		externalPG.PGSuperuserPassword == "" ||
		externalPG.PGDbUserName == "" ||
		externalPG.PGDbUserPassword == "" ||
		externalPG.PGRootCert == ""
}
