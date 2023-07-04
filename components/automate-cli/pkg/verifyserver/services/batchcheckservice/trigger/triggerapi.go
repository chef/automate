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

type ErrorResponse struct {
	Status  string          `json:"string"`
	Results []models.Checks `json:"checks"`
	Error   fiber.Error     `json:"error"`
}

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
		Timeout: 35 * time.Second,
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
		responseBody, err := io.ReadAll(resp.Body)
		if err != nil {
			output <- models.CheckTriggerResponse{
				Host:     host,
				NodeType: nodeType,
				Result: models.ApiResult{
					Passed: false,
					Error: &fiber.Error{
						Code:    http.StatusInternalServerError,
						Message: fmt.Sprintf("error while reading checks response Body: %s", err.Error()),
					},
				},
			}
			return
		}
		errResponse := ErrorResponse{}
		err = json.Unmarshal(responseBody, &errResponse)
		if err != nil {
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
		output <- models.CheckTriggerResponse{
			Host:     host,
			NodeType: nodeType,
			Result: models.ApiResult{
				Passed: false,
				Error: &fiber.Error{
					Code:    errResponse.Error.Code,
					Message: errResponse.Error.Message,
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

func HardwareNil(checkType string, includeOPENSEARCH bool, includePOSTGRESQL bool, includeBASTION bool) []models.CheckTriggerResponse {
	responses := []models.CheckTriggerResponse{
		{
			NodeType:  constants.AUTOMATE,
			CheckType: checkType,
			Result: models.ApiResult{
				Passed:  false,
				Skipped: true,
				Check:   checkType,
			},
			Host: constants.UNKNOWN_HOST,
		},
		{
			NodeType:  constants.CHEF_INFRA_SERVER,
			CheckType: checkType,
			Result: models.ApiResult{
				Passed:  false,
				Skipped: true,
				Check:   checkType,
			},
			Host: constants.UNKNOWN_HOST,
		},
	}

	if includeOPENSEARCH {
		responses = append(responses, models.CheckTriggerResponse{
			NodeType:  constants.OPENSEARCH,
			CheckType: checkType,
			Result: models.ApiResult{
				Passed:  false,
				Skipped: true,
				Check:   checkType,
			},
			Host: constants.UNKNOWN_HOST,
		})
	}

	if includePOSTGRESQL {
		responses = append(responses, models.CheckTriggerResponse{
			NodeType:  constants.POSTGRESQL,
			CheckType: checkType,
			Result: models.ApiResult{
				Passed:  false,
				Skipped: true,
				Check:   checkType,
			},
			Host: constants.UNKNOWN_HOST,
		})
	}

	if includeBASTION {
		responses = append(responses, models.CheckTriggerResponse{
			NodeType:  constants.BASTION,
			CheckType: checkType,
			Result: models.ApiResult{
				Passed:  false,
				Skipped: true,
				Check:   checkType,
			},
			Host: constants.LOCALHOST,
		})
	}

	return responses
}

func ErrTriggerCheckResp(ip, checkType, nodeType, msg string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		Host:      ip,
		NodeType:  nodeType,
		CheckType: checkType,
		Result: models.ApiResult{
			Passed: false,
			Error: &fiber.Error{
				Code:    http.StatusBadRequest,
				Message: msg,
			},
			Check: checkType,
		},
	}
}

func SkippedTriggerCheckResp(ip, checktype, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		NodeType:  nodeType,
		CheckType: checktype,
		Result: models.ApiResult{
			Passed:  false,
			Skipped: true,
			Check:   checktype,
		},
		Host: ip,
	}
}

func ConstructEmptyResp(config *models.Config, checktype, msg string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, ErrTriggerCheckResp(ip, checktype, constants.AUTOMATE, msg))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, ErrTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER, msg))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, ErrTriggerCheckResp(ip, checktype, constants.POSTGRESQL, msg))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, ErrTriggerCheckResp(ip, checktype, constants.OPENSEARCH, msg))
	}

	return resps
}

func ConstructNilResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, SkippedTriggerCheckResp(ip, checktype, constants.AUTOMATE))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, SkippedTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, SkippedTriggerCheckResp(ip, checktype, constants.POSTGRESQL))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, SkippedTriggerCheckResp(ip, checktype, constants.OPENSEARCH))
	}

	return resps
}
