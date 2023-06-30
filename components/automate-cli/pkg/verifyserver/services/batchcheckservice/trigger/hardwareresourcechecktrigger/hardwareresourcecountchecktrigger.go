package hardwareresourcechecktrigger

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
)

type HardwareResourceCountCheck struct {
	host string
	port string
	log  logger.Logger
}

func NewHardwareResourceCountCheck(log logger.Logger, port string) *HardwareResourceCountCheck {
	return &HardwareResourceCountCheck{
		log:  log,
		host: constants.LOCAL_HOST_URL,
		port: port,
	}
}

func (ss *HardwareResourceCountCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing Hardware Resource count check from batch check ")

	var finalResult []models.CheckTriggerResponse

	resp, err := ss.TriggerHardwareResourceCountCheck(config.Hardware)

	// In case of error, send the slice of checkTriggerResponse
	// for each and every IP to make the processing simple at the caller end

	if err != nil {
		statusCode := 500
		errMessage := err.Error()
		if resp != nil {
			statusCode = resp.Error.Code
			errMessage = resp.Error.Message
		}
		hostMap := configutils.GetNodeTypeMap(config.Hardware)
		for ip, types := range hostMap {
			for i := 0; i < len(types); i++ {
				finalResult = append(finalResult, checkutils.PrepareTriggerResponse(nil, ip, types[i], errMessage, constants.HARDWARE_RESOURCE_COUNT, constants.HARDWARE_RESOURCE_COUNT_MSG, true, statusCode))
			}
		}
		return finalResult
	}
	// send the success response
	for _, result := range resp.Result {
		finalResult = append(finalResult,
			models.CheckTriggerResponse{
				Status: resp.Status,
				Result: models.ApiResult{
					Check:   constants.HARDWARE_RESOURCE_COUNT,
					Message: constants.HARDWARE_RESOURCE_COUNT_MSG,
					Checks:  result.Checks,
					Passed:  checkutils.IsPassed(result.Checks),
				},
				NodeType: result.NodeType,
				Host:     result.IP,
			})
	}
	return finalResult
}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *HardwareResourceCountCheck) TriggerHardwareResourceCountCheck(body interface{}) (
	*models.HardwareResourceCheckResponse, error) {
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.HARDWARE_RESOURCE_CHECK_API_PATH)
	resp, err := httputils.MakeRequest(http.MethodPost, url, body)
	if err != nil {
		ss.log.Error("Error while Performing Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		ss.log.Error("Error while reading response of Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	response := models.HardwareResourceCheckResponse{}
	err = json.Unmarshal(respBody, &response)
	if err != nil {
		ss.log.Error("Error while reading unmarshalled response of Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	
	if response.Error.Message != "" {
		ss.log.Error("Error while calling the Hardware Resource Count: ", response.Error.Message)
		return &response, errors.New(response.Error.Message) 
	}
	return &response, nil
}

func (ss *HardwareResourceCountCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}
