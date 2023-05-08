package hardwareresourcechecktrigger

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

type HardwareResourceCountCheck struct {
	host string
	port string
	log  logger.Logger
}

func NewHardwareResourceCountCheck(log logger.Logger, port string) *HardwareResourceCountCheck {
	return &HardwareResourceCountCheck{
		log:  log,
		host: "http://localhost",
		port: port,
	}
}

func (ss *HardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	ss.log.Info("Performing Hardware Resource count check from batch check ")

	//This map will hold the Response against each IP
	finalResult := make(map[string]models.CheckTriggerResponse)

	var ipArray []string

	ipArray = append(ipArray, config.Hardware.AutomateNodeIps...)
	ipArray = append(ipArray, config.Hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, config.Hardware.OpenSearchNodeIps...)
	ipArray = append(ipArray, config.Hardware.PostgresqlNodeIps...)

	resp, err := ss.TriggerHardwareResourceCountCheck(config.Hardware)

	//In case of error, construct the map of IP and empty response with error
	if err != nil {
		for _, ip := range ipArray {
			checkResponse := models.CheckTriggerResponse{
				Error: fiber.NewError(fiber.StatusServiceUnavailable, err.Error()),
				Result: models.ApiResult{
					Passed:  false,
					Check:   constants.HARDWARE_RESOURCE_COUNT,
					Message: constants.HARDWARE_RESOURCE_COUNT_MSG,
				},
			}
			finalResult[ip] = checkResponse
		}
		return finalResult
	}
	// send the success response
	for _, result := range resp.Result {
		outputResult := models.ApiResult{
			Check:   constants.HARDWARE_RESOURCE_COUNT,
			Message: constants.HARDWARE_RESOURCE_COUNT_MSG,
			Checks:  result.Checks,
		}
		isPassed := true
		for _, check := range result.Checks {
			if !check.Passed {
				isPassed = false
			}
		}
		outputResult.Passed = isPassed
		checkResponse := models.CheckTriggerResponse{
			Status: resp.Status,
			Result: outputResult,
		}
		finalResult[result.IP] = checkResponse
	}
	return finalResult
}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *HardwareResourceCountCheck) TriggerHardwareResourceCountCheck(body interface{}) (
	*models.HardwareResourceCheckResponse, error) {
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.HARDWARE_RESOURCE_CHECK_API_PATH)
	resp, err := Post(url, body)
	if err != nil {
		ss.log.Error("Error while Performing Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	respBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		ss.log.Error("Error while reading response of Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	response := models.HardwareResourceCheckResponse{}
	err = json.Unmarshal(respBody, &response)
	if err != nil {
		ss.log.Error("Error while reading unmarshalling response of Hardware resource count check from batch Check API : ", err)
		return nil, err
	}
	return &response, nil
}

// Post - This function performs HTTP Post request to the given endpoint with the provided request body
func Post(url string, body interface{}) (*http.Response, error) {
	requestBody, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequest(http.MethodPost, url, bytes.NewBuffer(requestBody))
	if err != nil {
		return nil, err
	}
	req.Header.Set(constants.CONTENT_TYPE, constants.TYPE_JSON)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	if err == nil && resp.StatusCode != 200 {
		return nil, errors.New(resp.Status)
	}
	return resp, nil
}
