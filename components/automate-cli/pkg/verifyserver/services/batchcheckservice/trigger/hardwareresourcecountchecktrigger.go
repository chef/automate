package trigger

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

type HardwareResourceCountCheck struct {
	host string
	log  logrus.Logger
}

func NewHardwareResourceCountCheck() *HardwareResourceCountCheck {
	return &HardwareResourceCountCheck{log: *logrus.New(), host: "http://localhost"}
}

func (ss *HardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	ss.log.Info("Performing Hardware Resource count check from batch check ")

	//This map will hold the Response against each IP
	finalResult := make(map[string]models.CheckTriggerResponse)

	ipArray := []string{}

	ipArray = append(ipArray, config.Hardware.AutomateNodeIps...)
	ipArray = append(ipArray, config.Hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, config.Hardware.OpenSearchNodeIps...)
	ipArray = append(ipArray, config.Hardware.PostgresqlNodeIps...)

	resp, err := ss.TriggerHardwareResourceCountCheck(config.Hardware)

	if err != nil {
		for _, ip := range ipArray {
			checkResponse := models.CheckTriggerResponse{}
			outputResult := models.ApiResult{}
			outputResult.Check = constants.HARDWARE_RESOURCE_COUNT
			outputResult.Message = constants.HARDWARE_RESOURCE_COUNT_MSG
			outputResult.Passed = false
			checkResponse.Error = fiber.NewError(fiber.StatusServiceUnavailable, err.Error())
			checkResponse.Result = outputResult

			finalResult[ip] = checkResponse

		}

	} else {
		for _, result := range resp.Result {
			checkResponse := models.CheckTriggerResponse{}
			outputResult := models.ApiResult{}
			outputResult.Check = constants.HARDWARE_RESOURCE_COUNT
			outputResult.Message = constants.HARDWARE_RESOURCE_COUNT_MSG

			if err != nil {
				outputResult.Passed = false
				checkResponse.Error = fiber.NewError(fiber.StatusServiceUnavailable, err.Error())

			} else {
				outputResult.Checks = result.Checks
				outputResult.Passed = true
			}
			checkResponse.Result = outputResult

			finalResult[result.IP] = checkResponse
		}
	}
	return finalResult
}

//TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *HardwareResourceCountCheck) TriggerHardwareResourceCountCheck(body interface{}) (*models.HardwareResourceCheckResponse, error) {
	url := fmt.Sprintf("%s%s", ss.host, constants.HARDWARE_RESOURCE_CHECK_API_PATH)
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

//Post - This function performs HTTP Post request to the given endpoint with the provided request body
func Post(url string, body interface{}) (*http.Response, error) {
	requestBody, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequest("POST", url, bytes.NewBuffer(requestBody))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != 200 {
		return nil, errors.New(resp.Status)
	}
	return resp, nil
}
