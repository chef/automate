package sshuseraccesschecktrigger

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

type SshUserAccessCheck struct {
	host string
	port string
	log  logger.Logger
}

func NewSshUserAccessCheck(log logger.Logger, port string) *SshUserAccessCheck {
	return &SshUserAccessCheck{
		log:  log,
		host: constants.LOCAL_HOST_URL,
		port: port,
	}
}

func (ss *SshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	ss.log.Info("Performing SSH user access check from batch check ")
	count := config.Hardware.AutomateNodeCount + config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount + config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse, count)

	//This map will hold the Response against each IP
	finalResult := make(map[string]models.CheckTriggerResponse)

	ips := configutils.GetIps(config)

	for _, ip := range ips {
		var requestBody map[string]interface{}
		data, _ := json.Marshal(config.SSHUser)
		json.Unmarshal(data, &requestBody)
		requestBody[ip] = ip
		go ss.TriggerCheckAndFormatOutput(ip, requestBody, outputCh)
	}

	for i := 0; i < count; i++ {
		resp := <-outputCh
		finalResult[resp.Host] = resp
	}
	close(outputCh)
	return finalResult
}

func (ss *SshUserAccessCheck) TriggerCheckAndFormatOutput(host string, body interface{}, output chan<- models.CheckTriggerResponse) {
	var checkResp models.CheckTriggerResponse
	resp, err := ss.TriggerSshUserAccessCheck(body)
	if err != nil {
		checkResp = models.CheckTriggerResponse{
			Error: fiber.NewError(fiber.StatusServiceUnavailable, err.Error()),
			Result: models.ApiResult{
				Passed:  false,
				Check:   constants.SSH_USER,
				Message: constants.SSH_USER_MSG,
			},
			Host: host,
		}
	} else {
		isPassed := true
		for _, check := range resp.Result.Checks {
			if !check.Passed {
				isPassed = false
			}
		}
		checkResp = models.CheckTriggerResponse{
			Status: resp.Status,
			Result: models.ApiResult{
				Passed:  isPassed,
				Check:   constants.SSH_USER,
				Message: constants.SSH_USER_MSG,
				Checks:  resp.Result.Checks,
			},
			Host: host,
		}
	}
	output <- checkResp

}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *SshUserAccessCheck) TriggerSshUserAccessCheck(body interface{}) (
	*models.CheckTriggerResponse, error) {
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.SSH_USER_CHECK_API_PATH)
	resp, err := httputils.MakeRequest(http.MethodPost, url, body)
	if err != nil {
		ss.log.Error("Error while Performing SSH user access check from batch Check API : ", err)
		return nil, err
	}
	respBody, err := ioutil.ReadAll(resp.Body) // nosemgrep
	if err != nil {
		ss.log.Error("Error while reading response of SSH user access check from batch Check API : ", err)
		return nil, err
	}
	response := models.CheckTriggerResponse{}
	err = json.Unmarshal(respBody, &response)
	if err != nil {
		ss.log.Error("Error while reading unmarshalling response of SSH user access check from batch Check API : ", err)
		return nil, err
	}
	return &response, nil
}
