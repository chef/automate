package sshuseraccesschecktrigger

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
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

func (ss *SshUserAccessCheck) Run(config models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing SSH user access check from batch check ")

	var requests []interface{}

	hostMap := configutils.GetNodeTypeMap(config)
	for ip, types := range hostMap {
		for i := 0; i < len(types); i++ {
			var requestBody map[string]interface{}
			data, _ := json.Marshal(config.SSHUser)
			json.Unmarshal(data, &requestBody)
			requestBody[ip] = ip
			requests = append(requests, requestBody)
		}

	}

	return trigger.RunParallelChecksWithRequest(config, ss.log, ss.port, constants.SSH_USER_CHECK_API_PATH, "", http.MethodPost, requests)

	//COMMENTING OLD CHANGES _ WILL REMOVE AFTER FINAL REVIEW

	// ss.log.Info("Performing SSH user access check from batch check ")
	// count := config.Hardware.AutomateNodeCount + config.Hardware.ChefInfraServerNodeCount +
	// 	config.Hardware.PostgresqlNodeCount + config.Hardware.OpenSearchNodeCount

	// outputCh := make(chan models.CheckTriggerResponse, count)

	// var finalResult []models.CheckTriggerResponse
	// hostMap := configutils.GetNodeTypeMap(config)
	// for ip, types := range hostMap {
	// 	for i := 0; i < len(types); i++ {
	// 		var requestBody map[string]interface{}
	// 		data, _ := json.Marshal(config.SSHUser)
	// 		json.Unmarshal(data, &requestBody)
	// 		requestBody[ip] = ip
	// 		go ss.TriggerCheckAndFormatOutput(ip, types[i], requestBody, outputCh)
	// 	}

	// }

	// for i := 0; i < count; i++ {
	// 	resp := <-outputCh
	// 	finalResult = append(finalResult, resp)
	// }
	// close(outputCh)
	// return finalResult
}

func (ss *SshUserAccessCheck) TriggerCheckAndFormatOutput(host string, nodeType string, body interface{}, output chan<- models.CheckTriggerResponse) {
	var checkResp models.CheckTriggerResponse
	resp, err := ss.TriggerSshUserAccessCheck(body)
	if err != nil {
		checkResp = checkutils.PrepareTriggerResponse(nil, host, nodeType, err.Error(), constants.SSH_USER, constants.SSH_USER_MSG, true)
	} else {
		checkResp = checkutils.PrepareTriggerResponse(resp, host, nodeType, "", constants.SSH_USER, constants.SSH_USER_MSG, false)
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
