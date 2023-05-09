package trigger

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
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
		host: "http://localhost",
		port: port,
	}
}

func (ss *SshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{}
	return m
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
