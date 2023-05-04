package trigger

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/restutils"
)

//HardwareResourceCountCheck - Calls the hardware-resource-count API and collates the response
func (hrc *CheckTrigger) HardwareResourceCountCheck(config models.Config) (models.CheckTriggerResponse, error) {
	hrc.Logger.Info("Performing Hardware resource count check from batch Check API")
	resp, err := restutils.Post("https://localhost/api/v1/checks/hardware-resource-count", config.Hardware)
	if err != nil {
		hrc.Logger.Error("Error while Performing Hardware resource count check from batch Check API", err)
		return models.CheckTriggerResponse{}, err
	}
	respBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		hrc.Logger.Error("Error while reading response of Hardware resource count check from batch Check API", err)
		return models.CheckTriggerResponse{}, err
	}
	response := models.CheckTriggerResponse{}
	json.Unmarshal(respBody, &response)
	return response, nil
}

func post(url string, body interface{}) (*http.Response, error) {
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
	return resp, nil
}
