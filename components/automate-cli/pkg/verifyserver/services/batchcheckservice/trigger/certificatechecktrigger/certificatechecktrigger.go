package certificatechecktrigger

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

type CertificateCheck struct {
	host string
	port string
	log  logger.Logger
}

func NewCertificateCheck(log logger.Logger, port string) *CertificateCheck {
	return &CertificateCheck{
		log:  log,
		host: constants.LOCAL_HOST_URL,
		port: port,
	}
}

func (ss *CertificateCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	ss.log.Info("Performing Certificate check from batch check ")
	count := config.Hardware.AutomateNodeCount + config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount + config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse, count)

	//This map will hold the Response against each IP
	finalResult := make(map[string]models.CheckTriggerResponse)

	certificate := config.Certificate

	for _, node := range certificate.Nodes {

		//construct the request for Certificate Check API
		requestBody := models.CertificateCheckRequest{
			RootCertificate:  certificate.RootCert,
			PrivateKey:       node.Key,
			NodeCertificate:  node.Cert,
			AdminPrivateKey:  node.AdminKey,
			AdminCertificate: node.AdminCert,
		}
		go ss.TriggerCheckAndFormatOutput(node.IP, requestBody, outputCh)
	}

	//Read response from output channel
	for i := 0; i < count; i++ {
		resp := <-outputCh
		finalResult[resp.Host] = resp
	}
	close(outputCh)
	return finalResult
}

func (ss *CertificateCheck) TriggerCheckAndFormatOutput(host string, body interface{}, output chan<- models.CheckTriggerResponse) {
	var checkResp models.CheckTriggerResponse
	resp, err := ss.TriggerCertificateCheck(body)
	if err != nil {
		checkResp = models.CheckTriggerResponse{
			Error: fiber.NewError(fiber.StatusServiceUnavailable, err.Error()),
			Result: models.ApiResult{
				Passed:  false,
				Check:   constants.CERTIFICATE,
				Message: constants.CERTIFICATE_MSG,
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
				Check:   constants.CERTIFICATE,
				Message: constants.CERTIFICATE_MSG,
				Checks:  resp.Result.Checks,
			},
			Host: host,
		}
	}
	output <- checkResp

}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *CertificateCheck) TriggerCertificateCheck(body interface{}) (
	*models.CheckTriggerResponse, error) {
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.SSH_USER_CHECK_API_PATH)
	resp, err := httputils.MakeRequest(http.MethodPost, url, body)
	if err != nil {
		ss.log.Error("Error while Performing Certificate check from batch Check API : ", err)
		return nil, err
	}
	respBody, err := ioutil.ReadAll(resp.Body) // nosemgrep
	if err != nil {
		ss.log.Error("Error while reading response of Certificate check from batch Check API : ", err)
		return nil, err
	}
	response := models.CheckTriggerResponse{}
	err = json.Unmarshal(respBody, &response)
	if err != nil {
		ss.log.Error("Error while reading unmarshalling response of Certificate check from batch Check API : ", err)
		return nil, err
	}
	return &response, nil
}
