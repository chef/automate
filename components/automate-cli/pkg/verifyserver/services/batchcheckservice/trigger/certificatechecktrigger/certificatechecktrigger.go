package certificatechecktrigger

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

func (ss *CertificateCheck) Run(config models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing Certificate check from batch check ")

	var requests []interface{}

	hostMap := configutils.GetNodeTypeMap(config)
	for _, node := range config.Certificate.Nodes {
		nodeTypes := hostMap[node.IP]
		//construct the request for Certificate Check API
		requestBody := models.CertificateCheckRequest{
			RootCertificate:  config.Certificate.RootCert,
			PrivateKey:       node.Key,
			NodeCertificate:  node.Cert,
			AdminPrivateKey:  node.AdminKey,
			AdminCertificate: node.AdminCert,
		}

		for i := 0; i < len(nodeTypes); i++ {
			requests = append(requests, requestBody)
		}
	}

	return trigger.RunParallelChecksWithRequest(config, ss.log, ss.port, constants.SSH_USER_CHECK_API_PATH, "", http.MethodPost, requests)

	//COMMENTING OLD CHANGES _ WILL REMOVE AFTER FINAL REVIEW

	// count := config.Hardware.AutomateNodeCount + config.Hardware.ChefInfraServerNodeCount +
	// 	config.Hardware.PostgresqlNodeCount + config.Hardware.OpenSearchNodeCount

	// outputCh := make(chan models.CheckTriggerResponse, count)

	// //This map will hold the Response against each IP
	// var finalResult []models.CheckTriggerResponse

	// certificate := config.Certificate

	// hostMap := configutils.GetNodeTypeMap(config)
	// for _, node := range certificate.Nodes {
	// 	nodeTypes := hostMap[node.IP]
	// 	//construct the request for Certificate Check API
	// 	requestBody := models.CertificateCheckRequest{
	// 		RootCertificate:  certificate.RootCert,
	// 		PrivateKey:       node.Key,
	// 		NodeCertificate:  node.Cert,
	// 		AdminPrivateKey:  node.AdminKey,
	// 		AdminCertificate: node.AdminCert,
	// 	}

	// 	for i := 0; i < len(nodeTypes); i++ {
	// 		go ss.TriggerCheckAndFormatOutput(node.IP, nodeTypes[i], requestBody, outputCh)
	// 	}
	// }

	// //Read response from output channel
	// for i := 0; i < count; i++ {
	// 	resp := <-outputCh
	// 	finalResult = append(finalResult, resp)
	// }
	// close(outputCh)
	// return finalResult
}

func (ss *CertificateCheck) TriggerCheckAndFormatOutput(host string, nodeType string, body interface{}, output chan<- models.CheckTriggerResponse) {
	var checkResp models.CheckTriggerResponse
	resp, err := ss.TriggerCertificateCheck(body)
	if err != nil {
		checkResp = checkutils.PrepareTriggerResponse(nil, host, nodeType, err.Error(), constants.CERTIFICATE, constants.CERTIFICATE_MSG, true)
	} else {
		checkResp = checkutils.PrepareTriggerResponse(resp, host, nodeType, "", constants.CERTIFICATE, constants.CERTIFICATE_MSG, false)
	}
	output <- checkResp

}

// TriggerHardwareResourceCountCheck - Call the Hardware resource API and format response
func (ss *CertificateCheck) TriggerCertificateCheck(body interface{}) (
	*models.CheckTriggerResponse, error) {
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.CERTIFICATE_CHECK_API_PATH)
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
