package certificatechecktrigger

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
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

func (ss *CertificateCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing Certificate check from batch check ")

	// Check if certificate is empty or nil
	if isCertificateNil(config.Certificate) {
		return trigger.ConstructNilResp(config, constants.CERTIFICATE)
	}
	if isCertificateEmpty(config.Certificate) {
		return trigger.ConstructEmptyResp(config, constants.CERTIFICATE, constants.MISSING_CERTIFICATE)
	}

	count := 0

	outputCh := make(chan models.CheckTriggerResponse, count)

	//This map will hold the Response against each IP
	var finalResult []models.CheckTriggerResponse

	hardwareMap := configutils.GetNodeTypeMap(config.Hardware)

	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.CERTIFICATE_CHECK_API_PATH)
	for _, certificate := range config.Certificate {
		if len(certificate.Nodes) == 0 {
			finalResult = append(finalResult, skipCertificateForAutomateAndChefServerNodes(certificate.NodeType, hardwareMap)...)
			continue
		}
		for _, node := range certificate.Nodes {
			//construct the request for Certificate Check API
			requestBody := models.CertificateCheckRequest{
				RootCertificate:  node.RootCert,
				PrivateKey:       node.Key,
				NodeCertificate:  node.Cert,
				AdminPrivateKey:  node.AdminKey,
				AdminCertificate: node.AdminCert,
			}

			count++
			go trigger.TriggerCheckAPI(url, node.IP, certificate.NodeType, http.MethodPost, outputCh, requestBody)
		}

	}

	//Read response from output channel
	for i := 0; i < count; i++ {
		resp := <-outputCh
		finalResult = append(finalResult, resp)
	}
	close(outputCh)
	return finalResult
}

func (ss *CertificateCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

func isCertificateEmpty(certificate []*models.Certificate) bool {
	return len(certificate) == 0
}

func isCertificateNil(certificate []*models.Certificate) bool {
	return certificate == nil
}

func skipCertificateForAutomateAndChefServerNodes(nodeType string, hardwareMap map[string][]string) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	for ip, nodeTypes := range hardwareMap {
		for _, nodeTypeMap := range nodeTypes {
			if nodeTypeMap == nodeType {
				result = append(result, trigger.SkippedTriggerCheckResp(ip, constants.CERTIFICATE, nodeTypeMap))
			}
		}
	}

	return result

}
