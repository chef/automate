package certificatechecktrigger

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
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
	if config.Hardware == nil {
		return trigger.NilResp(constants.CERTIFICATE, true, true, false)
	}
	// Check if certificate is empty or nil
	if config.Certificate == nil {
		return nilCertificateResp(config, constants.CERTIFICATE)
	}
	if IsCertificateEmpty(config.Certificate) {
		return emptyCertificateResp(config, constants.CERTIFICATE)
	}

	count := 0

	outputCh := make(chan models.CheckTriggerResponse, count)

	//This map will hold the Response against each IP
	var finalResult []models.CheckTriggerResponse

	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.CERTIFICATE_CHECK_API_PATH)
	for _, certificate := range config.Certificate {
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

func nilCertificateResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, GetSkippedTriggerCheckResp(ip, checktype, constants.AUTOMATE))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, GetSkippedTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, GetSkippedTriggerCheckResp(ip, checktype, constants.POSTGRESQL))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, GetSkippedTriggerCheckResp(ip, checktype, constants.OPENSEARCH))
	}

	return resps
}

func GetSkippedTriggerCheckResp(ip, checktype, nodeType string) models.CheckTriggerResponse {
	return models.CheckTriggerResponse{
		NodeType:  nodeType,
		CheckType: checktype,
		Result: models.ApiResult{
			Passed:  false,
			Skipped: true,
			Check:   checktype,
		},
		Host: ip,
	}
}

func IsCertificateEmpty(certificate []*models.Certificate) bool {
	return len(certificate) == 0
}

func emptyCertificateResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.AUTOMATE, "Certificate is missing"))
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.CHEF_INFRA_SERVER, "Certificate is missing"))
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.POSTGRESQL, "Certificate is missing"))
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		resps = append(resps, trigger.GetErrTriggerCheckResp(ip, checktype, constants.OPENSEARCH, "Certificate is missing"))
	}

	return resps
}
