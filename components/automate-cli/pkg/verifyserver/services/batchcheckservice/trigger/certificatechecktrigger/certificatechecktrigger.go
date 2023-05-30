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

func (ss *CertificateCheck) Run(config models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing Certificate check from batch check ")

	count := 0

	outputCh := make(chan models.CheckTriggerResponse, count)

	//This map will hold the Response against each IP
	var finalResult []models.CheckTriggerResponse

	certificate := config.Certificate

	hostMap := configutils.GetNodeTypeMap(config.Hardware)
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.CERTIFICATE_CHECK_API_PATH)
	for _, node := range certificate.Nodes {
		nodeTypes := hostMap[node.IP]
		//construct the request for Certificate Check API
		requestBody := models.CertificateCheckRequest{
			RootCertificate:  certificate.RootCert,
			PrivateKey:       node.Key,
			NodeCertificate:  node.Cert,
			AdminPrivateKey:  node.AdminKey,
			AdminCertificate: node.AdminCert,
		}

		for i := 0; i < len(nodeTypes); i++ {
			count++
			go trigger.TriggerCheckAPI(url, node.IP, nodeTypes[i], http.MethodPost, outputCh, requestBody)
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
