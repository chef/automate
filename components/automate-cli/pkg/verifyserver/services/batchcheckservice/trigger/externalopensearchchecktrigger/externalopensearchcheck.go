package externalopensearchchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
)

type ExternalOpensearchCheck struct {
	log  logger.Logger
	port string
}

func NewExternalOpensearchCheck(log logger.Logger, port string) *ExternalOpensearchCheck {
	return &ExternalOpensearchCheck{
		log:  log,
		port: port,
	}
}

func (eoc *ExternalOpensearchCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return runCheckForOpensearch(config, constants.EXTERNAL_OPENSEARCH_API_PATH, eoc.port, eoc.log)
}

func runCheckForOpensearch(config models.Config, path string, port string, log logger.Logger) []models.CheckTriggerResponse {
	log.Debug("Trigger Opensearch check for automate and chef server nodes")
	req := models.ExternalOS{
		OSDomainName:   config.ExternalOS.OSDomainName,
		OSDomainURL:    config.ExternalOS.OSDomainURL,
		OSUsername:     config.ExternalOS.OSUsername,
		OSUserPassword: config.ExternalOS.OSUserPassword,
		OSCert:         config.ExternalOS.OSCert,
	}
	var result, result2 []models.CheckTriggerResponse
	outputCh := make(chan models.CheckTriggerResponse)
	count := 0
	for _, ip := range config.Hardware.AutomateNodeIps {
		log.Debugf("Trigger Opensearch check for automate ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, path)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.AUTOMATE, http.MethodPost, outputCh, req)
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		log.Debugf("Trigger Opensearch check for chefserver ip %s", ip)
		count++
		endPoint := checkutils.PrepareEndPoint(ip, port, path)
		go trigger.TriggerCheckAPI(endPoint, ip, constants.CHEF_INFRA_SERVER, http.MethodPost, outputCh, req)

	}

	for i := 0; i < count; i++ {
		res := <-outputCh
		if res.NodeType == "automate" {
			result = append(result, res)
		} else if res.NodeType == "chef-infra-server" {
			result2 = append(result2, res)
		}
	}
	result = append(result, result2...)

	close(outputCh)
	return result

}
