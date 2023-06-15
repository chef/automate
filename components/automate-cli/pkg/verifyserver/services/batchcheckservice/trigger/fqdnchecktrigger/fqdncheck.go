package fqdnchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
)

type FqdnCheck struct {
	port string
	log  logger.Logger
	host string
}

func NewFqdnCheck(log logger.Logger, port string) *FqdnCheck {
	return &FqdnCheck{
		log:  log,
		port: port,
		host: constants.LOCALHOST,
	}
}

func (fqc *FqdnCheck) Run(config models.Config) []models.CheckTriggerResponse {

	endPoint := checkutils.PrepareEndPoint(fqc.host, fqc.port, constants.FQDN_LOAD_BALANCER_CHECK)

	return triggerFqdnCheck(&config, endPoint, fqc.log)

}

// triggerFqdnCheck triggers all the fqdn requests for fqdn
func triggerFqdnCheck(config *models.Config, endPoint string, log logger.Logger) []models.CheckTriggerResponse {
	log.Debug("Trigger FQDN check for automate and chef server fqdn")
	var result []models.CheckTriggerResponse
	outputCh := make(chan models.CheckTriggerResponse)
	isAfterDeployment := false
	reqCount := 0

	if config.DeploymentState == constants.POST_DEPLOY {
		isAfterDeployment = true
	}

	if config.Certificate.AutomateFqdn != "" {
		for _, ip := range config.Hardware.AutomateNodeIps {
			log.Debugf("Trigger FQDN check for automate ip %s", ip)
			reqCount++
			req := getFqdnCheckRequest(ip, constants.AUTOMATE, config.Certificate.RootCert, config.Certificate.AutomateFqdn, isAfterDeployment, config.APIToken)
			go trigger.TriggerCheckAPI(endPoint, ip, constants.AUTOMATE, http.MethodPost, outputCh, req)
		}
	}

	if config.Certificate.ChefServerFqdn != "" {
		for _, ip := range config.Hardware.ChefInfraServerNodeIps {
			log.Debugf("Trigger FQDN check for chefserver ip %s", ip)
			reqCount++
			req := getFqdnCheckRequest(ip, constants.CHEF_INFRA_SERVER, config.Certificate.RootCert, config.Certificate.ChefServerFqdn, isAfterDeployment, config.APIToken)
			go trigger.TriggerCheckAPI(endPoint, ip, constants.CHEF_INFRA_SERVER, http.MethodPost, outputCh, req)

		}
	}

	for i := 0; i < reqCount; i++ {
		res := <-outputCh
		result = append(result, res)
	}

	close(outputCh)
	return result

}

// getFqdnCheckRequest creates req list for all the node ips with their fqdn
func getFqdnCheckRequest(ip, nodeType string, rootcert string, fqdn string, isAfterDeployment bool, apiToken string) models.FqdnRequest {

	fqdnReq := models.FqdnRequest{
		Fqdn:              fqdn,
		RootCert:          rootcert,
		IsAfterDeployment: isAfterDeployment,
		Nodes:             []string{ip},
		NodeType:          nodeType,
	}

	return fqdnReq
}

func (ss *FqdnCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := map[string]map[string][]int{
		constants.AUTOMATE: {
			constants.HTTPS: []int{443},
		},
		constants.CHEF_INFRA_SERVER: {
			constants.HTTPS: []int{443},
		},
	}
	return nodeTypePortMap
}
