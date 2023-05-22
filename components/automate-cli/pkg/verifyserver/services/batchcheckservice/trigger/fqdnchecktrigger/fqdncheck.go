package fqdnchecktrigger

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
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
		host: "127.0.0.1",
	}
}

func (fqc *FqdnCheck) Run(config models.Config) []models.CheckTriggerResponse {

	reqList := getRequestsForFqdn(config)
	endPointForRequest := fmt.Sprintf("http://%s:%s%s", fqc.host, fqc.port, constants.FQDN_LOAD_BALANCER_CHECK)

	response := trigger.RunCheckWithEndPointSpecified(endPointForRequest, fqc.log, reqList, http.MethodPost)

	return response

}

// getRequestsForFqdn gets all the requests for automate and chef-server for fqdn
func getRequestsForFqdn(config models.Config) []models.NodeIpRequest {
	IsAfterDeployment := false
	var nodeIpRequestList []models.NodeIpRequest

	if config.DeploymentState == "post-deploy" {
		IsAfterDeployment = true
	}

	nodeIpRequestList = append(nodeIpRequestList, getNodeRequests(config.Hardware.AutomateNodeIps, constants.AUTOMATE, config.Certificate.RootCert, config.Certificate.AutomateFqdn, IsAfterDeployment)...)

	nodeIpRequestList = append(nodeIpRequestList, getNodeRequests(config.Hardware.ChefInfraServerNodeIps, constants.CHEF_INFRA_SERVER, config.Certificate.RootCert, config.Certificate.ChefServerFqdn, IsAfterDeployment)...)

	return nodeIpRequestList
}

// getNodeRequests get the request with the node id and node type
func getNodeRequests(nodeIps []string, nodeType string, rootcert string, fqdn string, isAfterDeployment bool) []models.NodeIpRequest {
	var nodeIpRequestList []models.NodeIpRequest
	for _, ip := range nodeIps {
		fqdnReq := models.FqdnRequest{
			Fqdn:              fqdn,
			RootCert:          rootcert,
			IsAfterDeployment: isAfterDeployment,
			Nodes:             []string{ip},
		}

		nodeIpRequest := models.NodeIpRequest{
			NodeType: nodeType,
			NodeIP:   ip,
			Request:  fqdnReq,
		}

		nodeIpRequestList = append(nodeIpRequestList, nodeIpRequest)

	}

	return nodeIpRequestList
}
