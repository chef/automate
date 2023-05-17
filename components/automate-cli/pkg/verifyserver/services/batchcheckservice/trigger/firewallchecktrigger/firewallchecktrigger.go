package firewallchecktrigger

import (
	"encoding/json"
	"io/ioutil"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type FirewallCheck struct {
	host string
	port string
	log  logger.Logger
}

type Request struct {
	Requests []ReqBody
}

type ReqBody struct {
	SourceNodeIP               string `json:"source_node_ip"`
	DestinationNodeIP          string `json:"destination_node_ip"`
	DestinationServicePort     string `json:"destination_service_port"`
	DestinationServiceProtocol string `json:"destination_service_protocol"`
	Cert                       string `json:"cert"`
	Key                        string `json:"key"`
	RootCert                   string `json:"root_cert"`
}

func NewFirewallCheck(log logger.Logger, port string) *FirewallCheck {
	return &FirewallCheck{
		log:  log,
		host: constants.LOCAL_HOST_URL,
		port: port,
	}
}

func (fc *FirewallCheck) Run(config models.Config) []models.CheckTriggerResponse {
	fc.log.Info("Performing Firewall check from batch check ")
	portMap := constructPorts()

	// Prepare request

	bx, _ := json.MarshalIndent(portMap, "", "\t")
	ioutil.WriteFile("abc.json", bx, 0777)
	return nil
	return trigger.RunCheckAllInstance(config, fc.log, fc.port, constants.FIREWALL_API_PATH, "", http.MethodPost, nil)
}
