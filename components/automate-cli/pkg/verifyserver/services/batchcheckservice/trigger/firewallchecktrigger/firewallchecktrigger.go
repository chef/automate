package firewallchecktrigger

import (
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

type Ports struct {
	PortCheckFromAutomate    map[string][]string
	PortCheckFromInfraServer map[string][]string
	PortCheckFromPostgres    map[string][]string
	PortCheckFromOpensearch  map[string][]string
	// Add for loadbalancer
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

	// ports := Ports{
	// 	PortCheckFromAutomate: map[string][]string{},
	// }
	// var body []ReqBody

	// for i := 0; i < config.Hardware.AutomateNodeCount; i++ {
	// 	reqBody := ReqBody{
	// 		SourceNodeIP:               "127.0.0.1",
	// 		DestinationNodeIP:          config.Hardware.AutomateNodeIps[i],
	// 		DestinationServicePort:     fc.port,
	// 		DestinationServiceProtocol: "tcp",
	// 		RootCert:                   config.Certificate.RootCert,
	// 	}
	// 	body = append(body, reqBody)
	// }

	// for i := 0; i < config.Hardware.ChefInfraServerNodeCount; i++ {
	// 	reqBody := ReqBody{
	// 		SourceNodeIP:               "127.0.0.1",
	// 		DestinationNodeIP:          config.Hardware.ChefInfraServerNodeIps[i],
	// 		DestinationServicePort:     fc.port,
	// 		DestinationServiceProtocol: "tcp",
	// 		RootCert:                   config.Certificate.RootCert,
	// 	}
	// 	body = append(body, reqBody)
	// }

	// for i := 0; i < config.Hardware.OpenSearchNodeCount; i++ {
	// 	reqBody := ReqBody{
	// 		SourceNodeIP:               "127.0.0.1",
	// 		DestinationNodeIP:          config.Hardware.OpenSearchNodeIps[i],
	// 		DestinationServicePort:     fc.port,
	// 		DestinationServiceProtocol: "tcp",
	// 		RootCert:                   config.Certificate.RootCert,
	// 	}
	// 	body = append(body, reqBody)
	// }

	// for i := 0; i < config.Hardware.PostgresqlNodeCount; i++ {
	// 	reqBody := ReqBody{
	// 		SourceNodeIP:               "127.0.0.1",
	// 		DestinationNodeIP:          config.Hardware.PostgresqlNodeIps[i],
	// 		DestinationServicePort:     fc.port,
	// 		DestinationServiceProtocol: "tcp",
	// 		RootCert:                   config.Certificate.RootCert,
	// 	}
	// 	body = append(body, reqBody)
	// }

	return trigger.RunCheck(config, fc.log, fc.port, constants.FIREWALL_API_PATH, "", http.MethodPost, nil)
}
