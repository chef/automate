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

// type Request struct {
//  Requests []ReqBody
// }

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
	reqbody, _ := createReqBody(config)
	bx, _ := json.MarshalIndent(reqbody, "", "\t")
	ioutil.WriteFile("abc.json", bx, 0777)
	return nil
	return trigger.RunCheckAllInstance(config, fc.log, fc.port, constants.FIREWALL_API_PATH, "", http.MethodPost, nil)
}

func createReqBody(config models.Config) ([]ReqBody, error) {
	var reqBodies []ReqBody

	// Get the ports configuration
	ports := constructPorts()

	// source automate - dest postgres
	for _, sourceNodeIP := range config.Hardware.AutomateNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := ReqBody{
				SourceNodeIP:           sourceNodeIP,
				DestinationNodeIP:      destNodeIP,
				DestinationServicePort: "7432",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// source automate - dest postgres
	for _, sourceNodeIP := range config.Hardware.AutomateNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody := ReqBody{
				SourceNodeIP:           sourceNodeIP,
				DestinationNodeIP:      destNodeIP,
				DestinationServicePort: "9200",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// TODO: correct the rest
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody, err := createReqBodyForNodes(sourceNodeIP, destNodeIP, ports.ChefInfraServer)
			if err != nil {
				return nil, err
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	for _, sourceNodeIP := range config.Hardware.PostgresqlNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody, err := createReqBodyForNodes(sourceNodeIP, destNodeIP, ports.Postgresql)
			if err != nil {
				return nil, err
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// Add other pairings as needed for different server types

	return reqBodies, nil
}

func createReqBodyForNodes(sourceIP, destIP string, instances []Instance) (ReqBody, error) {
	var reqBody ReqBody

	// Find the matching instance configuration for the given server type
	var instanceConfig Instance
	for _, instance := range instances {
		if instance.Name == constants.OPENSEARCH {
			instanceConfig = instance
			break
		}
	}

	// Set the values in the request body
	reqBody.SourceNodeIP = sourceIP
	reqBody.DestinationNodeIP = destIP
	reqBody.DestinationServicePort = instanceConfig.PortProtocolMap["tcp"][0]
	reqBody.DestinationServiceProtocol = "tcp"
	reqBody.Cert = ""
	reqBody.Key = ""
	reqBody.RootCert = ""

	return reqBody, nil
}
