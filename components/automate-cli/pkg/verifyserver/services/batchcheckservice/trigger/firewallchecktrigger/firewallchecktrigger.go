package firewallchecktrigger

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/lib/logger"
)

type FirewallCheck struct {
	host string
	port string
	log  logger.Logger
}

func NewFirewallCheck(log logger.Logger, port string) *FirewallCheck {
	return &FirewallCheck{
		log:  log,
		host: constants.LOCALHOST,
		port: port,
	}
}

func (fc *FirewallCheck) Run(config models.Config) []models.CheckTriggerResponse {
	fc.log.Info("Performing Firewall check from batch check ")

	requestMap := make(map[string][]models.FirewallRequest)
	makeRequests(config, requestMap)

	endPoint := fmt.Sprintf("http://%s:%s%s", fc.host, fc.port, constants.FIREWALL_API_PATH)

	response := triggerMultipleRequests(config, fc.log, endPoint, http.MethodPost, requestMap)

	return response
}

// The request response is being constructed based on the https://docs.chef.io/automate/ha_on_premises_deployment_prerequisites/#firewall-checks (Firewall Checks)
func makeRequests(config models.Config, reqMap map[string][]models.FirewallRequest) {
	reqMap[constants.AUTOMATE] = getRequestsForAutomateAsSource(config)
	reqMap[constants.CHEF_INFRA_SERVER] = getRequestsForChefServerAsSource(config)
	reqMap[constants.POSTGRESQL] = getRequestsForPostgresAsSource(config)
	reqMap[constants.OPENSEARCH] = getRequestsForOpensearchAsSource(config)
	reqMap[constants.BASTION] = getRequestAsBastionSource(config)
}

// getRequestsForAutomateAsSource gives the requests for all the ports and types automate as source
func getRequestsForAutomateAsSource(config models.Config) []models.FirewallRequest {
	var reqBodies []models.FirewallRequest

	for _, sourceNodeIP := range config.Hardware.AutomateNodeIps {
		// Dest postgres
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "7432",
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}

		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			//Dest Opensearch
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9200",
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	return reqBodies

}

func getRootCertForNodeWithNodeTypeAndIP(certMap map[string]models.Certificate, nodeType string, nodeIp string) string {

	nodesCert, found := certMap[nodeType]
	if found {
		for _, cert := range nodesCert.Nodes {
			if cert.IP == nodeIp {
				return cert.RootCert
			}
		}
	}

	return ""
}

// getRequestsForChefServerAsSource gives the requests for all the ports where chefserver is the source
func getRequestsForChefServerAsSource(config models.Config) []models.FirewallRequest {

	var reqBodies []models.FirewallRequest
	certMap := configutils.GetCertificateMap(config.Certificate)
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		//Dest Automate
		for _, destNodeIP := range config.Hardware.AutomateNodeIps {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "443",
				DestinationServiceProtocol: "https",
				RootCert:                   getRootCertForNodeWithNodeTypeAndIP(certMap, constants.AUTOMATE, destNodeIP),
			}
			reqBodies = append(reqBodies, reqBody)
		}

		// Dest postgres
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "7432",
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}

		// Dest opensearch
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9200",
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}

	}

	return reqBodies
}

// getRequestsForPostgresAsSource gives the requests for all the ports where postgres is source
func getRequestsForPostgresAsSource(config models.Config) []models.FirewallRequest {
	var reqBodies []models.FirewallRequest
	for _, sourceNodeIP := range config.Hardware.PostgresqlNodeIps {
		//Dest Other postgres nodes
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			if sourceNodeIP == destNodeIP {
				continue
			}
			//for Upd
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9638",
				DestinationServiceProtocol: "udp",
			}
			reqBodies = append(reqBodies, reqBody)

			//for tcp
			for _, port := range postgresqlTCPPorts {
				reqBody := models.FirewallRequest{
					SourceNodeIP:               sourceNodeIP,
					DestinationNodeIP:          destNodeIP,
					DestinationServicePort:     port,
					DestinationServiceProtocol: "tcp",
				}
				reqBodies = append(reqBodies, reqBody)
			}

		}
	}

	return reqBodies
}

// getRequestsForOpensearchAsSource gives the requests for all the ports where opensearch is source
func getRequestsForOpensearchAsSource(config models.Config) []models.FirewallRequest {
	var reqBodies []models.FirewallRequest
	for _, sourceNodeIP := range config.Hardware.OpenSearchNodeIps {
		//other postgres nodes
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			if sourceNodeIP == destNodeIP {
				continue
			}
			//for upd
			reqBody := models.FirewallRequest{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9638",
				DestinationServiceProtocol: "udp",
			}
			reqBodies = append(reqBodies, reqBody)

			//for tcp
			for _, port := range opensearchTCPPorts {
				reqBody := models.FirewallRequest{
					SourceNodeIP:               sourceNodeIP,
					DestinationNodeIP:          destNodeIP,
					DestinationServicePort:     port,
					DestinationServiceProtocol: "tcp",
				}
				reqBodies = append(reqBodies, reqBody)

			}

		}
	}

	return reqBodies
}

func getRequestAsBastionSource(config models.Config) []models.FirewallRequest {
	var reqBodies []models.FirewallRequest

	for _, destNodeIP := range config.Hardware.AutomateNodeIps {
		//Dest Chef Infra server
		for _, port := range a2CsTCPPorts {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               constants.LOCALHOST,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Chef Infra
	for _, destNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, port := range a2CsTCPPorts {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               constants.LOCALHOST,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Postgres
	for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
		for _, port := range postgresBastionPorts {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               constants.LOCALHOST,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Opensearch
	for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
		for _, port := range ossBastionPorts {
			reqBody := models.FirewallRequest{
				SourceNodeIP:               constants.LOCALHOST,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	return reqBodies
}

// triggerMultipleRequests triggers multiple requests for firewall api on bastion host
func triggerMultipleRequests(config models.Config, log logger.Logger, endPoint, method string, requestsMap map[string][]models.FirewallRequest) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	outputCh := make(chan models.CheckTriggerResponse)
	reqCount := 0

	for nodeType, requests := range requestsMap {
		for _, reqBody := range requests {
			reqCount++
			go trigger.TriggerCheckAPI(endPoint, reqBody.SourceNodeIP, nodeType, method, outputCh, reqBody)
		}

	}

	for i := 0; i < reqCount; i++ {
		select {
		case res := <-outputCh:
			result = append(result, res)
		}
	}

	close(outputCh)
	return result
}
