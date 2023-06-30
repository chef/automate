package firewallchecktrigger

import (
	"fmt"
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

func NewFirewallCheck(log logger.Logger, port string) *FirewallCheck {
	return &FirewallCheck{
		log:  log,
		host: constants.LOCALHOST,
		port: port,
	}
}

func (fc *FirewallCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	fc.log.Info("Performing Firewall check from batch check ")

	if config.Hardware == nil {
		return trigger.HardwareNil(constants.FIREWALL, true, true, true)
	}

	requestMap := make(map[string][]models.FirewallRequest)
	makeRequests(config, requestMap)

	endPoint := fmt.Sprintf("http://%s:%s%s", fc.host, fc.port, constants.FIREWALL_API_PATH)

	response := triggerMultipleRequests(config, fc.log, endPoint, http.MethodPost, requestMap)

	return constructResponseFromOutput(response)
}

func (fc *FirewallCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := map[string]map[string][]int{
		constants.AUTOMATE: {
			constants.HTTP: []int{80},
		},
		constants.CHEF_INFRA_SERVER: {
			constants.HTTP: []int{80},
		},
		constants.POSTGRESQL: {
			constants.TCP: []int{7432, 9631, 5432, 6432},
			constants.UDP: []int{9638},
		},
		constants.OPENSEARCH: {
			constants.TCP: []int{9200, 9300, 9631},
			constants.UDP: []int{9638},
		},
	}
	return nodeTypePortMap
}

// The request response is being constructed based on the https://docs.chef.io/automate/ha_on_premises_deployment_prerequisites/#firewall-checks (Firewall Checks)
func makeRequests(config *models.Config, reqMap map[string][]models.FirewallRequest) {
	reqMap[constants.AUTOMATE] = getRequestsForAutomateAsSource(config)
	reqMap[constants.CHEF_INFRA_SERVER] = getRequestsForChefServerAsSource(config)
	reqMap[constants.POSTGRESQL] = getRequestsForPostgresAsSource(config)
	reqMap[constants.OPENSEARCH] = getRequestsForOpensearchAsSource(config)
	reqMap[constants.BASTION] = getRequestAsBastionSource(config)
}

// getRequestsForAutomateAsSource gives the requests for all the ports and types automate as source
func getRequestsForAutomateAsSource(config *models.Config) []models.FirewallRequest {
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

// getRequestsForChefServerAsSource gives the requests for all the ports where chefserver is the source
func getRequestsForChefServerAsSource(config *models.Config) []models.FirewallRequest {

	var reqBodies []models.FirewallRequest
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
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
func getRequestsForPostgresAsSource(config *models.Config) []models.FirewallRequest {
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
func getRequestsForOpensearchAsSource(config *models.Config) []models.FirewallRequest {
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

func getRequestAsBastionSource(config *models.Config) []models.FirewallRequest {
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
func triggerMultipleRequests(config *models.Config, log logger.Logger, endPoint, method string, requestsMap map[string][]models.FirewallRequest) []models.CheckTriggerResponse {
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

// constructResponseFromOutput constructing the final output from result in the format excepted by Batch Check service
func constructResponseFromOutput(result []models.CheckTriggerResponse) []models.CheckTriggerResponse {

	var response []models.CheckTriggerResponse
	resultMap := make(map[string]models.CheckTriggerResponse)

	for _, checkResponse := range result {
		key := checkResponse.Host + "_" + checkResponse.NodeType
		value, ok := resultMap[key]
		if ok {
			//Adding checks for the results
			newChecks := value.Result.Checks
			newChecks = append(newChecks, checkResponse.Result.Checks...)
			//Adding the new checks to the result
			value.Result.Passed = checkResponse.Result.Passed 
			value.Result.Skipped = checkResponse.Result.Skipped
			value.Result.Checks = newChecks
			if checkResponse.Result.Error != nil {
				value.Result.Error = checkResponse.Result.Error
			}
			resultMap[key] = value
		} else {
			resultMap[key] = checkResponse
		}

	}

	//Creating the check trigger response
	for _, checkResponses := range resultMap {
		response = append(response, checkResponses)
	}

	return response
}
