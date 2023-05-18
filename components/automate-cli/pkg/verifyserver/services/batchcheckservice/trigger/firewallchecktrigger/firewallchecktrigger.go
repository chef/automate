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

// type ReqBody struct {
// 	SourceNodeIP               string `json:"source_node_ip"`
// 	DestinationNodeIP          string `json:"destination_node_ip"`
// 	DestinationServicePort     string `json:"destination_service_port"`
// 	DestinationServiceProtocol string `json:"destination_service_protocol"`
// 	Cert                       string `json:"cert"`
// 	Key                        string `json:"key"`
// 	RootCert                   string `json:"root_cert"`
// }

func NewFirewallCheck(log logger.Logger, port string) *FirewallCheck {
	return &FirewallCheck{
		log:  log,
		host: constants.LOCAL_HOST_URL,
		port: port,
	}
}

func (fc *FirewallCheck) Run(config models.Config) []models.CheckTriggerResponse {
	fc.log.Info("Performing Firewall check from batch check ")
	requests := makeRequests(config)
	bx, _ := json.MarshalIndent("reqbody", "", "\t")
	ioutil.WriteFile("abc.json", bx, 0777)
	return trigger.RunCheckMultiRequests(config, fc.log, fc.port, constants.FIREWALL_API_PATH, "", http.MethodPost, requests)
}

// The request response is being constructed based on the https://docs.chef.io/automate/ha_on_premises_deployment_prerequisites/#firewall-checks (Firewall Checks)
// TODO: Check about Cert and Key
func makeRequests(config models.Config) []trigger.ReqBody {
	var reqBodies []trigger.ReqBody

	// _____________________________________________ Row 1: Chef Automate to all the OS and PG nodes _____________________________________________
	// Dest postgres
	for _, sourceNodeIP := range config.Hardware.AutomateNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "7432",
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// Dest opensearch
	for _, sourceNodeIP := range config.Hardware.AutomateNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9200",
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// _____________________________________________ Row 1: Chef Infra to all the OS and PG nodes _____________________________________________
	// Dest automate
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, destNodeIP := range config.Hardware.AutomateNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "443",
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// Dest postgres
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "7432",
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// Dest opensearch
	for _, sourceNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9200",
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	// _____________________________________________ Row 1: PSQL to all the PG nodes (TCP AND UDP ports) _____________________________________________
	// Dest postgres UDP
	for _, sourceNodeIP := range config.Hardware.PostgresqlNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9638",
				DestinationServiceProtocol: "udp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest postgres UDP
	for _, sourceNodeIP := range config.Hardware.PostgresqlNodeIps {
		for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
			for _, port := range postgresqlTCPPorts {
				reqBody := trigger.ReqBody{
					SourceNodeIP:               sourceNodeIP,
					DestinationNodeIP:          destNodeIP,
					DestinationServicePort:     port,
					DestinationServiceProtocol: "tcp",
					Cert:                       config.Certificate.Nodes[0].Cert,
					Key:                        config.Certificate.Nodes[0].Key,
					RootCert:                   config.Certificate.RootCert,
				}
				reqBodies = append(reqBodies, reqBody)
			}
		}
	}

	// _____________________________________________ Row 1: OS to all the OS nodes (TCP AND UDP ports) _____________________________________________
	// Dest Opensearch UDP
	for _, sourceNodeIP := range config.Hardware.OpenSearchNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               sourceNodeIP,
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     "9638",
				DestinationServiceProtocol: "udp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Opensearch UDP
	for _, sourceNodeIP := range config.Hardware.OpenSearchNodeIps {
		for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
			for _, port := range opensearchTCPPorts {
				reqBody := trigger.ReqBody{
					SourceNodeIP:               sourceNodeIP,
					DestinationNodeIP:          destNodeIP,
					DestinationServicePort:     port,
					DestinationServiceProtocol: "tcp",
					Cert:                       config.Certificate.Nodes[0].Cert,
					Key:                        config.Certificate.Nodes[0].Key,
					RootCert:                   config.Certificate.RootCert,
				}
				reqBodies = append(reqBodies, reqBody)
			}
		}
	}

	// _____________________________________________ Row 1: Bastion to all the HA nodes (TCP AND UDP ports) _____________________________________________
	// Dest Automate
	for _, destNodeIP := range config.Hardware.AutomateNodeIps {
		for _, port := range a2CsTCPPorts {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               "127.0.0.1", // Bastion host
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Chef Infra
	for _, destNodeIP := range config.Hardware.ChefInfraServerNodeIps {
		for _, port := range a2CsTCPPorts {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               "127.0.0.1", // Bastion host
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Postgres
	for _, destNodeIP := range config.Hardware.PostgresqlNodeIps {
		for _, port := range postgresBastionPorts {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               "127.0.0.1", // Bastion host
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}
	// Dest Chef Infra
	for _, destNodeIP := range config.Hardware.OpenSearchNodeIps {
		for _, port := range ossBastionPorts {
			reqBody := trigger.ReqBody{
				SourceNodeIP:               "127.0.0.1", // Bastion host
				DestinationNodeIP:          destNodeIP,
				DestinationServicePort:     port,
				DestinationServiceProtocol: "tcp",
				Cert:                       config.Certificate.Nodes[0].Cert,
				Key:                        config.Certificate.Nodes[0].Key,
				RootCert:                   config.Certificate.RootCert,
			}
			reqBodies = append(reqBodies, reqBody)
		}
	}

	return reqBodies
}
