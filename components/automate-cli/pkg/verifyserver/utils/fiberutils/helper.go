package fiberutils

import (
	"fmt"
	"net/url"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func GetHostFormEndPoint(endpoint string) (string, error) {
	parsedURL, err := url.Parse(endpoint)
	if err != nil {
		logrus.Errorf("cannot parse the endpoint, error: %+v", err)
		return "", err
	}

	return parsedURL.Hostname(), nil
}

// ConstructIpAndNodeTypeMap prepare the map with key as IP address and value as node type
func ConstructIpAndNodeTypeMap(config models.Config) map[string]string {
	ipNodeMap := make(map[string]string)
	// For automate nodes
	for _, ip := range config.Hardware.AutomateNodeIps {
		ipNodeMap[ip] = "automate"
	}
	// For Infra server nodes
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		ipNodeMap[ip] = "chef-infra-server"
	}

	// For PG nodes
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		ipNodeMap[ip] = "postgresql"
	}
	// For OS nodes
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		ipNodeMap[ip] = "opensearch"
	}

	return ipNodeMap
}

func HostToEndPoint(host, port, path, NodeType string) string {
	if NodeType == "" {
		endPoint := fmt.Sprintf("%s:%s%s", host, port, path)
		return endPoint
	}

	endPoint := fmt.Sprintf("%s:%s%s?node_type=%s", host, port, path, NodeType)
	return endPoint
}

func SendError(endPoint, host, errMsg string, errCode int, output chan<- models.CheckTriggerResponse, ctr models.CheckTriggerResponse) {
	//host, _ := GetHostFormEndPoint(endPoint)
	ctr.Host = host
	ctr.Error = &fiber.Error{Code: errCode, Message: errMsg}
	output <- ctr
}
