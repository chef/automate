package configutils

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func GetIps(config models.Config) []string {
	var ipArray []string

	ipArray = append(ipArray, config.Hardware.AutomateNodeIps...)
	ipArray = append(ipArray, config.Hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, config.Hardware.PostgresqlNodeIps...)
	ipArray = append(ipArray, config.Hardware.OpenSearchNodeIps...)
	return ipArray
}

func GetNodeTypeMap(config models.Config) map[string][]string {
	hostMap := make(map[string][]string)

	for _, ip := range config.Hardware.AutomateNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.AUTOMATE)
	}
	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.CHEF_INFRA_SERVER)
	}
	for _, ip := range config.Hardware.PostgresqlNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.POSTGRESQL)
	}
	for _, ip := range config.Hardware.OpenSearchNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.OPENSEARCH)
	}
	return hostMap
}
