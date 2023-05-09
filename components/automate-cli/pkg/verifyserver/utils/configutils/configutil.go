package configutils

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

func GetIps(config models.Config) []string {
	var ipArray []string

	ipArray = append(ipArray, config.Hardware.AutomateNodeIps...)
	ipArray = append(ipArray, config.Hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, config.Hardware.OpenSearchNodeIps...)
	ipArray = append(ipArray, config.Hardware.PostgresqlNodeIps...)
	return ipArray
}
