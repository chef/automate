package configutils

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func GetIps(hardware models.Hardware) []string {
	var ipArray []string

	ipArray = append(ipArray, hardware.AutomateNodeIps...)
	ipArray = append(ipArray, hardware.ChefInfraServerNodeIps...)
	ipArray = append(ipArray, hardware.PostgresqlNodeIps...)
	ipArray = append(ipArray, hardware.OpenSearchNodeIps...)
	return ipArray
}

func GetNodeTypeMap(hardware models.Hardware) map[string][]string {
	hostMap := make(map[string][]string)

	for _, ip := range hardware.AutomateNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.AUTOMATE)
	}
	for _, ip := range hardware.ChefInfraServerNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.CHEF_INFRA_SERVER)
	}
	for _, ip := range hardware.PostgresqlNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.POSTGRESQL)
	}
	for _, ip := range hardware.OpenSearchNodeIps {
		hostMap[ip] = append(hostMap[ip], constants.OPENSEARCH)
	}
	return hostMap
}

func GetCertificateMap(certificateList []models.Certificate) map[string]models.Certificate {

	certificateMap := make(map[string]models.Certificate)

	for _, certificate := range certificateList {
		certificateMap[certificate.NodeType] = certificate
	}

	return certificateMap

}
