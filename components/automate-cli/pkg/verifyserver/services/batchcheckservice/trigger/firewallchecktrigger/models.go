package firewallchecktrigger

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
)

type HWPorts struct {
	Bastion         []Instance `json:"Bastion"`
	ChefAutomate    []Instance `json:"Chef Automate"`
	ChefInfraServer []Instance `json:"Chef Infra Server"`
	OpenSearch      []Instance `json:"OpenSearch"`
	Postgresql      []Instance `json:"Postgresql"`
}

type Instance struct {
	Name            string              `json:"name"`
	PortProtocolMap map[string][]string `json:"ports"`
}

func constructPorts() HWPorts {
	return HWPorts{

		// Bastion to automate, chef infra, postgres, opensearch, loadbalancer
		Bastion: []Instance{
			{
				Name: constants.AUTOMATE,
				PortProtocolMap: map[string][]string{
					"tcp": {"22", "9631", "9638"},
				},
			},
			{
				Name: constants.CHEF_INFRA_SERVER,
				PortProtocolMap: map[string][]string{
					"tcp": {"22", "9631", "9638"},
				},
			},
			{
				Name: constants.POSTGRESQL,
				PortProtocolMap: map[string][]string{
					"tcp": {"22", "9631", "9638", "7432"},
				},
			},
			{
				Name: constants.OPENSEARCH,
				PortProtocolMap: map[string][]string{
					"tcp": {"22", "9631", "9638", "9200"},
				},
			},
		},

		ChefAutomate: []Instance{
			{
				Name: constants.POSTGRESQL,
				PortProtocolMap: map[string][]string{
					"tcp": {"7432"},
				},
			},
			{
				Name: constants.OPENSEARCH,
				PortProtocolMap: map[string][]string{
					"tcp": {"9200"},
				},
			},
		},
		ChefInfraServer: []Instance{
			{
				Name: constants.POSTGRESQL,
				PortProtocolMap: map[string][]string{
					"tcp": {"7432"},
				},
			},
			{
				Name: constants.OPENSEARCH,
				PortProtocolMap: map[string][]string{
					"tcp": {"9200"},
				},
			},
		},
		OpenSearch: []Instance{
			{
				Name: constants.OPENSEARCH,
				PortProtocolMap: map[string][]string{
					"tcp": {"9631", "9200", "9300", "9638"},
					"udp": {"9638"},
				},
			},
		},
		Postgresql: []Instance{
			{
				Name: constants.POSTGRESQL,
				PortProtocolMap: map[string][]string{
					"tcp": {"9631", "7432", "5432", "6432", "9638"},
					"udp": {"9638"},
				},
			},
		},
	}
}
