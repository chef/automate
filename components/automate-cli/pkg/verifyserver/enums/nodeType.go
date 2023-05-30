package enums

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
)

type NodeType string

const (
	NodeTypeAutomate   NodeType = constants.AUTOMATE
	NodeTypeChefServer NodeType = constants.CHEF_INFRA_SERVER
	NodeTypePostgresql NodeType = constants.POSTGRESQL
	NodeTypeOpensearch NodeType = constants.OPENSEARCH
	NodeTypeBastion    NodeType = constants.BASTION
)

func getNodeTypeMap() map[string]NodeType {
	return map[string]NodeType{
		string(NodeTypeAutomate):   NodeTypeAutomate,
		string(NodeTypeChefServer): NodeTypeChefServer,
		string(NodeTypePostgresql): NodeTypePostgresql,
		string(NodeTypeOpensearch): NodeTypeOpensearch,
		string(NodeTypeBastion):    NodeTypeBastion,
	}
}

func getNodeTypeOptions() string {
	options := ""
	optionsMap := getNodeTypeMap()

	for option, _ := range optionsMap {
		options = options + option + ","
	}
	return options[:len(options)-1]
}

func GetNodeType(nodeType string) (NodeType, error) {
	value, present := getNodeTypeMap()[nodeType]

	if !present {
		return NodeType(""), errors.New("Invalid NodeType,valid value can be " + getNodeTypeOptions())
	}
	return value, nil
}
