package constants

type NodeType string

const (
	AUTOMATE          = "automate"
	CHEF_INFRA_SERVER = "chef-infra-server"
	POSTGRESQL        = "postgresql"
	OPENSEARCH        = "opensearch"
	BASTION           = "bastion"
)

const (
	NodeTypeAutomate   NodeType = AUTOMATE
	NodeTypeChefServer NodeType = CHEF_INFRA_SERVER
	NodeTypePostgresql NodeType = POSTGRESQL
	NodeTypeOpensearch NodeType = OPENSEARCH
	NodeTypeBastion    NodeType = BASTION
)
