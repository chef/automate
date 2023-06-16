package config

const (
	ROOT_CA             = "root_ca"
	PUBLIC_KEY          = "public_key"
	PRIVATE_KEY         = "private_key"
	ADMIN_KEY           = "admin_key"
	ADMIN_CERT          = "admin_cert"
	AUTOMATE            = "automate"
	CHEFSERVER          = "chef-infra-server"
	POSTGRESQL          = "postgresql"
	OPENSEARCH          = "opensearch"
	BASTION             = "bastion"
	INVALID_EMPTY_VALUE = "invalid or empty: %s"
	EMPTY_VALUE         = "empty value: %s"
	INVALID_STRING_TYPE = "invalid string type: %s"
	INVALID_FIELD_VALUE = "invalid value '%s' for field '%s'"
	INVALID_PORT_NUMBER = "invalid value '%s' for field '%s' port number must be between 1 and 65535"
	AWS                 = "aws"
	SELF_MANAGED        = "self-managed"
)
