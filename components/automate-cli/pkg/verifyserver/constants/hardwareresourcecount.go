package constants

const (
	IP_ADDRESS                      = "IP address"
	BACKEND_CLUSTER                 = "backend"
	FRONTEND_CLUSTER                = "frontend"
	UNIQUE_SUCCESS_MESSAGE          = "IP address is unique"
	UNIQUE_ERROR_MESSAGE            = "IP address is not unique"
	UNIQUE_RESOLUTION_MESSAGE       = "IP address for %s type should be unique."
	VALID_COUNT_SUCCESS_MESSAGE     = "%s type has valid count as per Automate HA requirement"
	VALID_COUNT_ERROR_MESSAGE       = "%s type has invalid count as per Automate HA requirement"
	VALID_COUNT_RESOLUTION_MESSAGE  = "Hardware Resource Count for %s type should be according to Automate HA requirements."
	VALID_FORMAT_SUCCESS_MESSAGE    = "IP address is of valid format"
	VALID_FORMAT_ERROR_MESSAGE      = "IP address is of invalid format"
	VALID_FORMAT_RESOLUTION_MESSAGE = "IP address %s should be changed in config to valid format."
	SHARED_SUCCESS_MESSAGE          = "Not shared with %s nodes"
	SHARED_ERROR_MESSAGE            = "IP address is common in %s type and %s type"
	SHARED_RESOLUTION_MESSAGE       = "Unique IP address should be used for %s type and %s type"
	MIN_AUTOMATE_REQ                = 2
	MIN_CHEF_INFRA_SERVER_REQ       = 2
	MIN_POSTGRESQL_REQ              = 3
	MIN_OPENSEARCH_REQ              = 3
)
