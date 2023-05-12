package constants

const (
	UNIQUE_SUCCESS_MESSAGE          = "IP address is unique"
	UNIQUE_ERROR_MESSAGE            = "IP address is not unique"
	UNIQUE_RESOLUTION_MESSAGE       = "IP address for %s Type should be unique."
	VALID_COUNT_SUCCESS_MESSAGE     = "%s Type has valid count as per Automate HA requirement"
	VALID_COUNT_ERROR_MESSAGE       = "%s Type has invalid count as per Automate HA requirement"
	VALID_COUNT_RESOLUTION_MESSAGE  = "Hardware Resouce Count for %s Type should be according to Automate HA requirements."
	VALID_FORMAT_SUCCESS_MESSAGE    = "IP address is of valid format"
	VALID_FORMAT_ERROR_MESSAGE      = "IP address is of invalid format"
	VALID_FORMAT_RESOLUTION_MESSAGE = "IP address %s should be changed in config to valid format."
	SHARED_SUCCESS_MESSAGE          = "Not shared with backend nodes"
	SHARED_ERROR_MESSAGE            = "IP address is common in %s Type and %s Type"
	SHARED_RESOLUTION_MESSAGE       = "Unique IP address should be used for %s Type and %s Type"
)
