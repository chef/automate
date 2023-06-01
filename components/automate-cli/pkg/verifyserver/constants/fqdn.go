package constants

const (
	FQDN_TITLE                     = "FQDN is reachable"
	FQDN_SUCCESS_MESSAGE           = "FQDN is reachable"
	FQDN_ERROR_MESSAGE             = "FQDN is not reachable"
	FQDN_RESOLUTION_MESSAGE        = "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
	NODE_TITLE                     = "Nodes are reachable"
	NODE_SUCCESS_MESSAGE           = "All nodes are reachable"
	NODE_ERROR_MESSAGE             = "%v is not reachable"
	NODE_RESOLUTION_MESSAGE        = "Ensure your Port 443 is open. Review security group or firewall settings."
	CERTIFICATE_TITLE              = "Certificate validity for FQDN"
	CERTIFICATE_SUCCESS_MESSAGE    = "FQDN has with valid certificates"
	CERTIFICATE_ERROR_MESSAGE      = "FQDN certificate is not valid."
	CERTIFICATE_RESOLUTION_MESSAGE = "Generate new valid certificates and provide those."
	IP_TO_HASH_FAIL_MESSAGE        = "Failed to hash the ips."
	DEFAULT_HTTPS_PORT             = "443"
)
