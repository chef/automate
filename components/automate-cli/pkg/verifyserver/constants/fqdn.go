package constants

const (
	FQDN_TITLE                           = "FQDN is reachable"
	FQDN_ERROR_MESSAGE                   = "FQDN is not reachable"
	FQDN_RESOLUTION_MESSAGE              = "Ensure FQDN is reachable and mapped to load balancer. Also, ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
	CERT_CN_MISMATCH_RESOLUTION_MESSAGE  = "Ensure the certificate provided is Valid. In case of self-signed certificate, make sure the DNS provided in \"subjectAltName\" matches with \"CN\" (Common Name)"
	CERT_CN_MISMATCH_ERROR_PATTERN       = "x509: certificate is not valid for any names, but wanted to match"
	INVALID_FQDN_CERT_RESOLUTION_MESSAGE = "Ensure the certificate provided is Valid. In case of self-signed certificate, make sure the root-ca and the certificate provided in LB belongs to same CA"
	INVALID_FQDN_CERT_ERROR_PATTERN      = "x509: certificate signed by unknown authority"
	GENERIC_FQDN_CERT_RESOLUTION_MESSAGE = "Ensure the certificate provided is Valid. Also check if the FQDN is reachable, and mapped to load balancer. Review security group or firewall settings for the load-balance."
	NODE_TITLE                           = "Nodes are reachable"
	NODE_SUCCESS_MESSAGE                 = "All nodes are reachable"
	NODE_ERROR_MESSAGE                   = "%v is not reachable"
	NODE_RESOLUTION_MESSAGE              = "Ensure your Port 443 is open. Review security group or firewall settings of the node."
	CERTIFICATE_TITLE                    = "Certificate validity for FQDN"
	CERTIFICATE_SUCCESS_MESSAGE          = "FQDN has with valid certificates"
	CERTIFICATE_ERROR_MESSAGE            = "FQDN certificate is not valid."
	CERTIFICATE_RESOLUTION_MESSAGE       = "Generate new valid certificates and provide those."
	IP_TO_HASH_FAIL_MESSAGE              = "Failed to hash the IP."
	DEFAULT_HTTPS_PORT                   = "443"
	SERVER_IP_HEADER_KEY                 = "x-server-ip"
	CHAN_RESULT_ERROR_MESSAGE            = "error recieved"
	MIN_NUMBER_OF_CALLS                  = 50
)
