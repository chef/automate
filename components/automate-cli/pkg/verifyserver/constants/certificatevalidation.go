package constants

const (
	CERTIFICATE_EXPIRY_TITLE                 = "Certificates have valid expiry date"
	CERTIFICATE_EXPIRY_SUCCESS_MESSAGE       = "All certificates expiry date is later than 365 days"
	CERTIFICATE_EXPIRY_ERROR_MESSAGE         = "The %v certificates have expired"
	CERTIFICATE_INVALID_EXPIRY_MESSAGE       = "The %v certificates expiry date is earlier than 365 days"
	CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE    = "Generate and provide certificates with expiry date later than 365 days"
	CERTIFICATE_FORMAT_TITLE                 = "Certificates are of X509 V3 format"
	CERTIFICATE_FORMAT_SUCCESS_MESSAGE       = "All certificates are of X509 V3 format"
	CERTIFICATE_FORMAT_ERROR_MESSAGE         = "The %v certificates are not of X509 V3 format"
	CERTIFICATE_FORMAT_RESOLUTION_MESSAGE    = "Generate and provide certificates of X509 v3 format"
	CERTIFICATE_ALGORITHM_TITLE              = "Certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms"
	CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE    = "All certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms"
	CERTIFICATE_ALGORITHM_ERROR_MESSAGE      = "The %v certificate(s) are hashed using neither of the supported PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms"
	CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE = "Generate and provide %v certificate(s) hashed using either of the the supported PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms"
	KEY_FORMAT_TITLE                         = "Private Keys are of PKCS8 format"
	KEY_FORMAT_SUCCESS_MESSAGE               = "The private keys are of PKCS8 format"
	KEY_FORMAT_ERROR_MESSAGE                 = "The %v private key(s) are not of PKCS8 format"
	KEY_FORMAT_RESOLUTION_MESSAGE            = "Generate and provide %v private key of PKCS8 format"
	CERTIFICATE_BLOCK_TYPE                   = "CERTIFICATE"
	X509_VERSION                             = 3
	ROOT                                     = "Root"
	NODE                                     = "Node"
	ADMIN                                    = "Admin"
	NODE_KEY                                 = "Node-Key"
	ADMIN_KEY                                = "Admin-Key"
)
