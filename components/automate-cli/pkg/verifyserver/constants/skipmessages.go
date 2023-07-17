package constants

const (
	SKIP_CERT_TEST_MESSAGE                   = "Using Default certificates since no Custom certificates have been provided"
	SKIP_NODE_CERT_TEST_MESSAGE              = "Using Default certificates for this node since no Custom certificates have been provided"
	SKIP_MANAGED_OS_TEST_MESSAGE             = "Using Chef Managed OpenSearch since no External OpenSearch configuration found"
	SKIP_MANAGED_PG_TEST_MESSAGE             = "Using Chef Managed PostgreSQL since no External OpenSearch configuration found"
	SKIP_BACKUP_TEST_MESSAGE                 = "Backup settings wont be set since no Backup configuration was found"
	SKIP_OS_BUCKET_TEST_MESSAGE              = "Check skipped since no External OpenSearch configuration or no S3 Backup configuration was provided"
	SKIP_OS_BUCKET_TEST_SELF_MANAGED_MESSAGE = "Check skipped since External OpenSearch is self managed"
	SKIP_MISSING_HARDWARE_MESSAGE            = "Check skipped since Instance counts and Instance IPs are missing"
	SKIP_SSH_USER_TEST_MESSAGE               = "Check skipped since SSH credentials are missing"
)
