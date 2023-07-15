package constants

const (
	SKIP_CERT_TEST_MESSAGE                   = "Using default certificates"
	SKIP_NODE_CERT_TEST_MESSAGE              = "Using default certificates for nodes"
	SKIP_MANAGED_OS_TEST_MESSAGE             = "Using Chef Managed OpenSearch"
	SKIP_MANAGED_PG_TEST_MESSAGE             = "Using Chef Managed PostgreSQL"
	SKIP_BACKUP_TEST_MESSAGE                 = "Using Chef Managed PostgreSQL"
	SKIP_OS_BUCKET_TEST_MESSAGE              = "External OpenSearch configuration or Backup config not provided"
	SKIP_OS_BUCKET_TEST_SELF_MANAGED_MESSAGE = "External OpenSearch is self managed"
	SKIP_MISSING_HARDWARE_MESSAGE            = "Missing instance counts and instance IPs"
	SKIP_SSH_USER_TEST_MESSAGE               = "SSH credentials missing"
)
