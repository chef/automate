package constants

const (
	SKIP_CERT_TEST_MESSAGE                   = "- Using Default certificates\n- No Custom certificates provided"
	SKIP_NODE_CERT_TEST_MESSAGE              = "- Using Default certificates\n- No Custom certificates provided"
	SKIP_MANAGED_OS_TEST_MESSAGE             = "- Using Chef Managed OpenSearch\n- No External OpenSearch configuration found"
	SKIP_MANAGED_PG_TEST_MESSAGE             = "- Using Chef Managed PostgreSQL\n- No External OpenSearch configuration found"
	SKIP_BACKUP_TEST_MESSAGE                 = "- Backup settings wont be set\n- No Backup configuration was found"
	SKIP_OS_BUCKET_TEST_MESSAGE              = "- No External OpenSearch configuration provided\n- No S3 Backup configuration was provided"
	SKIP_OS_BUCKET_TEST_SELF_MANAGED_MESSAGE = "- External OpenSearch is self managed"
	SKIP_MISSING_HARDWARE_MESSAGE            = "- Instance counts and Instance IPs are missing"
	SKIP_SSH_USER_TEST_MESSAGE               = "- SSH credentials are missing"
)
