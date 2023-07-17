package constants

const (
	SKIP_CERT_TEST_MESSAGE                   = "- No Custom certificates provided\n- Using Default certificates"
	SKIP_MANAGED_OS_TEST_MESSAGE             = "- No External OpenSearch configuration found\n- Using Chef Managed OpenSearch"
	SKIP_MANAGED_PG_TEST_MESSAGE             = "- No External OpenSearch configuration found\n- Using Chef Managed PostgreSQL"
	SKIP_BACKUP_TEST_MESSAGE                 = "- No Backup configuration was found\n- Backup settings wont be set"
	SKIP_OS_BUCKET_TEST_MESSAGE              = "- No External OpenSearch configuration provided\n- No S3 Backup configuration was provided"
	SKIP_OS_BUCKET_TEST_SELF_MANAGED_MESSAGE = "- External OpenSearch is self managed"
	SKIP_MISSING_HARDWARE_MESSAGE            = "- Instance count and Instance IPs are missing"
	SKIP_SSH_USER_TEST_MESSAGE               = "- SSH credentials are missing"
)
