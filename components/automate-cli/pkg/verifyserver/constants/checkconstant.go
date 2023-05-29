package constants

const HARDWARE_RESOURCE_COUNT = "hardware-resource-count"
const CERTIFICATE = "certificate"
const SSH_USER = "ssh-user"
const SYSTEM_RESOURCES = "system-resources"
const SOFTWARE_VERSIONS = "software-versions"
const SYSTEM_USER = "system-user"
const S3_BACKUP_CONFIG = "s3-backup-config"
const FQDN = "fqdn"
const FIREWALL = "firewall"
const EXTERNAL_OPENSEARCH = "external-opensearch"
const AWS_OPENSEARCH_S3_BUCKET_ACCESS = "aws-opensearch-s3-bucket-access"
const EXTERNAL_POSTGRESQL = "external-postgresql"
const NFS_BACKUP_CONFIG = "nfs-backup-config"

const HARDWARE_RESOURCE_COUNT_MSG = "Hardware Resource Count Check"
const SSH_USER_MSG = "SSH User Access Check"
const CERTIFICATE_MSG = "Certificate check"

func GetBastionChecks() []string {
	var checks = []string{
		HARDWARE_RESOURCE_COUNT,
		CERTIFICATE,
		SSH_USER,
	}
	return checks
}

func GetRemoteChecks() []string {
	var checks = []string{
		SYSTEM_RESOURCES,
		SOFTWARE_VERSIONS,
		SYSTEM_USER,
		S3_BACKUP_CONFIG,
		FQDN,
		FIREWALL,
		EXTERNAL_OPENSEARCH,
		AWS_OPENSEARCH_S3_BUCKET_ACCESS,
		EXTERNAL_POSTGRESQL,
		NFS_BACKUP_CONFIG,
	}
	return checks
}
