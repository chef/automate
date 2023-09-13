package constants

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

const HARDWARE_RESOURCE_COUNT = "hardware-resource-count"
const CERTIFICATE = "certificate"
const SSH_USER = "ssh-user"
const SYSTEM_RESOURCES = "system-resources"
const SOFTWARE_VERSIONS = "software-versions"
const SYSTEM_USER = "system-user"
const S3_BACKUP_CONFIG = "s3-backup-config"
const GCP_BACKUP_CONFIG = "gcp-backup-config"
const FQDN = "fqdn"
const FIREWALL = "firewall"
const EXTERNAL_OPENSEARCH = "external-opensearch"
const AWS_OPENSEARCH_S3_BUCKET_ACCESS = "aws-opensearch-s3-bucket-access"
const EXTERNAL_POSTGRESQL = "external-postgresql"
const NFS_BACKUP_CONFIG = "nfs-backup-config"

const (
	HARDWARE_RESOURCE_COUNT_MSG         = "Hardware Resource Count Check"
	CERTIFICATE_MSG                     = "Certificate Check"
	SSH_USER_MSG                        = "SSH User Access Check"
	SYSTEM_RESOURCES_MSG                = "System Resources Check"
	SOFTWARE_VERSIONS_MSG               = "Software Versions Check"
	SYSTEM_USER_MSG                     = "System User Check"
	S3_BACKUP_CONFIG_MSG                = "S3 Backup Config Check"
	GCP_BACKUP_CONFIG_MSG               = "GCP Backup Config Check"
	FQDN_MSG                            = "FQDN Check"
	FIREWALL_MSG                        = "Firewall Check"
	EXTERNAL_OPENSEARCH_MSG             = "External OpenSearch Check"
	AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG = "AWS Opensearch S3 Bucket Access Check"
	EXTERNAL_POSTGRESQL_MSG             = "External PostgreSQL Check"
	NFS_BACKUP_CONFIG_MSG               = "NFS Backup Config Check"
)

const NFS_MOUNT_MSG = "NFS Mount Backup Check"

const (
	BASTION_CHECK = "bastion"
	REMOTE_CHECK  = "remote"
)

var checkMap map[string]models.CheckAndType = map[string]models.CheckAndType{
	HARDWARE_RESOURCE_COUNT: {
		CheckType: BASTION_CHECK,
		CheckName: HARDWARE_RESOURCE_COUNT,
		CheckMsg:  HARDWARE_RESOURCE_COUNT_MSG,
	},
	CERTIFICATE: {
		CheckType: BASTION_CHECK,
		CheckName: CERTIFICATE,
		CheckMsg:  CERTIFICATE_MSG,
	},
	SSH_USER: {
		CheckType: BASTION_CHECK,
		CheckName: SSH_USER,
		CheckMsg:  SSH_USER_MSG,
	},
	SYSTEM_RESOURCES: {
		CheckType: REMOTE_CHECK,
		CheckName: SYSTEM_RESOURCES,
		CheckMsg:  SYSTEM_RESOURCES_MSG,
	},
	SOFTWARE_VERSIONS: {
		CheckType: REMOTE_CHECK,
		CheckName: SOFTWARE_VERSIONS,
		CheckMsg:  SOFTWARE_VERSIONS_MSG,
	},
	SYSTEM_USER: {
		CheckType: REMOTE_CHECK,
		CheckName: SYSTEM_USER,
		CheckMsg:  SYSTEM_USER_MSG,
	},
	S3_BACKUP_CONFIG: {
		CheckType: REMOTE_CHECK,
		CheckName: S3_BACKUP_CONFIG,
		CheckMsg:  S3_BACKUP_CONFIG_MSG,
	},
	FQDN: {
		CheckType: REMOTE_CHECK,
		CheckName: FQDN,
		CheckMsg:  FQDN_MSG,
	},
	FIREWALL: {
		CheckType: REMOTE_CHECK,
		CheckName: FIREWALL,
		CheckMsg:  FIREWALL_MSG,
	},
	EXTERNAL_OPENSEARCH: {
		CheckType: REMOTE_CHECK,
		CheckName: EXTERNAL_OPENSEARCH,
		CheckMsg:  EXTERNAL_OPENSEARCH_MSG,
	},
	AWS_OPENSEARCH_S3_BUCKET_ACCESS: {
		CheckType: REMOTE_CHECK,
		CheckName: AWS_OPENSEARCH_S3_BUCKET_ACCESS,
		CheckMsg:  AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG,
	},
	EXTERNAL_POSTGRESQL: {
		CheckType: REMOTE_CHECK,
		CheckName: EXTERNAL_POSTGRESQL,
		CheckMsg:  EXTERNAL_POSTGRESQL_MSG,
	},
	NFS_BACKUP_CONFIG: {
		CheckType: REMOTE_CHECK,
		CheckName: NFS_BACKUP_CONFIG,
		CheckMsg:  NFS_BACKUP_CONFIG_MSG,
	},
	GCP_BACKUP_CONFIG: {
		CheckType: REMOTE_CHECK,
		CheckName: GCP_BACKUP_CONFIG,
		CheckMsg:  GCP_BACKUP_CONFIG_MSG,
	},
}

func GetCheckMessageByName(CheckName string) string {
	val, ok := checkMap[CheckName]
	if !ok {
		return ""
	}
	return val.CheckMsg
}

func GetBastionChecks() []string {
	var checks []string
	for k, v := range checkMap {
		if v.CheckType == BASTION_CHECK {
			checks = append(checks, k)
		}
	}
	return checks
}

func GetRemoteChecks() []string {
	var checks []string
	for k, v := range checkMap {
		if v.CheckType == REMOTE_CHECK {
			checks = append(checks, k)
		}
	}
	return checks
}
