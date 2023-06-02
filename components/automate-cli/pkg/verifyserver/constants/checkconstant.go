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
	FQDN_MSG                            = "FQDN Check"
	FIREWALL_MSG                        = "Firewall Check"
	EXTERNAL_OPENSEARCH_MSG             = "External Opensearch Check"
	AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG = "AWS Opensearch S3 Bucket Access Check"
	EXTERNAL_POSTGRESQL_MSG             = "External Postgresql Check"
	NFS_BACKUP_CONFIG_MSG               = "NFS Backup Config Check"
)

const NFS_MOUNT_MSG = "NFS Mount Backup Check"

const (
	BASTION_CHECK = "bastion"
	REMOTE_CHECK  = "remote"
)

var checkAPIMap map[string]models.CheckAndType = map[string]models.CheckAndType{
	HARDWARE_RESOURCE_COUNT: {
		CheckName: BASTION_CHECK,
		CheckType: HARDWARE_RESOURCE_COUNT,
		CheckMsg:  HARDWARE_RESOURCE_COUNT_MSG,
	},
	CERTIFICATE: {
		CheckName: BASTION_CHECK,
		CheckType: CERTIFICATE,
		CheckMsg:  CERTIFICATE_MSG,
	},
	SSH_USER: {
		CheckName: BASTION_CHECK,
		CheckType: SSH_USER,
		CheckMsg:  SSH_USER_MSG,
	},
	SYSTEM_RESOURCES: {
		CheckName: REMOTE_CHECK,
		CheckType: SYSTEM_RESOURCES,
		CheckMsg:  SYSTEM_RESOURCES_MSG,
	},
	SOFTWARE_VERSIONS: {
		CheckName: REMOTE_CHECK,
		CheckType: SOFTWARE_VERSIONS,
		CheckMsg:  SOFTWARE_VERSIONS_MSG,
	},
	SYSTEM_USER: {
		CheckName: REMOTE_CHECK,
		CheckType: SYSTEM_USER,
		CheckMsg:  SYSTEM_USER_MSG,
	},
	S3_BACKUP_CONFIG: {
		CheckName: REMOTE_CHECK,
		CheckType: S3_BACKUP_CONFIG,
		CheckMsg:  S3_BACKUP_CONFIG_MSG,
	},
	FQDN: {
		CheckName: REMOTE_CHECK,
		CheckType: FQDN,
		CheckMsg:  FQDN_MSG,
	},
	FIREWALL: {
		CheckName: REMOTE_CHECK,
		CheckType: FIREWALL,
		CheckMsg:  FIREWALL_MSG,
	},
	EXTERNAL_OPENSEARCH: {
		CheckName: REMOTE_CHECK,
		CheckType: EXTERNAL_OPENSEARCH,
		CheckMsg:  EXTERNAL_OPENSEARCH_MSG,
	},
	AWS_OPENSEARCH_S3_BUCKET_ACCESS: {
		CheckName: REMOTE_CHECK,
		CheckType: AWS_OPENSEARCH_S3_BUCKET_ACCESS,
		CheckMsg:  AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG,
	},
	EXTERNAL_POSTGRESQL: {
		CheckName: REMOTE_CHECK,
		CheckType: EXTERNAL_POSTGRESQL,
		CheckMsg:  EXTERNAL_POSTGRESQL_MSG,
	},
	NFS_BACKUP_CONFIG: {
		CheckName: REMOTE_CHECK,
		CheckType: NFS_BACKUP_CONFIG,
		CheckMsg:  NFS_BACKUP_CONFIG_MSG,
	},
}

func GetCheckMessageByType(checkType string) string {
	val, ok := checkAPIMap[checkType]
	if !ok {
		return ""
	}
	return val.CheckMsg
}

func GetBastionChecks() []string {
	var checks []string
	for k, v := range checkAPIMap {
		if v.CheckName == BASTION_CHECK {
			checks = append(checks, k)
		}
	}
	return checks
}

func GetRemoteChecks() []string {
	var checks []string
	for k, v := range checkAPIMap {
		if v.CheckName == REMOTE_CHECK {
			checks = append(checks, k)
		}
	}
	return checks
}
