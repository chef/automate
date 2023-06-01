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
	NFS_BACKUP_CONFIG_MSG               = "Nfs Backup Config Check"
)

const NFS_MOUNT_MSG = "NFS Mount Backup Check"

const (
	BastionCheckType = "bastion"
	RemoteCheckType  = "remote"
)

var abcMap map[string]models.CheckAndType = map[string]models.CheckAndType{
	HARDWARE_RESOURCE_COUNT: {
		CheckType: BastionCheckType,
		CheckName: HARDWARE_RESOURCE_COUNT,
		CheckMsg:  HARDWARE_RESOURCE_COUNT_MSG,
	},
	CERTIFICATE: {
		CheckType: BastionCheckType,
		CheckName: CERTIFICATE,
		CheckMsg:  CERTIFICATE_MSG,
	},
	SSH_USER: {
		CheckType: BastionCheckType,
		CheckName: SSH_USER,
		CheckMsg:  SSH_USER_MSG,
	},
	SYSTEM_RESOURCES: {
		CheckType: RemoteCheckType,
		CheckName: SYSTEM_RESOURCES,
		CheckMsg:  SYSTEM_RESOURCES_MSG,
	},
	SOFTWARE_VERSIONS: {
		CheckType: RemoteCheckType,
		CheckName: SOFTWARE_VERSIONS,
		CheckMsg:  SOFTWARE_VERSIONS_MSG,
	},
	SYSTEM_USER: {
		CheckType: RemoteCheckType,
		CheckName: SYSTEM_USER,
		CheckMsg:  SYSTEM_USER_MSG,
	},
	S3_BACKUP_CONFIG: {
		CheckType: RemoteCheckType,
		CheckName: S3_BACKUP_CONFIG,
		CheckMsg:  S3_BACKUP_CONFIG_MSG,
	},
	FQDN: {
		CheckType: RemoteCheckType,
		CheckName: FQDN,
		CheckMsg:  FQDN_MSG,
	},
	FIREWALL: {
		CheckType: RemoteCheckType,
		CheckName: FIREWALL,
		CheckMsg:  FIREWALL_MSG,
	},
	EXTERNAL_OPENSEARCH: {
		CheckType: RemoteCheckType,
		CheckName: EXTERNAL_OPENSEARCH,
		CheckMsg:  EXTERNAL_OPENSEARCH_MSG,
	},
	AWS_OPENSEARCH_S3_BUCKET_ACCESS: {
		CheckType: RemoteCheckType,
		CheckName: AWS_OPENSEARCH_S3_BUCKET_ACCESS,
		CheckMsg:  AWS_OPENSEARCH_S3_BUCKET_ACCESS_MSG,
	},
	EXTERNAL_POSTGRESQL: {
		CheckType: RemoteCheckType,
		CheckName: EXTERNAL_POSTGRESQL,
		CheckMsg:  EXTERNAL_POSTGRESQL_MSG,
	},
	NFS_BACKUP_CONFIG: {
		CheckType: RemoteCheckType,
		CheckName: NFS_BACKUP_CONFIG,
		CheckMsg:  NFS_BACKUP_CONFIG_MSG,
	},
}

func GetCheckByType(checkName string) string {
	val, ok := abcMap[checkName]
	if !ok {
		return ""
	}
	return val.CheckMsg
}

func GetBastionChecks() []string {
	var checks []string
	for _, v := range abcMap {
		if v.CheckType == BastionCheckType {
			checks = append(checks, v.CheckName)
		}
	}
	return checks
}

func GetRemoteChecks() []string {
	var checks []string
	for _, v := range abcMap {
		if v.CheckType == RemoteCheckType {
			checks = append(checks, v.CheckName)
		}
	}
	return checks
}
