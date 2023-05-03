package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ICheckTrigger interface {
	HardwareResourceCountCheck(config models.Config) models.CheckTriggerResponse
	SshUserAccessCheck(config models.Config) models.CheckTriggerResponse
	CertificateCheck(config models.Config) models.CheckTriggerResponse
	SystemResourceCheck(config models.Config) models.CheckTriggerResponse
	ExternalOpensearchCheck(config models.Config) models.CheckTriggerResponse
	ExternalPostgresCheck(config models.Config) models.CheckTriggerResponse
	FirewallCheck(config models.Config) models.CheckTriggerResponse
	FqdnCheck(config models.Config) models.CheckTriggerResponse
	NfsBackupConfigCheck(config models.Config) models.CheckTriggerResponse
	S3BackupConfigCheck(config models.Config) models.CheckTriggerResponse
	SoftwareVersionCheck(config models.Config) models.CheckTriggerResponse
	SystemUserCheck(config models.Config) models.CheckTriggerResponse
	OpensearchS3BucketAccessCheck(config models.Config) models.CheckTriggerResponse
}

type CheckTrigger struct {
}

func NewCheckTrigger() ICheckTrigger {
	return &CheckTrigger{}
}
