package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ICheckTrigger interface {
	//AddHardwareResourceCountCheck(ICheck) *ICheckTrigger
	// SshUserAccessCheck(config models.Config) map[string]models.CheckTriggerResponse
	// CertificateCheck(config models.Config) map[string]models.CheckTriggerResponse
	// SystemResourceCheck(config models.Config) map[string]models.CheckTriggerResponse
	// ExternalOpensearchCheck(config models.Config) map[string]models.CheckTriggerResponse
	// ExternalPostgresCheck(config models.Config) map[string]models.CheckTriggerResponse
	// FirewallCheck(config models.Config) map[string]models.CheckTriggerResponse
	// FqdnCheck(config models.Config) map[string]models.CheckTriggerResponse
	// NfsBackupConfigCheck(config models.Config) map[string]models.CheckTriggerResponse
	// S3BackupConfigCheck(config models.Config) map[string]models.CheckTriggerResponse
	// SoftwareVersionCheck(config models.Config) map[string]models.CheckTriggerResponse
	// SystemUserCheck(config models.Config) map[string]models.CheckTriggerResponse
	// OpensearchS3BucketAccessCheck(config models.Config) map[string]models.CheckTriggerResponse
}

type CheckTrigger struct {
	HardwareResourceCountCheck ICheck
}

// func (h *CheckTrigger) AddHardwareResourceCountCheck(ss ICheck) *CheckTrigger {
// 	h.HardwareResourceCountCheck = ss
// 	return h
// }

type ICheck interface {
	Run(config models.Config) map[string]models.CheckTriggerResponse
}

func NewCheckTrigger(hrc ICheck) *CheckTrigger {
	return &CheckTrigger{
		HardwareResourceCountCheck: hrc,
	}
}
