package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockCheckTrigger struct {
	HardwareResourceCountCheckFunc    func(config models.Config) models.CheckTriggerResponse
	SshUserAccessCheckFunc            func(config models.Config) models.CheckTriggerResponse
	CertificateCheckFunc              func(config models.Config) models.CheckTriggerResponse
	SystemResourceCheckFunc           func(config models.Config) models.CheckTriggerResponse
	ExternalOpensearchCheckFunc       func(config models.Config) models.CheckTriggerResponse
	ExternalPostgresCheckFunc         func(config models.Config) models.CheckTriggerResponse
	FirewallCheckFunc                 func(config models.Config) models.CheckTriggerResponse
	FqdnCheckFunc                     func(config models.Config) models.CheckTriggerResponse
	NfsBackupConfigCheckFunc          func(config models.Config) models.CheckTriggerResponse
	S3BackupConfigCheckFunc           func(config models.Config) models.CheckTriggerResponse
	SoftwareVersionCheckFunc          func(config models.Config) models.CheckTriggerResponse
	SystemUserCheckFunc               func(config models.Config) models.CheckTriggerResponse
	OpensearchS3BucketAccessCheckFunc func(config models.Config) models.CheckTriggerResponse
}

func (mss *MockCheckTrigger) HardwareResourceCountCheck(config models.Config) models.CheckTriggerResponse {
	return mss.HardwareResourceCountCheckFunc(config)
}

func (mss *MockCheckTrigger) SshUserAccessCheck(config models.Config) models.CheckTriggerResponse {
	return mss.SshUserAccessCheckFunc(config)
}

func (mss *MockCheckTrigger) CertificateCheck(config models.Config) models.CheckTriggerResponse {
	return mss.CertificateCheckFunc(config)
}

func (mss *MockCheckTrigger) SystemResourceCheck(config models.Config) models.CheckTriggerResponse {
	return mss.SystemResourceCheckFunc(config)
}

func (mss *MockCheckTrigger) ExternalOpensearchCheck(config models.Config) models.CheckTriggerResponse {
	return mss.ExternalOpensearchCheckFunc(config)
}

func (mss *MockCheckTrigger) ExternalPostgresCheck(config models.Config) models.CheckTriggerResponse {
	return mss.ExternalPostgresCheckFunc(config)
}

func (mss *MockCheckTrigger) FirewallCheck(config models.Config) models.CheckTriggerResponse {
	return mss.FirewallCheckFunc(config)
}

func (mss *MockCheckTrigger) FqdnCheck(config models.Config) models.CheckTriggerResponse {
	return mss.FqdnCheckFunc(config)
}

func (mss *MockCheckTrigger) NfsBackupConfigCheck(config models.Config) models.CheckTriggerResponse {
	return mss.NfsBackupConfigCheckFunc(config)
}

func (mss *MockCheckTrigger) S3BackupConfigCheck(config models.Config) models.CheckTriggerResponse {
	return mss.S3BackupConfigCheckFunc(config)
}

func (mss *MockCheckTrigger) SoftwareVersionCheck(config models.Config) models.CheckTriggerResponse {
	return mss.SoftwareVersionCheckFunc(config)
}

func (mss *MockCheckTrigger) SystemUserCheck(config models.Config) models.CheckTriggerResponse {
	return mss.SystemUserCheckFunc(config)
}

func (mss *MockCheckTrigger) OpensearchS3BucketAccessCheck(config models.Config) models.CheckTriggerResponse {
	return mss.OpensearchS3BucketAccessCheckFunc(config)
}
