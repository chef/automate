package batchcheckservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
)

type MockBatchCheckService struct {
	BatchCheckFunc func([]string, models.Config) models.BatchCheckResponse
}

func (mss *MockBatchCheckService) BatchCheck(checks []string, config models.Config) models.BatchCheckResponse {
	return mss.BatchCheckFunc(checks, config)
}

type MockHardwareResourceCountCheck struct {
	HardwareResourceCountCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockHardwareResourceCountCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.HardwareResourceCountCheckFunc(config)
}

func SetupMockHardwareResourceCountCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockHardwareResourceCountCheck{
		HardwareResourceCountCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			return resp
		},
	}
}

type MockSshUserAccessCheck struct {
	SshUserAccessCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockSshUserAccessCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.SshUserAccessCheckFunc(config)
}

func SetupMockSshUserAccessCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockSshUserAccessCheck{
		SshUserAccessCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			return resp
		},
	}
}

type MockCertificateCheck struct {
	CertificateCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockCertificateCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.CertificateCheckFunc(config)
}

func SetupMockCertificateCheck() trigger.ICheck {
	return &MockCertificateCheck{
		CertificateCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockExternalOpenSearchCheck struct {
	ExternalOpenSearchCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockExternalOpenSearchCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.ExternalOpenSearchCheckFunc(config)
}

func SetupMockExternalOpenSearchCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockExternalOpenSearchCheck{
		ExternalOpenSearchCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			return resp
		},
	}
}

type MockExternalPostgresCheck struct {
	ExternalPostgresCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockExternalPostgresCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.ExternalPostgresCheckFunc(config)
}

func SetupMockExternalPostgresCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockExternalPostgresCheck{
		ExternalPostgresCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			return resp
		},
	}
}

type MockFirewallCheck struct {
	FirewallCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockFirewallCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.FirewallCheckFunc(config)
}

func SetupMockFirewallCheck() trigger.ICheck {
	return &MockFirewallCheck{
		FirewallCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockFqdnCheck struct {
	FqdnCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockFqdnCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.FqdnCheckFunc(config)
}

func SetupMockFqdnCheck() trigger.ICheck {
	return &MockFqdnCheck{
		FqdnCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockNfsBackupConfigCheck struct {
	NfsBackupConfigCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockNfsBackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.NfsBackupConfigCheckFunc(config)
}

func SetupMockNfsBackupConfigCheck() trigger.ICheck {
	return &MockNfsBackupConfigCheck{
		NfsBackupConfigCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockOpenSearchS3BucketAccessCheck struct {
	OpenSearchS3BucketAccessCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockOpenSearchS3BucketAccessCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.OpenSearchS3BucketAccessCheckFunc(config)
}

func SetupMockOpenSearchS3BucketAccessCheck() trigger.ICheck {
	return &MockOpenSearchS3BucketAccessCheck{
		OpenSearchS3BucketAccessCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockS3BackupConfigCheck struct {
	S3BackupConfigCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockS3BackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.S3BackupConfigCheckFunc(config)
}

func SetupMockS3BackupConfigCheck() trigger.ICheck {
	return &MockS3BackupConfigCheck{
		S3BackupConfigCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSoftwareVersionCheck struct {
	SoftwareVersionCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockSoftwareVersionCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.SoftwareVersionCheckFunc(config)
}

func SetupMockSoftwareVersionCheck() trigger.ICheck {
	return &MockSoftwareVersionCheck{
		SoftwareVersionCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSystemResourceCheck struct {
	SystemResourceCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockSystemResourceCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.SystemResourceCheckFunc(config)
}

func SetupMockSystemResourceCheck() trigger.ICheck {
	return &MockSystemResourceCheck{
		SystemResourceCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSystemUserCheck struct {
	SystemUserCheckFunc func(config models.Config) []models.CheckTriggerResponse
}

func (mss *MockSystemUserCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return mss.SystemUserCheckFunc(config)
}

func SetupMockSystemUserCheck() trigger.ICheck {
	return &MockSystemUserCheck{
		SystemUserCheckFunc: func(config models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
	}
}
