package batchcheckservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
)

type MockBatchCheckService struct {
	BatchCheckFunc func([]string, *models.Config) (models.BatchCheckResponse, error)
}

func (mss *MockBatchCheckService) BatchCheck(checks []string, config *models.Config) (models.BatchCheckResponse, error) {
	return mss.BatchCheckFunc(checks, config)
}

type MockHardwareResourceCountCheck struct {
	HardwareResourceCountCheckFunc func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc      func() map[string]map[string][]int
}

func (mss *MockHardwareResourceCountCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.HardwareResourceCountCheckFunc(config)
}

func (mss *MockHardwareResourceCountCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func SetupMockHardwareResourceCountCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockHardwareResourceCountCheck{
		HardwareResourceCountCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			return resp
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockSshUserAccessCheck struct {
	SshUserAccessCheckFunc    func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockSshUserAccessCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.SshUserAccessCheckFunc(config)
}

func (mss *MockSshUserAccessCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func SetupMockSshUserAccessCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockSshUserAccessCheck{
		SshUserAccessCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			return resp
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockCertificateCheck struct {
	CertificateCheckFunc      func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockCertificateCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockCertificateCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.CertificateCheckFunc(config)
}

func SetupMockCertificateCheck() trigger.ICheck {
	return &MockCertificateCheck{
		CertificateCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "automate",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.CERTIFICATE,
						Message: "certificate-check",
						Checks: []models.Checks{
							{
								Title: "certificate-check-1",
							},
							{
								Title: "certificate-check-2",
							},
						},
					},
				},
				{
					Status:   "Passed",
					Host:     "1.2.3.4",
					NodeType: "chef-infra-server",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.CERTIFICATE,
						Message: "certificate-check",
						Checks: []models.Checks{
							{
								Title: "certificate-check-1",
							},
							{
								Title: "certificate-check-2",
							},
						},
					},
				},
			}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockExternalOpenSearchCheck struct {
	ExternalOpenSearchCheckFunc func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc   func() map[string]map[string][]int
}

func (mss *MockExternalOpenSearchCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockExternalOpenSearchCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.ExternalOpenSearchCheckFunc(config)
}

func SetupMockExternalOpenSearchCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockExternalOpenSearchCheck{
		ExternalOpenSearchCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			return resp
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockExternalPostgresCheck struct {
	ExternalPostgresCheckFunc func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockExternalPostgresCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockExternalPostgresCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.ExternalPostgresCheckFunc(config)
}

func SetupMockExternalPostgresCheck(resp []models.CheckTriggerResponse) trigger.ICheck {
	return &MockExternalPostgresCheck{
		ExternalPostgresCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			return resp
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockFirewallCheck struct {
	FirewallCheckFunc         func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockFirewallCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockFirewallCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.FirewallCheckFunc(config)
}

func SetupMockFirewallCheck() trigger.ICheck {
	return &MockFirewallCheck{
		FirewallCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			nodeTypePortMap := map[string]map[string][]int{
				constants.AUTOMATE: {
					constants.TCP:   []int{9631, 9638},
					constants.HTTPS: []int{443},
				},
				constants.CHEF_INFRA_SERVER: {
					constants.TCP:   []int{9631, 9638},
					constants.HTTPS: []int{443},
				},
				constants.POSTGRESQL: {
					constants.TCP: []int{7432, 9631, 5432, 6432, 9638},
					constants.UDP: []int{9638},
				},
				constants.OPENSEARCH: {
					constants.TCP: []int{9200, 9300, 9631, 9638},
				},
			}
			return nodeTypePortMap
		},
	}
}

type MockFqdnCheck struct {
	FqdnCheckFunc             func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockFqdnCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockFqdnCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.FqdnCheckFunc(config)
}

func SetupMockFqdnCheck() trigger.ICheck {
	return &MockFqdnCheck{
		FqdnCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			nodeTypePortMap := map[string]map[string][]int{
				constants.AUTOMATE: {
					constants.HTTPS: []int{443},
				},
				constants.CHEF_INFRA_SERVER: {
					constants.HTTPS: []int{443},
				},
			}
			return nodeTypePortMap
		},
	}
}

type MockNfsBackupConfigCheck struct {
	NfsBackupConfigCheckFunc  func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockNfsBackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.NfsBackupConfigCheckFunc(config)
}

func (mss *MockNfsBackupConfigCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func SetupMockNfsBackupConfigCheck() trigger.ICheck {
	return &MockNfsBackupConfigCheck{
		NfsBackupConfigCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockOpenSearchS3BucketAccessCheck struct {
	OpenSearchS3BucketAccessCheckFunc func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc         func() map[string]map[string][]int
}

func (mss *MockOpenSearchS3BucketAccessCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockOpenSearchS3BucketAccessCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.OpenSearchS3BucketAccessCheckFunc(config)
}

func SetupMockOpenSearchS3BucketAccessCheck() trigger.ICheck {
	return &MockOpenSearchS3BucketAccessCheck{
		OpenSearchS3BucketAccessCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockS3BackupConfigCheck struct {
	S3BackupConfigCheckFunc   func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockS3BackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.S3BackupConfigCheckFunc(config)
}

func (mss *MockS3BackupConfigCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func SetupMockS3BackupConfigCheck() trigger.ICheck {
	return &MockS3BackupConfigCheck{
		S3BackupConfigCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockSoftwareVersionCheck struct {
	SoftwareVersionCheckFunc  func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockSoftwareVersionCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockSoftwareVersionCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.SoftwareVersionCheckFunc(config)
}

func SetupMockSoftwareVersionCheck() trigger.ICheck {
	return &MockSoftwareVersionCheck{
		SoftwareVersionCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockSystemResourceCheck struct {
	SystemResourceCheckFunc   func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockSystemResourceCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func (mss *MockSystemResourceCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.SystemResourceCheckFunc(config)
}

func SetupMockSystemResourceCheck() trigger.ICheck {
	return &MockSystemResourceCheck{
		SystemResourceCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockSystemUserCheck struct {
	SystemUserCheckFunc       func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockSystemUserCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}
func (mss *MockSystemUserCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.SystemUserCheckFunc(config)
}

func SetupMockSystemUserCheck() trigger.ICheck {
	return &MockSystemUserCheck{
		SystemUserCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}

type MockGCPBackupConfigCheck struct {
	GCPBackupConfigCheckFunc  func(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServerFunc func() map[string]map[string][]int
}

func (mss *MockGCPBackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	return mss.GCPBackupConfigCheckFunc(config)
}

func (mss *MockGCPBackupConfigCheck) GetPortsForMockServer() map[string]map[string][]int {
	return mss.GetPortsForMockServerFunc()
}

func SetupMockGCPBackupConfigCheck() trigger.ICheck {
	return &MockGCPBackupConfigCheck{
		GCPBackupConfigCheckFunc: func(config *models.Config) []models.CheckTriggerResponse {
			m := []models.CheckTriggerResponse{}
			return m
		},
		GetPortsForMockServerFunc: func() map[string]map[string][]int {
			m := make(map[string]map[string][]int)
			return m
		},
	}
}
