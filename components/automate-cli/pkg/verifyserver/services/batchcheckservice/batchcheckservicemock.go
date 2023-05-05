package batchcheckservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
)

type MockHardwareResourceCountCheck struct {
	HardwareResourceCountCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockHardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.HardwareResourceCountCheckFunc(config)
}

func SetupMockHardwareResourceCountCheck() trigger.ICheck {
	return &MockHardwareResourceCountCheck{
		HardwareResourceCountCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  false,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "hardware-resource-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "hardware-resource-check-2",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  true,
						Check:   "hardware-resource-count",
						Message: "Hardware Resource Count Check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "hardware-resource-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "hardware-resource-check-2",
								},
							},
						},
					},
				},
			}
			return m
		},
	}
}

type MockSshUserAccessCheck struct {
	SshUserAccessCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockSshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.SshUserAccessCheckFunc(config)
}

func SetupMockSshUserAccessCheck() trigger.ICheck {
	return &MockSshUserAccessCheck{
		SshUserAccessCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "ssh-user-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "ssh-user-check-2",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.SSH_USER,
						Message: "ssh-user-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "ssh-user-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "ssh-user-check-2",
								},
							},
						},
					},
				},
			}
			return m
		},
	}
}

type MockCertificateCheck struct {
	CertificateCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockCertificateCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.CertificateCheckFunc(config)
}

func SetupMockCertificateCheck() trigger.ICheck {
	return &MockCertificateCheck{
		CertificateCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockExternalOpensearchCheck struct {
	ExternalOpensearchCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockExternalOpensearchCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.ExternalOpensearchCheckFunc(config)
}

func SetupMockExternalOpensearchCheck() trigger.ICheck {
	return &MockExternalOpensearchCheck{
		ExternalOpensearchCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockExternalPostgresCheck struct {
	ExternalPostgresCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockExternalPostgresCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.ExternalPostgresCheckFunc(config)
}

func SetupMockExternalPostgresCheck() trigger.ICheck {
	return &MockExternalPostgresCheck{
		ExternalPostgresCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockFirewallCheck struct {
	FirewallCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockFirewallCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.FirewallCheckFunc(config)
}

func SetupMockFirewallCheck() trigger.ICheck {
	return &MockFirewallCheck{
		FirewallCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.FIREWALL,
						Message: "Firewall-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Firewall-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Firewall-check-2",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.FIREWALL,
						Message: "Firewall-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Firewall-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Firewall-check-2",
								},
							},
						},
					},
				},
			}
			return m
		},
	}
}

type MockFqdnCheck struct {
	FqdnCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockFqdnCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.FqdnCheckFunc(config)
}

func SetupMockFqdnCheck() trigger.ICheck {
	return &MockFqdnCheck{
		FqdnCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  false,
						Check:   constants.FQDN,
						Message: "fqdn-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Fqdn-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Fqdn-check-2",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed:  true,
						Check:   constants.FQDN,
						Message: "fqdn-check",
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Fqdn-check-1",
								},
							},
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "Fqdn-check-2",
								},
							},
						},
					},
				},
			}
			return m
		},
	}
}

type MockNfsBackupConfigCheck struct {
	NfsBackupConfigCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockNfsBackupConfigCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.NfsBackupConfigCheckFunc(config)
}

func SetupMockNfsBackupConfigCheck() trigger.ICheck {
	return &MockNfsBackupConfigCheck{
		NfsBackupConfigCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockOpensearchS3BucketAccessCheck struct {
	OpensearchS3BucketAccessCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockOpensearchS3BucketAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.OpensearchS3BucketAccessCheckFunc(config)
}

func SetupMockOpensearchS3BucketAccessCheck() trigger.ICheck {
	return &MockOpensearchS3BucketAccessCheck{
		OpensearchS3BucketAccessCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockS3BackupConfigCheck struct {
	S3BackupConfigCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockS3BackupConfigCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.S3BackupConfigCheckFunc(config)
}

func SetupMockS3BackupConfigCheck() trigger.ICheck {
	return &MockS3BackupConfigCheck{
		S3BackupConfigCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSoftwareVersionCheck struct {
	SoftwareVersionCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockSoftwareVersionCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.SoftwareVersionCheckFunc(config)
}

func SetupMockSoftwareVersionCheck() trigger.ICheck {
	return &MockSoftwareVersionCheck{
		SoftwareVersionCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSystemResourceCheck struct {
	SystemResourceCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockSystemResourceCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.SystemResourceCheckFunc(config)
}

func SetupMockSystemResourceCheck() trigger.ICheck {
	return &MockSystemResourceCheck{
		SystemResourceCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}

type MockSystemUserCheck struct {
	SystemUserCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockSystemUserCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.SystemUserCheckFunc(config)
}

func SetupMockSystemUserCheck() trigger.ICheck {
	return &MockSystemUserCheck{
		SystemUserCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{}
			return m
		},
	}
}
