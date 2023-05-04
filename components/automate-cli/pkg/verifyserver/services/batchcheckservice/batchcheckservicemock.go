package batchcheckservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
)

type MockHardwareResourceCountCheck struct {
	HardwareResourceCountCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockHardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.HardwareResourceCountCheckFunc(config)
}

type MockSshUserAccessCheck struct {
	SshUserAccessCheckFunc func(config models.Config) map[string]models.CheckTriggerResponse
}

func (mss *MockSshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	return mss.SshUserAccessCheckFunc(config)
}

// func SetupMockCheckTrigger() trigger.CheckTrigger {
// 	return &trigger.MockCheckTrigger{
// 		HardwareResourceCountCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{
// 				Passed: true,
// 				Checks: []models.CheckResponse{
// 					{
// 						Checks: struct {
// 							Title         string "json:\"title\""
// 							Passed        bool   "json:\"passed\""
// 							SuccessMsg    string "json:\"success_msg\""
// 							ErrorMsg      string "json:\"error_msg\""
// 							ResolutionMsg string "json:\"resolution_msg\""
// 						}{},
// 					},
// 					{
// 						Checks: struct {
// 							Title         string "json:\"title\""
// 							Passed        bool   "json:\"passed\""
// 							SuccessMsg    string "json:\"success_msg\""
// 							ErrorMsg      string "json:\"error_msg\""
// 							ResolutionMsg string "json:\"resolution_msg\""
// 						}{},
// 					},
// 				},
// 			}
// 		},
// 		SshUserAccessCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		CertificateCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		SystemResourceCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		ExternalOpensearchCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		ExternalPostgresCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		FirewallCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		FqdnCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		NfsBackupConfigCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		S3BackupConfigCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		SoftwareVersionCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 		SystemUserCheckFunc: func(config models.Config) models.CheckTriggerResponse {
// 			return models.CheckTriggerResponse{}
// 		},
// 	}
// }

func SetupMockHardwareResourceCountCheck() trigger.ICheck {
	return &MockHardwareResourceCountCheck{
		HardwareResourceCountCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "check-hardware",
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
									Title: "check-hardware",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "check-hardware",
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
									Title: "check-hardware",
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

func SetupMockSshUserAccessCheck() trigger.ICheck {
	return &MockSshUserAccessCheck{
		SshUserAccessCheckFunc: func(config models.Config) map[string]models.CheckTriggerResponse {
			m := map[string]models.CheckTriggerResponse{
				"1.2.3.4": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed: false,
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "check-ssh-user",
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
									Title: "check-ssh-user",
								},
							},
						},
					},
				},
				"1.2.4.5": {
					Status: "Passed",
					Result: models.ApiResult{
						Passed: true,
						Checks: []models.CheckResponse{
							{
								Checks: struct {
									Title         string "json:\"title\""
									Passed        bool   "json:\"passed\""
									SuccessMsg    string "json:\"success_msg\""
									ErrorMsg      string "json:\"error_msg\""
									ResolutionMsg string "json:\"resolution_msg\""
								}{
									Title: "check-ssh-user",
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
									Title: "check-ssh-user",
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
