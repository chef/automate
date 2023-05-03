package batchcheckservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/stretchr/testify/assert"
)

func SetupMockCheckTrigger() trigger.ICheckTrigger {
	return &trigger.MockCheckTrigger{
		HardwareResourceCountCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{
				Ip: "1.2.3.4", 
				Passed: true, 
				Checks: []models.CheckResponse{
					{
						Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{},
					}, 
					{
						Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{},
					},
				},
			}
		},
		SshUserAccessCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		CertificateCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		SystemResourceCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		ExternalOpensearchCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		ExternalPostgresCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		FirewallCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		FqdnCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		NfsBackupConfigCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		S3BackupConfigCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		SoftwareVersionCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
		SystemUserCheckFunc: func(config models.Config) models.CheckTriggerResponse {
			return models.CheckTriggerResponse{}
		},
	}
}

func TestStatusService(t *testing.T) {
	ss := NewBatchCheckService(trigger.NewCheckTrigger())
	services := ss.BatchCheck([]string{""}, models.Config{})
	assert.Equal(t, []models.ServiceDetails{}, services)
}
