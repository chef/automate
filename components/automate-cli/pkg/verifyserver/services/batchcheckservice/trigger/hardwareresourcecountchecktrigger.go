package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type HardwareResourceCountCheck struct {
}

func NewHardwareResourceCountCheck() *HardwareResourceCountCheck {
	return &HardwareResourceCountCheck{}
}

func (ss *HardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{}
	return m
}
