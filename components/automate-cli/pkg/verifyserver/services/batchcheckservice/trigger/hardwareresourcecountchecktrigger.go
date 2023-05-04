package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type HardwareResourceCountCheck struct {
}

func NewHardwareResourceCountCheck() *HardwareResourceCountCheck {
	return &HardwareResourceCountCheck{}
}

// func (hrc *CheckTrigger) HardwareResourceCountCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *HardwareResourceCountCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "1.2.3.4": {
        	Passed: false,
        	Checks: []models.CheckResponse{
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-hardware",
					},
				}, 
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-hardware",
					},
				},
			},
        },
		"1.2.4.5":{
			Passed: true,
        	Checks: []models.CheckResponse{
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-hardware",
					},
				}, 
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-hardware",
					},
				},
			},
		},
    }
	return m
}