package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SshUserAccessCheck struct {
}

func NewSshUserAccessCheck() *SshUserAccessCheck {
	return &SshUserAccessCheck{}
}
// func (hrc *CheckTrigger) SshUserAccessCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *SshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "1.2.3.4": {
        	Passed: false,
        	Checks: []models.CheckResponse{
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-ssh-user",
					},
				}, 
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-ssh-user",
					},
				},
			},
        },
		"1.2.4.5":{
			Passed: true,
        	Checks: []models.CheckResponse{
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-ssh-user",
					},
				}, 
				{
					Checks: struct{Title string "json:\"title\""; Passed bool "json:\"passed\""; SuccessMsg string "json:\"success_msg\""; ErrorMsg string "json:\"error_msg\""; ResolutionMsg string "json:\"resolution_msg\""}{
						Title: "check-ssh-user",
					},
				},
			},
		},
    }
	return m
}
