package systemuserservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	HabUserSuccessTitle = "User creation/validation check"
	HabUserFailureTitle = "User validation failure"
	HabGroupSuccessTitle = "Group creation/validation check"
	HabGroupFailureTitle = "Group Validation failure"
	HabUserAndGroupMappingSuccessTitle = "User and group mapping successfully"
	HabUserAndGroupMappingFailureTitle = "User and group mapping failed"
	HabUserSuccessMsg = "User is created or found successfully"
	HabUserErrorMsg = "User found but failed the validation"
	HabUserResolutionMsg = "Check the user name"
	HabGroupSuccessMsg = "Group is created or found successfully"
	HabGroupErrorMsg = "Group found but failed the validation"
	HabGroupResolutionMsg = "Check the group name"
	HabUserAndGroupMapSuccessMSg = "User and group mapping successful"
	HabUserAndGroupMapErrorMsg = "User and group mapping failure"
	HabUserAndGroupMapResolutionMsg = "Verify the mapping for the user and the group"
)

func TestGetSystemUserServiceDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	su := NewSystemUserService(log)
	suDetails := su.GetSystemUserServiceDetails()
	assert.Equal(t, &models.SystemUserResponse{
		Passed: false,
		Checks: []models.SystemUserServiceCheck{
			{
				Title: HabUserFailureTitle,
				Passed: false,
				SuccessMsg: "",
				ErrorMsg: HabUserErrorMsg,
				ResolutionMsg: HabUserResolutionMsg,
			},
			{
				Title: HabGroupFailureTitle,
				Passed: false,
				SuccessMsg: "",
				ErrorMsg: HabGroupErrorMsg,
				ResolutionMsg: HabGroupResolutionMsg,
			},
			{
				Title: HabUserAndGroupMappingFailureTitle,
				Passed: false,
				SuccessMsg: "",
				ErrorMsg: HabUserAndGroupMapErrorMsg,
				ResolutionMsg: HabUserAndGroupMapResolutionMsg,
			},
		},
	}, suDetails)
}

// func TestGetSystemUserServiceDetailsSuccess(t *testing.T) {
// 	log, _ := logger.NewLogger("text", "debug")

// 	type testCase struct {
// 		description string
// 		expectedOutput *models.SystemUserResponse
// 	}
// 	testCases := []testCase {
// 		{
// 			description: "User and Group verified and mapping validated",
// 			expectedOutput: &models.SystemUserResponse{
// 				Passed: true,
// 				Checks: []models.SystemUserServiceCheck{
// 					{
// 						Title: HabUserSuccessTitle,
// 						Passed: true,
// 						SuccessMsg: HabUserSuccessMsg,
// 						ErrorMsg: "",
// 						ResolutionMsg: "",
// 					},
// 					{
// 						Title: HabGroupSuccessTitle,
// 						Passed: true,
// 						SuccessMsg: HabGroupSuccessMsg,
// 						ErrorMsg: "",
// 						ResolutionMsg: "",
// 					},
// 					{
// 						Title: HabUserAndGroupMappingSuccessTitle,
// 						Passed: true,
// 						SuccessMsg: HabUserAndGroupMapSuccessMSg,
// 						ErrorMsg: "",
// 						ResolutionMsg: "",
// 					},
// 				},
// 			},
// 		},
// 	}
// 	for _, tc := range testCases {
// 		t.Run(tc.description, func(t *testing.T) {
// 			su := NewSystemUserService(log)
// 			actualOutput := su.GetSystemUserServiceDetails()
// 			assert.Equal(t, tc.expectedOutput, actualOutput)
// 		})
// 	}
	
// }
