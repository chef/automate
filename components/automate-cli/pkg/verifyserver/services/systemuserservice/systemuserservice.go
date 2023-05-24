package systemuserservice

import (
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
)

//serivce have variable, that variable get the instance

type SystemUser interface {
	GetSystemUserServiceDetails() *models.SystemUserResponse
}

type SystemUserService struct {
	exec fiberutils.ExecCmdService
	user fiberutils.UserCmdService
	Log  logger.Logger
}

func NewSystemUserService(log logger.Logger) *SystemUserService {
	return &SystemUserService{
		exec: fiberutils.NewExecCmdServiceImp(),
		user: fiberutils.NewUserCmdServiceImp(),
		Log:  log,
	}
}

func (su *SystemUserService) GetSystemUserServiceDetails() *models.SystemUserResponse {
	serviceResponse := &models.SystemUserResponse{}
	serviceResponse.Passed = true
	serviceResponseArray := []*models.SystemUserServiceCheck{}

	habUserResponse, isHabUserCreated := su.ValidateHabUser()
	if isHabUserCreated {
		defer su.deleteUser(constants.USERNAME)
	}
	if !habUserResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponseArray = append(serviceResponseArray, habUserResponse)

	habGroupResponse := su.ValidateHabGroup()
	if !habGroupResponse.Passed {
		serviceResponse.Passed = false
	}

	habUserAndGroupMapResponse := su.ValidateHabUserAndGroupMapping()
	if !habUserAndGroupMapResponse.Passed {
		serviceResponse.Passed = false
	}

	serviceResponseArray = append(serviceResponseArray, habGroupResponse, habUserAndGroupMapResponse)
	checks := make([]models.SystemUserServiceCheck, len(serviceResponseArray))
	for i, serviceResp := range serviceResponseArray {
		checks[i] = *serviceResp
	}
	serviceResponse.Checks = checks
	return serviceResponse

}

func (su *SystemUserService) ValidateHabUser() (*models.SystemUserServiceCheck, bool) {
	isHabUserPresent := su.isHabUserPresent(constants.USERNAME)

	if !isHabUserPresent {
		_, err := su.exec.Command(constants.USERADDCMD, []string{constants.USERNAME})
		if err != nil {
			su.Log.Error("Failed to create user 'hab':", err)
			return failureResponse(constants.SYSTEM_USER_HAB_VALIDATION_FAILURE_TITLE, constants.SYSTEM_USER_HAB_ERROR_MSG, constants.SYSTEM_USER_HAB_RESOLUTION_MSG), false
		}
		su.Log.Info("Created 'hab' user")
		return successResponse(constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_USER_HAB_SUCCESS_MSG), true
	}
	su.Log.Info("User 'hab' found successfully")
	return successResponse(constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_USER_HAB_SUCCESS_MSG), false
}

func (su *SystemUserService) ValidateHabGroup() *models.SystemUserServiceCheck {
	isHabGroupPresent := su.isHabGroupPresent(constants.GROUPNAME)

	if !isHabGroupPresent {
		su.Log.Error("Group 'hab' doesn't exists")
		return failureResponse(constants.SYSTEM_GROUP_HAB_VALIDATION_FAILURE_TITLE, constants.SYSTEM_GROUP_HAB_ERROR_MSG, constants.SYSTEM_GROUP_HAB_RESOLUTION_MSG)
	}
	su.Log.Info("Group 'hab' found successfully")
	return successResponse(constants.SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_GROUP_HAB_SUCCESS_MSG)
}

func (su *SystemUserService) ValidateHabUserAndGroupMapping() *models.SystemUserServiceCheck {
	// Check if "hab" user is a member of "hab" group
	userInGroup := su.checkUserInGroup(constants.USERNAME, constants.GROUPNAME)

	if !userInGroup {
		su.Log.Error("Validation failed: User 'hab' is not a member of group 'hab'")
		return failureResponse(constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE, constants.SYSTEM_USERANDGROUP_MAPPING_ERROR_MSG, constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG)
	}
	su.Log.Info("User 'hab' is a member of group 'hab'")

	// Check if "hab" user's primary group is "hab" group
	primaryGroupMatch := su.checkPrimaryGroupMatch(constants.USERNAME, constants.GROUPNAME)

	if !primaryGroupMatch {
		su.Log.Error("Validation failed: Primary group mapping for user 'hab' is not 'hab' group")
		return failureResponse(constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE, constants.SYSTEM_USERANDGROUP_MAPPING_ERROR_MSG, constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG)
	}

	su.Log.Info("Validation passed: Primary group mapping for user 'hab' is 'hab' group")
	return successResponse(constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE, constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG)
}

// Check if "hab" user exists
func (su *SystemUserService) isHabUserPresent(username string) bool {
	_, err := su.user.Lookup(username)
	return err == nil
}

// Check if "hab" group exists
func (su *SystemUserService) isHabGroupPresent(groupname string) bool {
	_, err := su.user.LookupGroup(groupname)
	return err == nil
}

// Delete a user
func (su *SystemUserService) deleteUser(username string) error {
	_, err := su.exec.Command(constants.USERDELCMD, []string{username})
	return err
}

// Check if a user is a member of a group
func (su *SystemUserService) checkUserInGroup(username, groupname string) bool {
	output, err := su.exec.Command("id", []string{"-Gn", username})
	if err != nil {
		return false
	}

	groups := strings.Split(strings.TrimSpace(string(output)), " ")
	for _, group := range groups {
		if group == groupname {
			return true
		}
	}
	return false
}

// Check if a user's primary group matches a specified group
func (su *SystemUserService) checkPrimaryGroupMatch(username, groupname string) bool {
	u, err := su.user.Lookup(username)
	if err != nil {
		su.Log.Error("User 'hab' not found")
		return false
	}

	groups, err := u.GroupIds()
	if err != nil {
		return false
	}

	for _, gid := range groups {
		group, err := su.user.LookupGroupId(gid)
		if err != nil {
			su.Log.Debug("Group 'hab' not found")
			return false
		}

		if groupname == group.Name {
			su.Log.Debug("Primary Group 'hab' found")
			return true
		}
	}
	return false
}

func successResponse(title string, successMsg string) *models.SystemUserServiceCheck {
	checkResponse := &models.SystemUserServiceCheck{
		Title:         title,
		Passed:        true,
		SuccessMsg:    successMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return checkResponse
}

func failureResponse(title string, errorMsg string, resolutionMsg string) *models.SystemUserServiceCheck {
	checkResponse := &models.SystemUserServiceCheck{
		Title:         title,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
	return checkResponse
}
