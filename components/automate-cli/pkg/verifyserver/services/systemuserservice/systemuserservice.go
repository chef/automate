package systemuserservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/executil"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/userutils"
)

type SystemUserService interface {
	GetSystemUserServiceDetails() *models.SystemUserResponse
}

type SystemUserServiceImp struct {
	exec executil.ExecCmdService
	user userutils.UserUtil
	Log  logger.Logger
}

func NewSystemUserService(log logger.Logger, exec executil.ExecCmdService, user userutils.UserUtil) *SystemUserServiceImp {
	return &SystemUserServiceImp {
		exec: exec,
		user: user,
		Log:  log,
	}
}

func (su *SystemUserServiceImp) GetSystemUserServiceDetails() *models.SystemUserResponse {
	serviceResponse := &models.SystemUserResponse{}
	serviceResponse.Passed = true
	serviceResponseArray := []*models.Checks{}

	habUserResponse, isHabUserCreated := su.ValidateOrCreateHabUser()
	if !habUserResponse.Passed {
		serviceResponse.Passed = false
	}

	habGroupResponse := su.ValidateHabGroup()
	if !habGroupResponse.Passed {
		serviceResponse.Passed = false
	}

	habUserAndGroupMapResponse := su.ValidateHabUserAndGroupMapping()
	if !habUserAndGroupMapResponse.Passed {
		serviceResponse.Passed = false
	}

	if isHabUserCreated {
		defer func() {
			_, _ = su.exec.Command(constants.USER_DEL_CMD, []string{constants.USER_NAME})

			if su.isHabGroupPresent(constants.GROUP_NAME) {
				_, _ = su.exec.Command(constants.GROUP_DEL_CMD, []string{constants.GROUP_NAME})
				su.Log.Debug("Group hab deleted")
			}
		}()

	}

	serviceResponseArray = append(serviceResponseArray, habUserResponse, habGroupResponse, habUserAndGroupMapResponse)

	serviceResponse.Checks = serviceResponseArray
	return serviceResponse

}

func (su *SystemUserServiceImp) ValidateOrCreateHabUser() (*models.Checks, bool) {
	isHabUserPresent := su.isHabUserPresent(constants.USER_NAME)

	if !isHabUserPresent {
		_, err := su.exec.Command(constants.USER_ADD_CMD, []string{"-U", constants.USER_NAME})
		if err != nil {
			su.Log.Error("Failed to create user 'hab':", err)
			return failureResponse(constants.SYSTEM_USER_HAB_VALIDATION_FAILURE_TITLE, constants.SYSTEM_USER_HAB_ERROR_MSG, constants.SYSTEM_USER_HAB_RESOLUTION_MSG), false
		}
		su.Log.Debug("Created 'hab' user and group ")
		return successResponse(constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_USER_HAB_SUCCESS_MSG), true
	}
	su.Log.Debug("User 'hab' found successfully")
	return successResponse(constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_USER_HAB_SUCCESS_MSG), false
}

func (su *SystemUserServiceImp) ValidateHabGroup() *models.Checks {
	isHabGroupPresent := su.isHabGroupPresent(constants.GROUP_NAME)

	if !isHabGroupPresent {
		su.Log.Error("Group 'hab' doesn't exists")
		return failureResponse(constants.SYSTEM_GROUP_HAB_VALIDATION_FAILURE_TITLE, constants.SYSTEM_GROUP_HAB_ERROR_MSG, constants.SYSTEM_GROUP_HAB_RESOLUTION_MSG)
	}
	su.Log.Debug("Group 'hab' found successfully")
	return successResponse(constants.SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE, constants.SYSTEM_GROUP_HAB_SUCCESS_MSG)
}

func (su *SystemUserServiceImp) ValidateHabUserAndGroupMapping() *models.Checks {
	// Check if "hab" user's primary group is "hab" group
	userPrimaryGroup := su.checkUserPrimaryGroup(constants.USER_NAME, constants.GROUP_NAME)

	if !userPrimaryGroup {
		su.Log.Error("Validation failed: Primary group mapping for user 'hab' is not 'hab' group")
		return failureResponse(constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE, constants.SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG, constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG)
	}
	su.Log.Debug("Validation passed: Primary group mapping for user 'hab' is 'hab' group")

	return successResponse(constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE, constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG)
}

// Check if "hab" user exists
func (su *SystemUserServiceImp) isHabUserPresent(username string) bool {
	_, err := su.user.Lookup(username)
	if err != nil {
		su.Log.Error("Hab user not present: ", err)
		return false
	}
	su.Log.Debug("Hab user exists")
	return true
}

// Check if "hab" group exists
func (su *SystemUserServiceImp) isHabGroupPresent(groupname string) bool {
	_, err := su.user.LookupGroup(groupname)
	if err != nil {
		su.Log.Error("Hab group not present: ", err)
		return false
	}
	su.Log.Debug("Hab group exists")
	return true
}

// Check for user's primary group
func (su *SystemUserServiceImp) checkUserPrimaryGroup(username, groupname string) bool {
	sysUser, err := su.user.Lookup(username)
	if err != nil {
		su.Log.Error("User not found: ", err)
		return false
	}

	// Get the user's primary group
	group, err := su.user.LookupGroupId(sysUser.Gid)
	if err != nil {
		su.Log.Error("Group not found by Gid '"+sysUser.Gid+"': ", err)
		return false
	}

	if group.Name != groupname {
		su.Log.Debug("User's primary group is not hab")
		return false
	}
	su.Log.Debug("User's primary group is hab with gid '"+ sysUser.Gid+"'")
	return true
}

func successResponse(title string, successMsg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        true,
		SuccessMsg:    successMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return checkResponse
}

func failureResponse(title string, errorMsg string, resolutionMsg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
	return checkResponse
}
