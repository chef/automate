package systemuserservice

import (
	"errors"
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/executil"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/userutils"
	"github.com/stretchr/testify/assert"
)

func TestNewSystemUserService(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)

	s := NewSystemUserService(log, executil.NewExecCmdServiceImp(), userutils.NewUserUtilImp())
	assert.NotEqual(t, s, nil)
}

func TestValidateHabUserSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, nil
		},
	}

	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateOrCreateHabUser()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE,
		Passed:        true,
		SuccessMsg:    constants.SYSTEM_USER_HAB_SUCCESS_MSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestGetUserAndGroupID(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{
				Uid: "1002",
				Gid: "1002",
			}, nil
		},
	}

	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.getUserAndGroupID()

	assert.Equal(t, service.UserID, "1002")
	assert.Equal(t, service.GroupID, "1002")
}

func TestValidateHabUserFailure(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("user not found")
		},
	}

	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, errors.New("exit status 1")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateOrCreateHabUser()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USER_HAB_VALIDATION_FAILURE_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.SYSTEM_USER_HAB_ERROR_MSG,
		ResolutionMsg: constants.SYSTEM_USER_HAB_RESOLUTION_MSG,
	}, service)
}

func TestValidateHabUserCreatedSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("user not found")
		},
	}

	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateOrCreateHabUser()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE,
		Passed:        true,
		SuccessMsg:    constants.SYSTEM_USER_HAB_SUCCESS_MSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabGroup(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabGroup()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE,
		Passed:        true,
		SuccessMsg:    constants.SYSTEM_GROUP_HAB_SUCCESS_MSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabGroupFailed(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, errors.New("Group 'hab' doesn't exists")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabGroup()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_GROUP_HAB_VALIDATION_FAILURE_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.SYSTEM_GROUP_HAB_ERROR_MSG,
		ResolutionMsg: constants.SYSTEM_GROUP_HAB_RESOLUTION_MSG,
	}, service)
}

func TestValidateHabUserAndGroupMappingFailed(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{
				Uid:      "1001",
				Gid:      "1001",
				Username: "hab",
				Name:     "hab",
				HomeDir:  "",
			}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{
				Gid:  "1002",
				Name: "test",
			}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG,
		ResolutionMsg: constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG,
	}, service)
}

func TestValidateHabUserAndGroupMappingSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{
				Uid:      "1001",
				Gid:      "1001",
				Username: "hab",
				Name:     "hab",
				HomeDir:  "",
			}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{
				Gid:  "1001",
				Name: "hab",
			}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE,
		Passed:        true,
		SuccessMsg:    constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabUserAndGroupMappingFailedForLookupUsernameError(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("User not found")
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG,
		ResolutionMsg: constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG,
	}, service)
}

func TestValidateHabUserAndGroupMappingFailedForLookUpGroupId(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{
				Uid:      "1001",
				Gid:      "1001",
				Username: "hab",
				Name:     "hab",
				HomeDir:  "",
			}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, errors.New("Group not found with Gid")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG,
		ResolutionMsg: constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG,
	}, service)
}

func TestGetSystemUserServiceDetailsSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{
				Uid:      "1001",
				Gid:      "1001",
				Username: "hab",
				Name:     "hab",
				HomeDir:  "",
			}, nil
		},
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{
				Gid:  "1001",
				Name: "hab",
			}, nil
		},
	}
	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		exec: mockExec,
		user: mockUser,
	}

	tests := []struct {
		description  string
		expectedBody *models.SystemUserResponse
	}{
		{
			description: "User and group not validated and mapping not successful",
			expectedBody: &models.SystemUserResponse{
				Passed: true,
				Checks: []*models.Checks{
					{
						Title:         constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_USER_HAB_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_GROUP_HAB_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
				Id: &models.SystemUserID{
					UserID:  "1001",
					GroupID: "1001",
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got := s.GetSystemUserServiceDetails()
			assert.Equal(t, tt.expectedBody, got)
		})
	}
}

func TestGetSystemUserServiceDetailsFailed(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("user not found")
		},
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, errors.New("Group 'hab' doesn't exists")
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, errors.New("Primary group mapping failed")
		},
	}
	mockExec := &executil.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, errors.New("exit status 1")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserServiceImp{
		Log:  log,
		exec: mockExec,
		user: mockUser,
	}

	tests := []struct {
		description  string
		expectedBody *models.SystemUserResponse
	}{
		{
			description: "User and group not validated and mapping not successful",
			expectedBody: &models.SystemUserResponse{
				Passed: false,
				Checks: []*models.Checks{
					{
						Title:         constants.SYSTEM_USER_HAB_VALIDATION_FAILURE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.SYSTEM_USER_HAB_ERROR_MSG,
						ResolutionMsg: constants.SYSTEM_USER_HAB_RESOLUTION_MSG,
					},
					{
						Title:         constants.SYSTEM_GROUP_HAB_VALIDATION_FAILURE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.SYSTEM_GROUP_HAB_ERROR_MSG,
						ResolutionMsg: constants.SYSTEM_GROUP_HAB_RESOLUTION_MSG,
					},
					{
						Title:         constants.SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      constants.SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG,
						ResolutionMsg: constants.SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG,
					},
				},
				Id: &models.SystemUserID{UserID: "", GroupID: ""},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got := s.GetSystemUserServiceDetails()
			assert.Equal(t, tt.expectedBody, got)
		})
	}
}
