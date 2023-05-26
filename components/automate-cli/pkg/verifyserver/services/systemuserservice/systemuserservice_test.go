package systemuserservice

import (
	"errors"
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/userutils"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	HabUserSuccessTitle                = "User creation/validation check"
	HabUserFailureTitle                = "User validation failure"
	HabGroupSuccessTitle               = "Group creation/validation check"
	HabGroupFailureTitle               = "Group Validation failure"
	HabUserAndGroupMappingSuccessTitle = "User and group mapping successfully"
	HabUserAndGroupMappingFailureTitle = "User and group mapping failed"
	HabUserSuccessMsg                  = "User is created or found successfully"
	HabUserErrorMsg                    = "Hab user creation failed"
	HabUserResolutionMsg               = "Check the user name"
	HabGroupSuccessMsg                 = "Group is created or found successfully"
	HabGroupErrorMsg                   = "Hab group not found"
	HabGroupResolutionMsg              = "Check the group name"
	HabUserAndGroupMapSuccessMSg       = "User and group mapping successful"
	HabPrimaryGroupMatchErrorMsg       = "Primary group mapping for user hab is not hab group"
	HabUserAndGroupMapResolutionMsg    = "Verify the mapping for the user and the group"
)

func TestNewSystemUserService(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)

	s := NewSystemUserService(log)
	assert.NotEqual(t, s, nil)
}

func TestValidateHabUserSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, nil
		},
	}

	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateHabUser()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         HabUserSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabUserFailure(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("user not found")
		},
	}

	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, errors.New("exit status 1")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateHabUser()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         HabUserFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserErrorMsg,
		ResolutionMsg: HabUserResolutionMsg,
	}, service)
}

func TestValidateHabUserCreatedSuccess(t *testing.T) {
	mockUser := &userutils.UserUtilMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("user not found")
		},
	}

	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
		exec: mockExec,
	}
	service, _ := s.ValidateHabUser()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         HabUserSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserSuccessMsg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabGroup()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         HabGroupSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabGroupSuccessMsg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabGroup()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         HabGroupFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabGroupErrorMsg,
		ResolutionMsg: HabGroupResolutionMsg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabPrimaryGroupMatchErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.Checks{
		Title:         HabUserAndGroupMappingSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserAndGroupMapSuccessMSg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabPrimaryGroupMatchErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
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
	s := &SystemUserService{
		Log:  log,
		user: mockUser,
	}
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.Checks{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabPrimaryGroupMatchErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
	}, service)
}

func TestDeleteUser(t *testing.T) {
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, errors.New("Could not delete user")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
		Log:  log,
		exec: mockExec,
	}
	err = s.deleteUser("hab")
	if err != nil {
		assert.Equal(t, "Could not delete user", err.Error())
	}
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
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, nil
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
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
				Checks: []models.Checks{
					{
						Title:         HabUserSuccessTitle,
						Passed:        true,
						SuccessMsg:    HabUserSuccessMsg,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         HabGroupSuccessTitle,
						Passed:        true,
						SuccessMsg:    HabGroupSuccessMsg,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         HabUserAndGroupMappingSuccessTitle,
						Passed:        true,
						SuccessMsg:    HabUserAndGroupMapSuccessMSg,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
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
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte{}, errors.New("exit status 1")
		},
	}
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := &SystemUserService{
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
				Checks: []models.Checks{
					{
						Title:         HabUserFailureTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      HabUserErrorMsg,
						ResolutionMsg: HabUserResolutionMsg,
					},
					{
						Title:         HabGroupFailureTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      HabGroupErrorMsg,
						ResolutionMsg: HabGroupResolutionMsg,
					},
					{
						Title:         HabUserAndGroupMappingFailureTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      HabPrimaryGroupMatchErrorMsg,
						ResolutionMsg: HabUserAndGroupMapResolutionMsg,
					},
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
