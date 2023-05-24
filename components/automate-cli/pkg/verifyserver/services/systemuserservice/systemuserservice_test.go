package systemuserservice

import (
	"errors"
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
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
	HabUserErrorMsg                    = "User found but failed the validation"
	HabUserResolutionMsg               = "Check the user name"
	HabGroupSuccessMsg                 = "Group is created or found successfully"
	HabGroupErrorMsg                   = "Group found but failed the validation"
	HabGroupResolutionMsg              = "Check the group name"
	HabUserAndGroupMapSuccessMSg       = "User and group mapping successful"
	HabUserAndGroupMapErrorMsg         = "User and group mapping failure"
	HabUserAndGroupMapResolutionMsg    = "Verify the mapping for the user and the group"
)

func TestNewSystemUserService(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)

	s := NewSystemUserService(log)
	assert.NotEqual(t, s, nil)
}

func TestValidateHabUserSuccess(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return nil, nil
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
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabUserFailure(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return nil, errors.New("user not found")
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
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserErrorMsg,
		ResolutionMsg: HabUserResolutionMsg,
	}, service)
}

func TestValidateHabUserCreatedSuccess(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return nil, errors.New("user not found")
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
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabGroup(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return nil, nil
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
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabGroupSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabGroupSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabGroupFailed(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupGroupFunc: func(name string) (*user.Group, error) {
			return nil, errors.New("Group 'hab' doesn't exists")
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
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabGroupFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabGroupErrorMsg,
		ResolutionMsg: HabGroupResolutionMsg,
	}, service)
}

func TestValidateHabUserAndGroupMappingFailedForWrongGroupName(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("group1"), nil
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserAndGroupMapErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
	}, service)
}

func TestValidateHabUserAndGroupMappingPrimaryGroupMappingFailed(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("hab"), nil
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserAndGroupMapErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
	}, service)
}

func TestValidateHabUserAndGroupMappingSuccess(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
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
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("hab"), nil
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, true)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingSuccessTitle,
		Passed:        true,
		SuccessMsg:    HabUserAndGroupMapSuccessMSg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, service)
}

func TestValidateHabUserAndGroupMappingCommandExecutionFail(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, nil
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("hab"), errors.New("Command execution failed")
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserAndGroupMapErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
	}, service)
}

func TestValidateHabUserAndGroupMappingFailedForLookupUsernameError(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
		LookupFunc: func(name string) (*user.User, error) {
			return &user.User{}, errors.New("Primary group mapping for user 'hab' is not 'hab' group")
		},
		LookupGroupIdFunc: func(name string) (*user.Group, error) {
			return &user.Group{}, nil
		},
	}
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("hab"), nil
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserAndGroupMapErrorMsg,
		ResolutionMsg: HabUserAndGroupMapResolutionMsg,
	}, service)
}

func TestValidateHabUserAndGroupMappingFailedForLookUpGroupId(t *testing.T) {
	mockUser := &fiberutils.UserCmdServiceMock{
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
			return &user.Group{}, errors.New("Group not found")
		},
	}
	mockExec := &fiberutils.ExecCmdServiceMock{
		CommandFunc: func(name string, args []string) ([]byte, error) {
			return []byte("hab"), nil
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
	service := s.ValidateHabUserAndGroupMapping()
	assert.Equal(t, service.Passed, false)
	assert.Equal(t, &models.SystemUserServiceCheck{
		Title:         HabUserAndGroupMappingFailureTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      HabUserAndGroupMapErrorMsg,
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
