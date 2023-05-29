package userutils_test

import (
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/lib/userutils"
	"github.com/stretchr/testify/assert"
)

func TestLookup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	sysUser, err := s.Lookup(constants.USER_NAME)
	if sysUser == nil {
		assert.Equal(t, user.UnknownUserError(constants.USER_NAME), err)
	}
}

func TestLookupGroup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	group, err := s.LookupGroup(constants.GROUP_NAME)
	if group == nil {
		assert.Equal(t, user.UnknownGroupError(constants.GROUP_NAME), err)
	}
}

func TestLookupGroupId(t *testing.T) {
	s := &userutils.UserUtilImp{}
	gid := "1000"
	group, err := s.LookupGroupId(gid)
	if group == nil {
		assert.Equal(t, user.UnknownGroupIdError(gid), err)
	}
}
