package userutils_test

import (
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/userutils"
	"github.com/stretchr/testify/assert"
)

func TestLookup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	sysUser, err := s.Lookup(constants.USERNAME)
	if sysUser == nil {
		assert.Equal(t, user.UnknownUserError(constants.USERNAME), err)
	}
}

func TestLookupGroup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	group, err := s.LookupGroup(constants.GROUPNAME)
	if group == nil {
		assert.Equal(t, user.UnknownGroupError(constants.GROUPNAME), err)
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
