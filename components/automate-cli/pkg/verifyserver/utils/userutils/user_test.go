package userutils_test

import (
	"os/user"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/userutils"
	"github.com/stretchr/testify/assert"
)

func TestLookup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	username := "hab"
	sysUser, err := s.Lookup(username)
	if sysUser == nil {
		assert.Equal(t, user.UnknownUserError(username), err)
	}
}

func TestLookupGroup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	groupname := "hab"
	group, err := s.LookupGroup(groupname)
	if group == nil {
		assert.Equal(t, user.UnknownGroupError(groupname), err)
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
