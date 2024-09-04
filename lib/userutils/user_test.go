package userutils_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/lib/userutils"
	"github.com/stretchr/testify/assert"
)

func TestLookup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	sysUser, err := s.Lookup(constants.USER_NAME)
	if sysUser == nil {
		assert.Contains(t, constants.USER_NAME, err.Error())
	}
}

func TestLookupGroup(t *testing.T) {
	s := &userutils.UserUtilImp{}
	group, err := s.LookupGroup(constants.GROUP_NAME)
	if group == nil {
		assert.Contains(t, constants.GROUP_NAME, err.Error())
	}
}

func TestLookupGroupId(t *testing.T) {
	s := &userutils.UserUtilImp{}
	gid := "1000"
	group, err := s.LookupGroupId(gid)

	if group == nil {
		assert.Contains(t, gid, err.Error())
	}
}
