package user

import (
	"errors"
	gouser "os/user"
	"testing"

	"github.com/stretchr/testify/suite"
)

type LookupTestSuite struct {
	suite.Suite
	p LookupProvider
}

func (suite *LookupTestSuite) TestLookupUser() {
	suite.Run("user is not found", func() {
		_, err := suite.p.Lookup("thisuserdoesnotexist")
		suite.Require().Error(err)
		suite.Require().True(errors.Is(err, gouser.UnknownUserError("thisuserdoesnotexist")))
	})

	suite.Run("user exists", func() {
		u, err := suite.p.Lookup("root")
		suite.Require().NoError(err)
		suite.Require().Equal("root", u.Username)
		suite.Require().Equal("0", u.Uid)
		suite.Require().Equal("0", u.Gid)
		suite.Require().Contains(u.HomeDir, "/root")
	})
}

func (suite *LookupTestSuite) TestLookupUserId() {
	suite.Run("user is not found", func() {
		_, err := suite.p.LookupId("65535") // wikipedia says this value is avoided for reasons
		suite.Require().Error(err)
		suite.Require().True(errors.Is(err, gouser.UnknownUserIdError(65535)))
	})

	suite.Run("user exists", func() {
		u, err := suite.p.Lookup("root")
		suite.Require().NoError(err)
		suite.Require().Equal("root", u.Username)
		suite.Require().Equal("0", u.Uid)
		suite.Require().Equal("0", u.Gid)
		suite.Require().Contains(u.HomeDir, "/root")
	})
}

func (suite *LookupTestSuite) TestLookupGroup() {
	suite.Run("group is not found", func() {
		_, err := suite.p.LookupGroup("thisgroupdoesnotexist")
		suite.Require().Error(err)
		suite.Require().True(errors.Is(err, gouser.UnknownGroupError("thisgroupdoesnotexist")))
	})

	suite.Run("group exists", func() {
		g, err := suite.p.LookupGroup("root")
		suite.Require().NoError(err)
		suite.Require().Equal("root", g.Name)
		suite.Require().Equal("0", g.Gid)
	})
}

func (suite *LookupTestSuite) TestLookupGroupId() {
	suite.Run("group is not found", func() {
		_, err := suite.p.LookupGroupId("4294967295")
		suite.Require().Error(err)
		suite.Require().True(errors.Is(err, gouser.UnknownGroupIdError("4294967295")))
	})

	suite.Run("group exists", func() {
		g, err := suite.p.LookupGroupId("0")
		suite.Require().NoError(err)
		suite.Require().Equal("root", g.Name)
		suite.Require().Equal("0", g.Gid)
	})
}

func TestGetent(t *testing.T) {
	s := &LookupTestSuite{
		p: getentLookupProvider{},
	}
	suite.Run(t, s)
}

func TestGouser(t *testing.T) {
	s := &LookupTestSuite{
		p: goLookupProvider{},
	}
	suite.Run(t, s)
}

type funcLookupProvider struct {
	LookupFunc        func(username string) (*gouser.User, error)
	LookupIdFunc      func(uid string) (*gouser.User, error)
	LookupGroupFunc   func(groupname string) (*gouser.Group, error)
	LookupGroupIdFunc func(gid string) (*gouser.Group, error)
}

func (p funcLookupProvider) Lookup(username string) (*gouser.User, error) {
	return p.LookupFunc(username)
}

func (p funcLookupProvider) LookupId(uid string) (*gouser.User, error) {
	return p.LookupIdFunc(uid)
}

func (p funcLookupProvider) LookupGroup(groupname string) (*gouser.Group, error) {
	return p.LookupGroupFunc(groupname)
}

func (p funcLookupProvider) LookupGroupId(gid string) (*gouser.Group, error) {
	return p.LookupGroupIdFunc(gid)
}

func TestChainFallback(t *testing.T) {
	fallback := funcLookupProvider{
		LookupFunc: func(username string) (*gouser.User, error) {
			return nil, errors.New("fails")
		},
		LookupIdFunc: func(uid string) (*gouser.User, error) {
			return nil, errors.New("fails")
		},
		LookupGroupFunc: func(groupname string) (*gouser.Group, error) {
			return nil, errors.New("fails")
		},
		LookupGroupIdFunc: func(gid string) (*gouser.Group, error) {
			return nil, errors.New("fails")
		},
	}
	s := &LookupTestSuite{
		p: &chainLookupProvider{
			providers: []LookupProvider{
				fallback,
				&goLookupProvider{},
			},
		},
	}
	suite.Run(t, s)
}
