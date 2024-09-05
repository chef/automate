package user

import (
	"errors"
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
		suite.Require().True(errors.Is(err, UnknownUserError("thisuserdoesnotexist")))
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
		suite.Require().True(errors.Is(err, UnknownUserIdError(65535)))
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
		suite.Require().True(errors.Is(err, UnknownGroupError("thisgroupdoesnotexist")))
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
		suite.Require().True(errors.Is(err, UnknownGroupIdError("4294967295")))
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
		p: newGetentLookupProvider(),
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
	LookupFunc        func(username string) (*User, error)
	LookupIdFunc      func(uid string) (*User, error)
	LookupGroupFunc   func(groupname string) (*Group, error)
	LookupGroupIdFunc func(gid string) (*Group, error)
}

func (p funcLookupProvider) Lookup(username string) (*User, error) {
	return p.LookupFunc(username)
}

func (p funcLookupProvider) LookupId(uid string) (*User, error) {
	return p.LookupIdFunc(uid)
}

func (p funcLookupProvider) LookupGroup(groupname string) (*Group, error) {
	return p.LookupGroupFunc(groupname)
}

func (p funcLookupProvider) LookupGroupId(gid string) (*Group, error) {
	return p.LookupGroupIdFunc(gid)
}

func TestChainFallback(t *testing.T) {
	fallback := funcLookupProvider{
		LookupFunc: func(username string) (*User, error) {
			return nil, errors.New("fails")
		},
		LookupIdFunc: func(uid string) (*User, error) {
			return nil, errors.New("fails")
		},
		LookupGroupFunc: func(groupname string) (*Group, error) {
			return nil, errors.New("fails")
		},
		LookupGroupIdFunc: func(gid string) (*Group, error) {
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
