package db_test

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/compliance-service/dao/pgdb/dbtest"
	"github.com/chef/automate/components/compliance-service/profiles/db"

	"github.com/stretchr/testify/suite"
)

type ProfileStoreIntegrationTest struct {
	suite.Suite
	Store *db.Store
}

func (suite *ProfileStoreIntegrationTest) SetupSuite() {
	suite.Store = &db.Store{DB: dbtest.Setup()}
}

func (suite *ProfileStoreIntegrationTest) SetupTest() {
	err := dbtest.TruncateTables(suite.Store.DB)
	suite.Require().NoError(err)
}

func TestRunSuite(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(ProfileStoreIntegrationTest))
	}
}

func (suite *ProfileStoreIntegrationTest) TestDeleteRemovesProfilesForCurrentUser() {
	name := "foo"
	version := "1.2.3.4"
	sha256 := "12345"
	tar := []byte{}
	info := []byte(fmt.Sprintf(`{"name":%q,"version":%q}`, name, version))

	err := suite.Store.UploadProfile(sha256, "alice", tar, info)
	suite.Require().NoError(err)

	profile, err := suite.Store.Read("alice", name, version)
	suite.NotNil(profile)
	suite.NoError(err)

	err = suite.Store.UploadProfile(sha256, "bob", tar, info)
	suite.Require().NoError(err)

	profile, err = suite.Store.Read("bob", name, version)
	suite.NotNil(profile)
	suite.NoError(err)

	err = suite.Store.Delete("bob", name, version)
	suite.Require().NoError(err)

	profile, err = suite.Store.Read("bob", name, version)
	suite.Nil(profile)
	suite.Error(err)

	profile, err = suite.Store.Read("alice", name, version)
	suite.NotNil(profile)
	suite.NoError(err)
}
