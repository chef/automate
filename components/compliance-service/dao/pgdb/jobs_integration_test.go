package pgdb_test

import (
	"context"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/suite"

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb/dbtest"
)

type JobsDBSuite dbtest.Suite

func (suite *JobsDBSuite) SetupSuite() {
	suite.Database = dbtest.Setup()
}

func (suite *JobsDBSuite) SetupTest() {
	err := dbtest.TruncateTables(suite.Database)
	suite.Require().NoError(err)
}

func TestRunSuiteJobs(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(JobsDBSuite))
	}
}

func (suite *JobsDBSuite) TestListInitiatedScans() {
	now := time.Now()
	nowMinus5 := now.Add(time.Duration(-5) * time.Minute)
	_, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 1",
		Status:     "completed",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	_, err = suite.Database.AddJob(&jobs.Job{
		Name:       "my job 2",
		Status:     "running",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	_, err = suite.Database.AddJob(&jobs.Job{
		Name:       "my job 3",
		Status:     "scheduled",
		Recurrence: "bla",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	_, err = suite.Database.AddJob(&jobs.Job{
		Name:       "my job 4",
		Status:     "running",
		Recurrence: "",
		Type:       "detect",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	timestamp, err := ptypes.TimestampProto(nowMinus5)
	suite.Require().NoError(err)

	reports, err := suite.Database.ListInitiatedScans(context.Background(), timestamp)
	suite.Require().NoError(err)
	suite.Equal(2, len(reports))
}
