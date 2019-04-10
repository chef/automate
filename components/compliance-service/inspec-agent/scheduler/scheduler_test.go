package scheduler

import (
	"context"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb/dbtest"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/suite"
)

type DBSuite struct {
	dbtest.Suite
}

func (suite *DBSuite) SetupSuite() {
	suite.Database = dbtest.Setup()
}

func (suite *DBSuite) SetupTest() {
	err := dbtest.TruncateTables(suite.Database)
	suite.Require().NoError(err)
}

func TestRunSuiteJobs(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(DBSuite))

	}
}

func (suite *DBSuite) TestRunNodeJobs() {
	a := Scheduler{
		scannerServer: &scanner.Scanner{
			DB: suite.Database,
		},
		db: suite.Database,
	}

	job := &jobs.Job{
		Name:       "my job 1",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	}
	id, err := suite.Database.AddJob(job)
	suite.Require().NoError(err)
	job.Id = id

	ctx := context.Background()
	job, err = suite.Database.GetJob(id)

	nodeJobs := []*types.InspecJob{}
	err = a.runNodeJobs(ctx, job, nodeJobs)
	suite.Equal("no nodes found for job, aborting", err.Error())

	nodeJob := &types.InspecJob{
		InspecBaseJob: types.InspecBaseJob{
			JobID: job.Id,
		},
	}
	err = a.runNodeJobs(ctx, job, []*types.InspecJob{nodeJob})
	suite.Equal("Unable to add jobs to inspec agent: No workers have been instantiated, can't add job", err.Error())
}
