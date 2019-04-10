package scanner

import (
	"context"
	"testing"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/suite"

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb/dbtest"
)

type ScannerDBSuite struct{ dbtest.Suite }

func (suite *ScannerDBSuite) SetupSuite() {
	suite.Database = dbtest.Setup()
}

func (suite *ScannerDBSuite) SetupTest() {
	err := dbtest.TruncateTables(suite.Database)
	suite.Require().NoError(err)
}

func TestRunSuiteJobs(t *testing.T) {
	if dbtest.Run() {
		suite.Run(t, new(ScannerDBSuite))
	}
}

func (suite *ScannerDBSuite) TestUpdateJobStatus() {
	s := Scanner{DB: suite.Database}
	// add a job
	id, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 1",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	// update job to running
	s.UpdateJobStatus(id, "running", nil, nil)

	// and then try to update job to scheduled afterwards - this should not go through
	s.UpdateJobStatus(id, "scheduled", nil, nil)

	job, err := suite.Database.GetJob(id)
	suite.Require().NoError(err)

	suite.Equal("running", job.Status)

	// update job to completed
	s.UpdateJobStatus(id, "completed", nil, nil)
	job, err = suite.Database.GetJob(id)
	suite.Require().NoError(err)

	suite.Equal("completed", job.Status)

	// and then try to update job to running afterwards - this should not go through
	s.UpdateJobStatus(id, "running", nil, nil)

	suite.Equal("completed", job.Status)
}

func (suite *ScannerDBSuite) TestCheckForHungJobs() {
	s := Scanner{DB: suite.Database}

	// add two exec jobs
	id, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 1",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	// update job to running
	s.UpdateJobStatus(id, "running", nil, nil)

	id2, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 2",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	// update job to scheduled
	s.UpdateJobStatus(id2, "scheduled", nil, nil)

	// add one detect job
	id3, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my detect job",
		Recurrence: "",
		Type:       "detect",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	// update job to scheduled
	s.UpdateJobStatus(id3, "scheduled", nil, nil)

	scheduledJobs, err := s.CheckForHungJobs(context.Background())
	suite.Require().NoError(err)

	// add a job and then delete it
	id4, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 2",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)
	err = suite.Database.DeleteJob(id4)
	suite.Require().NoError(err)

	// we only expect job with id2 here. the other exec job was running, and
	// we are explicitly not picking up detect jobs
	suite.Equal([]string{id2}, scheduledJobs)
}

func (suite *ScannerDBSuite) TestIsJobDeleted() {
	s := Scanner{DB: suite.Database}

	// add two exec jobs
	id, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 1",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	id2, err := suite.Database.AddJob(&jobs.Job{
		Name:       "my job 2",
		Recurrence: "",
		Type:       "exec",
		EndTime:    ptypes.TimestampNow(),
	})
	suite.Require().NoError(err)

	// delete job one
	err = suite.Database.DeleteJob(id)
	suite.Require().NoError(err)

	deleted, err := s.IsJobDeleted(id)
	suite.Require().NoError(err)
	// ensure IsJobDeleted returns true for id
	suite.Equal(true, deleted)

	deleted, err = s.IsJobDeleted(id2)
	suite.Require().NoError(err)
	// ensure IsJobDeleted returns false for id2
	suite.Equal(false, deleted)
}
