//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/config"
	"github.com/stretchr/testify/assert"
)

func TestJobSchedulerBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.JobSchedulerStatusRequest)
		expected = new(ingest.JobSchedulerStatus)
	)
	expected.Running = true

	res, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected.Running, res.Running, "job scheduler should be running by default")
	assert.Equal(t, 3, len(res.Jobs), "there should be three jobs configured")

	// Inspec jobs
	for _, job := range res.Jobs {
		switch job.Name {
		case config.JobList[config.DeleteNodes]:
			assert.Equal(t, false, job.Running, "job should not be running")
			assert.Equal(t, "15m", job.Every, "job every field should be 15m")
			assert.Equal(t, "1d", job.Threshold, "job threshold field should be 1d")
		case config.JobList[config.NodesMissing]:
			assert.Equal(t, true, job.Running, "job should be running")
			assert.Equal(t, "15m", job.Every, "job every field should be 15m")
			assert.Equal(t, "1d", job.Threshold, "job threshold field should be 1d")
		case config.JobList[config.MissingNodesForDeletion]:
			assert.Equal(t, true, job.Running, "job should be running")
			assert.Equal(t, "15m", job.Every, "job every field should be 15m")
			assert.Equal(t, "30d", job.Threshold, "job threshold field should be 30d")
		}
	}
}

func TestJobSchedulerStopAndStart(t *testing.T) {
	var (
		ctx              = context.Background()
		stopReq          = new(ingest.StopJobSchedulerRequest)
		startReq         = new(ingest.StartJobSchedulerRequest)
		statusReq        = new(ingest.JobSchedulerStatusRequest)
		expectedStopRes  = new(ingest.StopJobSchedulerResponse)
		expectedStartRes = new(ingest.StartJobSchedulerResponse)
	)

	// Stop the scheduler
	stopRes, err := suite.JobSchedulerServer.StopJobScheduler(ctx, stopReq)
	assert.Nil(t, err)
	assert.Equal(t, expectedStopRes, stopRes, "stop response should be empty")

	// wait for the scheduler to stop
	waitForModificationsToApply()

	// Check that is not running
	schedulerStatus, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, statusReq)
	assert.Nil(t, err)
	assert.Equal(t, false, schedulerStatus.Running, "job scheduler should not be running")
	// As well as all its jobs
	for _, job := range schedulerStatus.Jobs {
		assert.Equal(t, false, job.Running, "job should not be running")
	}

	// Start the scheduler
	startRes, err := suite.JobSchedulerServer.StartJobScheduler(ctx, startReq)
	assert.Nil(t, err)
	assert.Equal(t, expectedStartRes, startRes, "start response should be empty")

	// Check that is running again
	schedulerStatus, err = suite.JobSchedulerServer.GetStatusJobScheduler(ctx, statusReq)
	assert.Nil(t, err)
	assert.Equal(t, true, schedulerStatus.Running, "job scheduler should be running")
}
