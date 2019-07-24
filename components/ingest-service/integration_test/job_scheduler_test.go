//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/config"
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
