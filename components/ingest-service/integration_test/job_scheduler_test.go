//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/teambition/rrule-go"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/components/ingest-service/server"
)

func TestJobSchedulerStatus(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.JobSchedulerStatusRequest)
		expected = ingest.JobSchedulerStatus{
			Running: true,
		}
		rec            *rrule.RRule
		err            error
		jobSettingsReq = &ingest.JobSettings{Every: "15m", Running: true}
	)

	rec, err = rrule.NewRRule(rrule.ROption{
		Freq:     rrule.HOURLY,
		Interval: 1,
		Dtstart:  time.Now(),
	})
	require.NoError(t, err)

	// Make sure all of our node jobs are enabled
	_, err = suite.JobSchedulerServer.ConfigureMissingNodesForDeletionScheduler(ctx, jobSettingsReq)
	assert.NoError(t, err)
	_, err = suite.JobSchedulerServer.ConfigureDeleteNodesScheduler(ctx, jobSettingsReq)
	assert.NoError(t, err)
	_, err = suite.JobSchedulerServer.ConfigureNodesMissingScheduler(ctx, jobSettingsReq)
	assert.NoError(t, err)

	t.Run("with purge policies enabled", func(t *testing.T) {
		_, err := suite.PurgeServer.Configure(ctx, &data_lifecycle.ConfigureRequest{
			Enabled:    true,
			Recurrence: rec.String(),
			PolicyUpdate: &data_lifecycle.PolicyUpdate{
				Es: []*data_lifecycle.EsPolicyUpdate{
					{
						PolicyName:    "converge-history",
						OlderThanDays: 14,
					},
					{
						PolicyName:    "actions",
						OlderThanDays: 14,
					},
				},
			},
		})
		require.NoError(t, err)

		res, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, req)
		require.NoError(t, err)
		assert.Equal(t, expected.Running, res.Running, "job scheduler should be running by default")
		assert.Equal(t, 5, len(res.Jobs), "there should be 5 jobs configured")

		verifyInspecJobs(t, res)
		verifyPurgeJobs(t, res)
	})

	t.Run("with purge policies enabled workflow disabled", func(t *testing.T) {
		_, err := suite.PurgeServer.Configure(ctx, &data_lifecycle.ConfigureRequest{
			Enabled:    false,
			Recurrence: rec.String(),
			PolicyUpdate: &data_lifecycle.PolicyUpdate{
				Es: []*data_lifecycle.EsPolicyUpdate{
					{
						PolicyName:    "converge-history",
						OlderThanDays: 14,
					},
					{
						PolicyName:    "actions",
						OlderThanDays: 14,
					},
				},
			},
		})
		require.NoError(t, err)

		res, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, expected.Running, res.Running, "job scheduler should be running by default")
		assert.Equal(t, 3, len(res.Jobs), "there should be 3 jobs configured")

		verifyInspecJobs(t, res)
	})

	t.Run("with purge policies disabled workflow enabled", func(t *testing.T) {
		_, err := suite.PurgeServer.Configure(ctx, &data_lifecycle.ConfigureRequest{
			Enabled:    true,
			Recurrence: rec.String(),
			PolicyUpdate: &data_lifecycle.PolicyUpdate{
				Es: []*data_lifecycle.EsPolicyUpdate{
					{
						PolicyName:    "converge-history",
						OlderThanDays: 14,
						Disabled:      true,
					},
					{
						PolicyName:    "actions",
						OlderThanDays: 14,
						Disabled:      true,
					},
				},
			},
		})
		require.NoError(t, err)

		res, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, expected.Running, res.Running, "job scheduler should be running by default")
		assert.Equal(t, 3, len(res.Jobs), "there should be 3 jobs configured")

		verifyInspecJobs(t, res)
	})
}

func verifyInspecJobs(t *testing.T, status *ingest.JobSchedulerStatus) {
	for _, job := range status.Jobs {
		switch job.Name {
		case config.JobList[config.DeleteNodes]:
			assert.Equal(t, true, job.Running, "job should be running")
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

func verifyPurgeJobs(t *testing.T, status *ingest.JobSchedulerStatus) {
	// Purge jobs
	for _, pp := range server.DefaultPurgePolicies.Es {
		for _, job := range status.Jobs {
			name := strings.ReplaceAll(pp.Name, "-", "_")
			name = fmt.Sprintf("%s_%s", server.PurgeJobName, name)

			if name == job.Name {
				assert.NotEqual(t, pp.Disabled, job.Running, "job is disabled and should be running")
				assert.Equal(t, "14d", job.Threshold, "job threshold field should be 1d")
			}
		}
	}
}
