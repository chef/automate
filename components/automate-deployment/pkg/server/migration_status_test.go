package server

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/ingest"
	compliance "github.com/chef/automate/components/compliance-service/api/status"
)

func TestIngestResponseToServiceMigrationStatus(t *testing.T) {
	t.Run("identifies if the ingest migration failed", func(t *testing.T) {
		input := &ingest.MigrationStatus{
			Finished:  true,
			Total:     2,
			Completed: 1,
			Status:    "Error: something-bad-happened",
		}

		s := ingestResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_FAILED, s.Status)
	})

	t.Run("identifies when the migration has successfully completed", func(t *testing.T) {
		input := &ingest.MigrationStatus{
			Finished:  true,
			Total:     2,
			Completed: 2,
			Status:    "",
		}

		s := ingestResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_COMPLETE, s.Status)
		assert.Equal(t, int32(100), s.Progress)
	})

	t.Run("calculates the percentage done of an IN_PROGRESS migration", func(t *testing.T) {
		input := &ingest.MigrationStatus{
			Finished:  false,
			Total:     7,
			Completed: 3,
			Status:    "Doing something",
		}

		s := ingestResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, s.Status)
		assert.Equal(t, int32(42), s.Progress)
	})

	t.Run("if Total is 0 it refuses to divide by 0 and sets Progress to 100", func(t *testing.T) {
		input := &ingest.MigrationStatus{
			Finished:  false,
			Total:     0,
			Completed: 3,
			Status:    "Doing something",
		}

		s := ingestResponseToServiceMigrationStatus(input)
		assert.Equal(t, int32(100), s.Progress)
	})

	t.Run("if Total is 0 it sets Status to COMPLETE", func(t *testing.T) {
		input := &ingest.MigrationStatus{
			Finished:  false,
			Total:     0,
			Completed: 0,
			Status:    "Doing nothing",
		}

		s := ingestResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_COMPLETE, s.Status)
	})

}

func TestComplianceResponseToServiceMigrationStatus(t *testing.T) {
	t.Run("identifies if the compliance migration failed", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     2,
			Completed: 1,
			Status:    compliance.MigrationStatus_FAILED,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_FAILED, s.Status)
	})

	t.Run("identifies when the migration has successfully completed", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     2,
			Completed: 2,
			Status:    compliance.MigrationStatus_FINISHED,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_COMPLETE, s.Status)
		assert.Equal(t, int32(100), s.Progress)
	})

	t.Run("marks a skipped migration as completed", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     0,
			Completed: 0,
			Status:    compliance.MigrationStatus_SKIPPED,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_COMPLETE, s.Status)
		assert.Equal(t, int32(100), s.Progress)
	})

	t.Run("marks an unknown migration as unknown", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     0,
			Completed: 0,
			Status:    compliance.MigrationStatus_UNKNOWN,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_UNKNOWN, s.Status)
		assert.Equal(t, int32(100), s.Progress)
	})

	t.Run("includes the last log line in info", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     7,
			Completed: 3,
			Status:    compliance.MigrationStatus_RUNNING,
			Logs: []*compliance.LogEntry{
				{
					Text: "doing first thing",
				},
				{
					Text: "doing second thing",
				},
			},
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, s.Status)
		assert.True(t, strings.Contains(s.Info, "doing second thing"))
		assert.Equal(t, int32(42), s.Progress)
	})

	t.Run("calculates the percentage done of an IN_PROGRESS migration", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     7,
			Completed: 3,
			Status:    compliance.MigrationStatus_RUNNING,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, s.Status)
		assert.Equal(t, int32(42), s.Progress)
	})

	t.Run("refuses to divide by 0", func(t *testing.T) {
		input := &compliance.MigrationStatus{
			Total:     0,
			Completed: 3,
			Status:    compliance.MigrationStatus_RUNNING,
		}

		s := complianceResponseToServiceMigrationStatus(input)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, s.Status)
		assert.Equal(t, int32(100), s.Progress)
	})
}

func TestOverallStatus(t *testing.T) {
	t.Run("returns IN_PROGRESS as long as 1 stays is in-progress", func(t *testing.T) {
		actual := overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_IN_PROGRESS,
			},
			{
				Status: api.A1UpgradeStatusResponse_FAILED,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, actual)

		actual = overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_IN_PROGRESS,
			},
			{
				Status: api.A1UpgradeStatusResponse_COMPLETE,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, actual)

		actual = overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_IN_PROGRESS,
			},
			{
				Status: api.A1UpgradeStatusResponse_UNKNOWN,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_IN_PROGRESS, actual)
	})
	t.Run("returns failed if any are failed", func(t *testing.T) {
		actual := overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_COMPLETE,
			},
			{
				Status: api.A1UpgradeStatusResponse_FAILED,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_FAILED, actual)
	})
	t.Run("returns complete if all are complete", func(t *testing.T) {
		actual := overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_COMPLETE,
			},
			{
				Status: api.A1UpgradeStatusResponse_COMPLETE,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_COMPLETE, actual)
	})

	t.Run("returns unknown if one was unknown and none failed", func(t *testing.T) {
		actual := overallStatus([]*api.A1UpgradeStatusResponse_ServiceMigrationStatus{
			{
				Status: api.A1UpgradeStatusResponse_COMPLETE,
			},
			{
				Status: api.A1UpgradeStatusResponse_UNKNOWN,
			},
		}...)
		assert.Equal(t, api.A1UpgradeStatusResponse_UNKNOWN, actual)
	})
}
