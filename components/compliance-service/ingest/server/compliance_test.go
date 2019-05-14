package server

import (
	"testing"

	"github.com/chef/automate/components/nodemanager-service/api/nodes"

	ingest_inspec "github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/stretchr/testify/assert"
)

func TestGetReportStatus(t *testing.T) {
	profiles := []*ingest_inspec.Profile{
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "skipped"},
					},
				},
			}}}
	assert.Equal(t, nodes.LastContactData_PASSED, getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "failed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-3",
					Results: []*ingest_inspec.Result{
						{Status: "skipped"},
					},
				},
			}}}
	assert.Equal(t, nodes.LastContactData_FAILED, getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{Status: "skipped"},
	}
	assert.Equal(t, nodes.LastContactData_SKIPPED, getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{Status: "skipped"},
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "skipped"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "skipped"},
					},
				},
			},
		},
	}
	assert.Equal(t, nodes.LastContactData_SKIPPED, getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "skipped"},
					},
				},
			},
		},
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
					},
				},
			},
		},
	}
	assert.Equal(t, nodes.LastContactData_PASSED, getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "failed"},
					},
				},
			},
		},
		{
			Controls: []*ingest_inspec.Control{
				{
					Id: "test-1",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
						{Status: "skipped"},
					},
				},
				{
					Id: "test-2",
					Results: []*ingest_inspec.Result{
						{Status: "passed"},
					},
				},
			},
		},
	}
	assert.Equal(t, nodes.LastContactData_FAILED, getReportStatus(profiles))
}
