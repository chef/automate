package server

import (
	"testing"

	ingest_inspec "github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/stretchr/testify/assert"
)

func TestGetReportStatus(t *testing.T) {
	profiles := []*ingest_inspec.Profile{
		{Status: "passed"},
		{Status: "skipped"},
	}
	assert.Equal(t, "passed", getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{Status: "passed"},
		{Status: "failed"},
		{Status: "skipped"},
	}
	assert.Equal(t, "failed", getReportStatus(profiles))

	profiles = []*ingest_inspec.Profile{
		{Status: "skipped"},
		{Status: "skipped"},
	}
	assert.Equal(t, "skipped", getReportStatus(profiles))

}
