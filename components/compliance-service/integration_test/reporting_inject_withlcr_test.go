package integration_test

import (
	"github.com/chef/automate/components/compliance-service/ingest/server"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"

	"github.com/stretchr/testify/require"
)

func TestInjectReportWithLCR(t *testing.T) {
	reportFileName := "../ingest/examples/compliance-failure-big-report.json"
	s := suite
	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, "", s.NotifierMock,
		s.ProjectsClientMock, 100, true)
	err := s.ingestReport(reportFileName, func(r *compliance.Report) {
		id := newUUID()

		r.Environment = id
		r.NodeName = id
		r.NodeUuid = id
		r.Platform.Name = id
		r.Profiles[0].Controls = r.Profiles[0].Controls[2:3]
		r.Profiles[0].Controls[0].Id = id
		r.Profiles[0].Controls[0].Title = id
		r.Profiles = r.Profiles[:1]
		r.Profiles[0].Sha256 = id
		r.Profiles[0].Title = id
		r.Recipes = []string{id}
		r.ReportUuid = id
		r.Roles = []string{id}
	})
	require.NoError(t, err)
}
