package integration_test

import (
	"io/ioutil"
	"testing"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/compliance-service/ingest/server"
	"github.com/golang/mock/gomock"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/stretchr/testify/require"
)

func TestInjectReportWithLCR(t *testing.T) {
	reportFileName := "../ingest/examples/compliance-failure-big-report.json"
	s := suite

	reportStoreMock := report_manager.NewMockReportManagerService_StoreReportClient(gomock.NewController(t))
	reportStoreMock.EXPECT().Send(gomock.Any()).AnyTimes().Return(nil)
	reportStoreMock.EXPECT().CloseAndRecv().MaxTimes(3).Return(nil, nil)
	s.ReportServiceClientMock = report_manager.NewMockReportManagerServiceClient(gomock.NewController(t))
	s.ReportServiceClientMock.EXPECT().StoreReport(gomock.Any(), gomock.Any()).AnyTimes().Return(
		reportStoreMock, nil)

	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, s.ReportServiceClientMock, "", s.NotifierMock,
		s.ProjectsClientMock, 100, true)
	body, err := ioutil.ReadFile(reportFileName)
	require.NoError(t, err)
	err = s.ingestReport(reportFileName, func(r *compliance.Report) {
		r = gateway.ParseBytesToComplianceReport(body)
		r.ReportUuid = newUUID()
	})
	require.NoError(t, err)
}
