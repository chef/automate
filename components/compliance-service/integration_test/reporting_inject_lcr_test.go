package integration_test

import (
	"errors"
	"io/ioutil"
	"testing"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/compliance-service/ingest/server"
	"github.com/golang/mock/gomock"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/stretchr/testify/assert"
)

func TestInjectLCR(t *testing.T) {
	s := suite

	reportStoreMock := report_manager.NewMockReportManagerService_StoreReportClient(gomock.NewController(t))
	reportStoreMock.EXPECT().Send(gomock.Any()).AnyTimes().Return(nil)
	reportStoreMock.EXPECT().CloseAndRecv().MaxTimes(3).Return(nil, nil)
	s.ReportServiceClientMock = report_manager.NewMockReportManagerServiceClient(gomock.NewController(t))
	s.ReportServiceClientMock.EXPECT().StoreReport(gomock.Any(), gomock.Any()).AnyTimes().Return(
		reportStoreMock, nil)

	err := ingestLCR(s)
	assert.NoError(t, err, "expected error doesn't match")
}

func TestInjectLCRWithSendError(t *testing.T) {
	s := suite
	returnErr := "rpc error: code = Internal desc = Unable to send report stream: this a test error"

	reportStoreMock := report_manager.NewMockReportManagerService_StoreReportClient(gomock.NewController(t))
	reportStoreMock.EXPECT().Send(gomock.Any()).AnyTimes().Return(errors.New("this a test error"))
	reportStoreMock.EXPECT().CloseAndRecv().MaxTimes(3).Return(nil, nil)
	s.ReportServiceClientMock = report_manager.NewMockReportManagerServiceClient(gomock.NewController(t))
	s.ReportServiceClientMock.EXPECT().StoreReport(gomock.Any(), gomock.Any()).AnyTimes().Return(
		reportStoreMock, nil)

	err := ingestLCR(s)
	assert.EqualError(t, err, returnErr, "expected error doesn't match")
}

func TestInjectLCRWithStoreReportError(t *testing.T) {
	s := suite
	returnErr := "rpc error: code = Internal desc = Unable to get report stream: this a test error"

	reportStoreMock := report_manager.NewMockReportManagerService_StoreReportClient(gomock.NewController(t))
	reportStoreMock.EXPECT().Send(gomock.Any()).AnyTimes().Return(nil)
	reportStoreMock.EXPECT().CloseAndRecv().MaxTimes(3).Return(nil, nil)
	s.ReportServiceClientMock = report_manager.NewMockReportManagerServiceClient(gomock.NewController(t))
	s.ReportServiceClientMock.EXPECT().StoreReport(gomock.Any(), gomock.Any()).AnyTimes().Return(
		reportStoreMock, errors.New("this a test error"))

	err := ingestLCR(s)
	assert.EqualError(t, err, returnErr, "expected error doesn't match")
}

func ingestLCR(s *Suite) error {
	fileName := "../ingest/examples/compliance-failure-big-report.json"
	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, s.ReportServiceClientMock, "", s.NotifierMock,
		s.ProjectsClientMock, 100, true)
	body, err := ioutil.ReadFile(fileName)
	if err != nil {
		return err
	}
	err = s.ingestReport(fileName, func(r *compliance.Report) {
		r = gateway.ParseBytesToComplianceReport(body)
		r.ReportUuid = newUUID()
	})
	return err
}
