package compliance

import (
	"context"
	"net/url"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/notifications-client/notifier"

	gp "github.com/golang/protobuf/ptypes/empty"
)

type ComplianceIngestServer struct {
	ingesterClient ingest.ComplianceIngesterClient
	notifierClient *notifier.Notifier
	automateUrl    string
}

func NewComplianceIngestServer(automateUrl *url.URL, ingesterClient ingest.ComplianceIngesterClient, notifierClient *notifier.Notifier) *ComplianceIngestServer {
	return &ComplianceIngestServer{
		ingesterClient: ingesterClient,
		notifierClient: notifierClient,
		automateUrl:    automateUrl.String(),
	}
}

func (s *ComplianceIngestServer) ProcessComplianceReport(ctx context.Context, report *compliance.Report) (*gp.Empty, error) {
	return s.ingesterClient.ProcessComplianceReport(ctx, report)
}
