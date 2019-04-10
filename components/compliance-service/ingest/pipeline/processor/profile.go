package processor

import (
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func ComplianceProfile(in <-chan message.Compliance) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Info("Processing Compliance Profile")
			var err error
			msg.InspecProfiles, err = compliance.ProfilesFromReport(msg.Report.Profiles)

			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to parse profiles from report: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processed Compliance Profile")

			out <- msg
		}
		close(out)
	}()
	return out
}
