package processor

import (
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func ComplianceProfile(client *ingestic.ESClient) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return complianceProfile(in, client)
	}
}

func complianceProfile(in <-chan message.Compliance, client *ingestic.ESClient) <-chan message.Compliance {
	//ctx := context.Background()
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processing Compliance Profile")
			var err error

			msg.Report.Profiles = compliance.FixInheritedProfiles(msg.Report.Profiles)

			// bring here profile and compliment reports

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
