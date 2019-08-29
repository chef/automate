package processor

import (
	"fmt"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func ComplianceReport(in <-chan message.Compliance) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processing Compliance Report")
			parsedTime, err := time.Parse(time.RFC3339, msg.Report.EndTime)
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to Parse end_time: %s", err)
				logrus.Errorf("Unable to Parse end_time: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}
			msg.InspecReport = &relaxting.ESInSpecReport{
				NodeID:           msg.Report.NodeUuid,
				NodeName:         msg.Report.NodeName,
				JobID:            msg.Report.JobUuid,
				Environment:      msg.Report.Environment,
				EndTime:          parsedTime,
				Roles:            msg.Report.Roles,
				Recipes:          msg.Report.Recipes,
				ControlsSums:     *msg.Shared.AllProfileSums,
				Profiles:         compliance.ReportProfilesFromInSpecProfiles(msg.Report.Profiles, msg.Shared.PerProfileSums),
				Status:           msg.Shared.Status,
				InSpecVersion:    msg.Report.Version,
				DocVersion:       compliance.DocVersion,
				ESTimestamp:      compliance.CurrentTime(),
				PolicyName:       msg.Report.PolicyName,
				PolicyGroup:      msg.Report.PolicyGroup,
				OrganizationName: msg.Report.OrganizationName,
				SourceFQDN:       msg.Report.SourceFqdn,
				ChefTags:         msg.Report.ChefTags,
				IPAddress:        &msg.Report.Ipaddress,
				FQDN:             msg.Report.Fqdn,
			}
			// Elastic won't accept empty string for a field of data type 'ip'
			if *msg.InspecReport.IPAddress == "" {
				msg.InspecReport.IPAddress = nil
			}
			msg.InspecReport.Platform.Name = msg.Report.GetPlatform().GetName()
			msg.InspecReport.Platform.Release = msg.Report.GetPlatform().GetRelease()
			msg.InspecReport.Statistics.Duration = msg.Report.GetStatistics().GetDuration()
			msg.InspecReport.Platform.Full = fmt.Sprintf("%s %s", msg.InspecReport.Platform.Name, msg.InspecReport.Platform.Release)
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processed Compliance Report")
			out <- msg
		}
		close(out)
	}()
	return out
}
