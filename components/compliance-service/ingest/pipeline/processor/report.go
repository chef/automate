package processor

import (
	"context"
	"fmt"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	cmpReport "github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/notifications-client/builder"
	"github.com/chef/automate/components/notifications-client/notifier"
)

func ComplianceReport(notifierClient notifier.Notifier, automateURL string) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return complianceReport(in, notifierClient, automateURL)
	}
}

func complianceReport(in <-chan message.Compliance, notifierClient notifier.Notifier, automateURL string) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessingCompliance(err)
				continue
			}
			err := handleNotifications(msg.Ctx, notifierClient, &msg.Report, automateURL)
			if err != nil {
				logrus.Errorf("ProcessComplianceReport unable to send notification: %s", err.Error())
			}

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
				StatusMessage:    msg.Shared.StatusMessage,
				InSpecVersion:    msg.Report.Version,
				DocVersion:       compliance.DocVersion,
				ESTimestamp:      compliance.CurrentTime(),
				PolicyName:       msg.Report.PolicyName,
				PolicyGroup:      msg.Report.PolicyGroup,
				OrganizationName: msg.Report.OrganizationName,
				SourceFQDN:       msg.Report.SourceFqdn,
				ChefTags:         msg.Report.ChefTags,
				FQDN:             msg.Report.Fqdn,
				RunTimeLimit:     msg.Report.RunTimeLimit,
			}

			copyOfIpAddress := msg.Report.Ipaddress
			// Elastic won't accept empty string for a field of data type 'ip'
			if copyOfIpAddress == "" {
				msg.InspecReport.IPAddress = nil
			} else {
				msg.InspecReport.IPAddress = &copyOfIpAddress
			}
			msg.InspecReport.Platform.Name = msg.Report.GetPlatform().GetName()
			msg.InspecReport.Platform.Release = msg.Report.GetPlatform().GetRelease()
			msg.InspecReport.Statistics.Duration = msg.Report.GetStatistics().GetDuration()
			msg.InspecReport.Platform.Full = fmt.Sprintf("%s %s",
				msg.InspecReport.Platform.Name, msg.InspecReport.Platform.Release)
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processed Compliance Report")
			message.Propagate(out, &msg)
		}
		close(out)
	}()
	return out
}
func handleNotifications(ctx context.Context, notifierClient notifier.Notifier, report *cmpReport.Report, automateURL string) error {
	// now that we've glued the report information we ingested together with the profile information and re-created
	// a full report, send it off to notifications service
	logrus.Debugf("Calling handleNotifications for report id %s", report.ReportUuid)
	if notifierClient == nil {
		return fmt.Errorf("no notifier client found. unable to send notification for report_id %s", report.ReportUuid)
	}

	ev, err := builder.Compliance(automateURL, report)
	if err != nil {
		// We treat notification errors as non fatal
		logrus.WithFields(logrus.Fields{"id": report.ReportUuid}).Warnf("Could not build notifications InSpec event: %v", err)
	} else {
		// This happens asynchronously
		notifierClient.Send(ctx, ev)
	}
	return nil
}
