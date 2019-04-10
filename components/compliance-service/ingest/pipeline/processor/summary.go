package processor

import (
	"fmt"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func ComplianceShared(in <-chan message.Compliance) <-chan message.Compliance {
	var err error
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			dl, _ := msg.Ctx.Deadline()
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "ctx_dealine": dl.UTC().Format(time.RFC3339)}).Info("Processing ComplianceShared")

			if msg.Report.Platform == nil {
				msg.Report.Platform = &inspec.Platform{}
			}
			if msg.Report.Platform.Name == "" {
				msg.Report.Platform.Name = "unknown"
			}
			if msg.Report.Platform.Release == "" {
				msg.Report.Platform.Release = "unknown"
			}

			msg.Report.Profiles = compliance.FixInheritedProfiles(msg.Report.Profiles)

			perProfileSums := make([]relaxting.ESInSpecSummaryProfile, 0)
			totalSum := &reportingTypes.NodeControlSummary{}
			for _, profile := range msg.Report.Profiles {
				sum := *compliance.ProfileControlSummary(profile)
				profileStatus := profile.Status
				if profileStatus == "" || profileStatus == inspec.ResultStatusLoaded {
					profileStatus = compliance.ReportComplianceStatus(&sum)
				}
				logrus.WithFields(logrus.Fields{"ReportUuid": msg.Report.ReportUuid, "profile_status": profileStatus}).Debug("profileStatus yey!")

				perProfileSums = append(perProfileSums, relaxting.ESInSpecSummaryProfile{
					Profile:      compliance.NameSha(profile),
					Name:         profile.Name,
					SHA256:       profile.Sha256,
					Title:        profile.Title,
					Version:      profile.Version,
					Status:       profileStatus,
					ControlsSums: sum,
				})

				compliance.AddControlSummary(totalSum, sum)
			}
			msg.Shared.PerProfileSums = perProfileSums
			msg.Shared.AllProfileSums = totalSum
			msg.Shared.Status = compliance.ReportComplianceStatus(totalSum)
			msg.Shared.EndTime, err = time.Parse(time.RFC3339, msg.Report.EndTime)
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to Parse end_time: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "total_sum": totalSum}).Debug("Finished ComplianceShared")

			out <- msg
		}
		close(out)
	}()
	return out
}

func ComplianceSummary(in <-chan message.Compliance) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Info("Processing Compliance Summary")
			msg.InspecSummary = &relaxting.ESInSpecSummary{
				NodeID:           msg.Report.NodeUuid,
				NodeName:         msg.Report.NodeName,
				JobID:            msg.Report.JobUuid,
				Environment:      msg.Report.Environment,
				EndTime:          msg.Shared.EndTime,
				Roles:            msg.Report.Roles,
				Recipes:          msg.Report.Recipes,
				ControlsSums:     *msg.Shared.AllProfileSums,
				Profiles:         msg.Shared.PerProfileSums,
				Status:           msg.Shared.Status,
				DocVersion:       compliance.DocVersion,
				ESTimestamp:      compliance.CurrentTime(),
				PolicyName:       msg.Report.PolicyName,
				PolicyGroup:      msg.Report.PolicyGroup,
				OrganizationName: msg.Report.OrganizationName,
				SourceFQDN:       msg.Report.SourceFqdn,
				ChefTags:         msg.Report.ChefTags,
			}
			msg.InspecSummary.Platform.Name = msg.Report.Platform.Name
			msg.InspecSummary.Platform.Release = msg.Report.Platform.Release
			msg.InspecSummary.Platform.Full = fmt.Sprintf("%s %s", msg.Report.Platform.Name, msg.Report.Platform.Release)
			msg.InspecSummary.InSpecVersion = msg.Report.Version
			msg.InspecSummary.Statistics.Duration = msg.Report.Statistics.Duration
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "profiles": msg.InspecSummary.Profiles}).Debug("Processed Compliance Summary")
			out <- msg
		}
		close(out)
	}()
	return out
}
