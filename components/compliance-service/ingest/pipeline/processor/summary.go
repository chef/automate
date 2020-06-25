package processor

import (
	"fmt"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func ComplianceShared(in <-chan message.Compliance) <-chan message.Compliance {
	var err error
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessingCompliance(err)
				continue
			}

			dl, _ := msg.Ctx.Deadline()
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "ctx_dealine": dl.UTC().Format(time.RFC3339)}).Debug("Processing ComplianceShared")

			if msg.Report.Platform == nil {
				msg.Report.Platform = &inspec.Platform{}
			}
			if msg.Report.Platform.Name == "" {
				msg.Report.Platform.Name = "unknown"
			}
			if msg.Report.Platform.Release == "" {
				msg.Report.Platform.Release = "unknown"
			}

			perProfileSums := make([]relaxting.ESInSpecSummaryProfile, 0)
			totalSum := &reportingTypes.NodeControlSummary{}

			skippedProfiles := 0
			failedProfiles := 0

			for _, profile := range msg.Report.Profiles {
				sum := *compliance.ProfileControlSummary(profile)
				profileStatus := profile.Status

				// This is profile status as it comes in, not calculated one based on control results statues
				// status of "" is legacy, "loaded" is the current status of a profile that was loaded and executed.
				if profileStatus == "" || profileStatus == inspec.ResultStatusLoaded {
					profileStatus = compliance.ReportComplianceStatus(&sum)
				} else if profileStatus == inspec.ResultStatusSkipped {
					skippedProfiles += 1
				} else if profileStatus == inspec.ResultStatusFailed {
					failedProfiles += 1
				}

				logrus.WithFields(logrus.Fields{"ReportUuid": msg.Report.ReportUuid, "profile_status": profileStatus}).Debug("profileStatus yey!")

				perProfileSums = append(perProfileSums, relaxting.ESInSpecSummaryProfile{
					Profile:      compliance.NameSha(profile),
					Name:         profile.Name,
					SHA256:       profile.Sha256,
					Title:        profile.Title,
					Version:      profile.Version,
					Full:         fmt.Sprintf("%s, v%s", profile.Title, profile.Version),
					Status:       profileStatus,
					ControlsSums: sum,
				})

				compliance.AddControlSummary(totalSum, sum)
			}
			msg.Shared.PerProfileSums = perProfileSums
			msg.Shared.AllProfileSums = totalSum

			if msg.Report.Status == "failed" && len(msg.Report.Profiles) == 0 {
				msg.Shared.Status = msg.Report.Status
				msg.Shared.StatusMessage = msg.Report.StatusMessage
				logrus.Warn("Report status is failed and there are not profiles")
			} else if failedProfiles > 0 {
				// If at least one of the profiles in the report comes in with "failed" status, we ignore the totalSum aggregation which might have all
				// controls passed from another profile. This way we flag the failure of a profile without running any controls, e.g. profile parse error.
				msg.Shared.Status = inspec.ResultStatusFailed
			} else if skippedProfiles > 0 && skippedProfiles == len(msg.Report.Profiles) {
				// If the ingested report has the status of "skipped" for all profiles, we mark the report as "skipped"
				msg.Shared.Status = inspec.ResultStatusSkipped
			} else if len(msg.Report.Profiles) == 0 {
				// If the report has no profiles at all, InSpec most likely threw a runtime exception
				msg.Shared.Status = inspec.ResultStatusFailed
			} else {
				// Otherwise, we leave the summary of all profiles in the report to decide the overall status
				msg.Shared.Status = compliance.ReportComplianceStatus(totalSum)
			}

			msg.Shared.EndTime, err = time.Parse(time.RFC3339, msg.Report.EndTime)
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to Parse end_time: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "total_sum": totalSum}).Debug("Finished ComplianceShared")

			message.Propagate(out, &msg)
		}
		close(out)
	}()
	return out
}

func ComplianceSummary(in <-chan message.Compliance) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessingCompliance(err)
				continue
			}
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processing Compliance Summary")
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
				StatusMessage:    msg.Shared.StatusMessage,
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
			message.Propagate(out, &msg)
		}
		close(out)
	}()
	return out
}
