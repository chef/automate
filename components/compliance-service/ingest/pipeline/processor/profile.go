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

			reportProfilesShas := make([]string, len(msg.Report.Profiles))
			reportProfilesShasMissingMeta := make(map[string]string, 0)
			msg.Shared.EsProfilesMissingMeta = make(map[string]interface{}, 0)

			for i, profile := range msg.Report.Profiles {
				reportProfilesShas[i] = profile.Sha256
				if profile.Name == "" {
					reportProfilesShasMissingMeta[profile.Sha256] = profile.Title
				}
			}

			esProfilesMissingMeta, err := client.ProfilesMissing(reportProfilesShas)
			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to find profiles missing meta in the backend: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}

			// If a report being ingested is missing the metadata for a profile, ES must have it in the backend, otherwise we can't continue
			for _, esMissingSha := range esProfilesMissingMeta {
				msg.Shared.EsProfilesMissingMeta[esMissingSha] = struct{}{}
				if reportProfilesShasMissingMeta[esMissingSha] != "" {
					grpcErr := status.Errorf(codes.Internal, "Ingesting report '%s' is not possible because profile '%s' with id '%s' does "+
						"not have the metadata known to the backend.", msg.Report.ReportUuid, reportProfilesShasMissingMeta[esMissingSha], esMissingSha)
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}
			}

			// If one of the profiles coming in is missing the metadata, we pull the metadata from the backend for it to complement the report
			if len(reportProfilesShasMissingMeta) > 0 {
				// Pull the profiles metadata from the backend
				for _, profile := range msg.Report.Profiles {
					if profile.Name == "" {

						//HEREEEEEEEEE

					}
				}
			}

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
