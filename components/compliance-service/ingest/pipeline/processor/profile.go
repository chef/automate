package processor

import (
	"strings"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/golang/protobuf/jsonpb"
	structpb "github.com/golang/protobuf/ptypes/struct"
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
	out := make(chan message.Compliance, 100)
	go func() {
		for msg := range in {
			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processing Compliance Profile")
			var err error

			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessingCompliance(err)
				continue
			}

			reportProfilesShas := make([]string, len(msg.Report.Profiles))
			reportProfilesShasMissingMetaMap := make(map[string]string, 0)
			reportProfilesShasMissingMetaArr := make([]string, 0)
			msg.Shared.EsProfilesMissingMeta = make(map[string]interface{}, 0)

			for i, profile := range msg.Report.Profiles {
				reportProfilesShas[i] = profile.Sha256
				if profile.Name == "" {
					reportProfilesShasMissingMetaArr = append(reportProfilesShasMissingMetaArr, profile.Sha256)
					reportProfilesShasMissingMetaMap[profile.Sha256] = profile.Title
				}
			}

			esProfilesMissingMeta, err := client.ProfilesMissing(reportProfilesShas)

			var grpcErr error
			if err != nil {
				grpcErr = status.Errorf(codes.Internal, "Unable to find profiles missing meta in the backend: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}

			// If a report being ingested is missing the metadata for a profile, ES must have it in the backend, otherwise we can't continue
			for _, esMissingSha := range esProfilesMissingMeta {
				msg.Shared.EsProfilesMissingMeta[esMissingSha] = struct{}{}
				if reportProfilesShasMissingMetaMap[esMissingSha] != "" {
					grpcErr = status.Errorf(codes.Internal, "Ingesting report '%s' is not possible because profile '%s' with id '%s' does "+
						"not have the metadata known to the backend.", msg.Report.ReportUuid, reportProfilesShasMissingMetaMap[esMissingSha], esMissingSha)
					// Now 'break' from the esProfilesMissingMeta loop and 'continue' to the next message from the 'in' channel
					break
				}
			}

			if grpcErr != nil {
				// 'continue' to the next message from the 'in' channel
				// FinishProcessingCompliance can only be called once for a msg as it closes the Done channel
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}

			// If one of the profiles coming in is missing the metadata, we pull the metadata from the backend for it to complement the report
			if len(reportProfilesShasMissingMetaArr) > 0 {
				// Pull the minimum required profiles metadata from the backend
				esProfilesForMissingMeta, err := client.GetProfilesMissingMetadata(reportProfilesShasMissingMetaArr)
				if err != nil {
					grpcErr = status.Errorf(codes.Internal, "Unable to get the profiles missing metadata: %s", err)
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}

				// Loop through all report profiles to find the ones missing metadata
				for _, repProfile := range msg.Report.Profiles {
					// With profile 'name' being a require property, a profile missing it indicates that metadata stripping has
					// occurred. We need to complement it now with backend data to ensure a complete ingestion
					if repProfile.Name == "" {
						esProfile := esProfilesForMissingMeta[repProfile.Sha256]
						if esProfile == nil {
							grpcErr = status.Errorf(codes.Internal, "Unable to find profile '%s' metadata in the profiles store.", repProfile.Sha256)
							break
						}

						repProfile.Name = esProfile.Name

						// Making a hash with the controls coming from ElasticSearch for quicker lookup below
						esControlsHash := make(map[string]relaxting.ESInspecControl, len(esProfile.Controls))
						for _, esControl := range esProfile.Controls {
							esControlsHash[esControl.ID] = esControl
						}

						// Complement report controls with metadata from ElasticSearch
						for _, repControl := range repProfile.Controls {
							if esControl, ok := esControlsHash[repControl.Id]; ok {
								repControl.Title = esControl.Title
								repControl.Impact = esControl.Impact

								var controlTags structpb.Struct
								err := (&jsonpb.Unmarshaler{}).Unmarshal(strings.NewReader(esControl.Tags), &controlTags)
								if err == nil {
									repControl.Tags = &controlTags
								}
							} else {
								grpcErr = status.Errorf(codes.Internal, "Unable to find control '%s' in ES profile '%s' (%s)", repControl.Id, repProfile.Title, repProfile.Sha256)
								break
							}
						}
						if grpcErr != nil {
							break
						}
					}
				}
				if grpcErr != nil {
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}
			}

			msg.InspecProfiles, err = compliance.ProfilesFromReport(msg.Report.Profiles)

			if err != nil {
				grpcErr = status.Errorf(codes.Internal, "Unable to parse profiles from report: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}

			msg.Report.Profiles = compliance.FixInheritedProfiles(msg.Report.Profiles)

			logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Processed Compliance Profile")
			message.Propagate(out, &msg)

		}
		close(out)
	}()
	return out
}
