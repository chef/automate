package processor

import (
	"strings"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/ptypes/struct"
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

			if err != nil {
				grpcErr := status.Errorf(codes.Internal, "Unable to find profiles missing meta in the backend: %s", err)
				msg.FinishProcessingCompliance(grpcErr)
				continue
			}

			// If a report being ingested is missing the metadata for a profile, ES must have it in the backend, otherwise we can't continue
			for _, esMissingSha := range esProfilesMissingMeta {
				msg.Shared.EsProfilesMissingMeta[esMissingSha] = struct{}{}
				if reportProfilesShasMissingMetaMap[esMissingSha] != "" {
					grpcErr := status.Errorf(codes.Internal, "Ingesting report '%s' is not possible because profile '%s' with id '%s' does "+
						"not have the metadata known to the backend.", msg.Report.ReportUuid, reportProfilesShasMissingMetaMap[esMissingSha], esMissingSha)
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}
			}

			// If one of the profiles coming in is missing the metadata, we pull the metadata from the backend for it to complement the report
			if len(reportProfilesShasMissingMetaArr) > 0 {
				// Pull the profiles metadata from the backend
				esProfilesForMissingMeta, err := client.GetProfilesMissingMetadata(reportProfilesShasMissingMetaArr)
				if err != nil {
					grpcErr := status.Errorf(codes.Internal, "Unable to get the profiles missing metadata: %s", err)
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}
				for _, repProfile := range msg.Report.Profiles {
					if repProfile.Name == "" {
						esProfile := esProfilesForMissingMeta[repProfile.Sha256]
						if esProfile == nil {
							grpcErr := status.Errorf(codes.Internal, "Unable to find profile '%s' metadata in the profiles store.", repProfile.Sha256)
							msg.FinishProcessingCompliance(grpcErr)
							continue
						}
						repProfile.Name = esProfile.Name

						esControlsHash := make(map[string]relaxting.ESInspecControl, len(esProfile.Controls))
						for _, esControl := range esProfile.Controls {
							esControlsHash[esControl.ID] = esControl
						}
						for _, repControl := range repProfile.Controls {
							if val, ok := esControlsHash[repControl.Id]; ok {
								repControl.Title = val.Title
								repControl.Impact = val.Impact
								//repControl.Refs = val.Refs

								var controlTags structpb.Struct
								err := (&jsonpb.Unmarshaler{}).Unmarshal(strings.NewReader(val.Tags), &controlTags)
								if err == nil {
									repControl.Tags = &controlTags
								}
								var controlRefs structpb.Struct
								err = (&jsonpb.Unmarshaler{}).Unmarshal(strings.NewReader(val.Refs), &controlRefs)
								if err == nil {
									//repControl.Refs = &controlRefs
									logrus.Debugf("!!! %s controlRefs=%+v", repControl.Id, controlRefs)
								} else {
									logrus.Debugf("!!! %s didn't work for controlRefs=%+v", repControl.Id, controlRefs)
								}

							} else {
								grpcErr := status.Errorf(codes.Internal, "Unable to find control '%s' in ES profile '%s' (%s)", repControl.Id, repProfile.Title, repProfile.Sha256)
								msg.FinishProcessingCompliance(grpcErr)
								continue
							}
						}
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
