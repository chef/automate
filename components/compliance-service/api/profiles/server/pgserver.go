package profiles

import (
	"bytes"
	"context"
	"io"
	"io/ioutil"
	"os"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/compliance/profiles"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/api/profiles/conversion"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/inspec"
	dbstore "github.com/chef/automate/components/compliance-service/profiles/db"
	"github.com/chef/automate/components/compliance-service/profiles/market"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	event "github.com/chef/automate/components/event-service/config"
)

// PGProfileServer implements the profile store GRPC interface
type PGProfileServer struct {
	es                  *relaxting.ES2Backend
	esClient            *ingestic.ESClient
	profiles            *config.Profiles
	store               *dbstore.Store
	eventsClient        automate_event.EventServiceClient
	firejailProfilePath string
}

func (srv *PGProfileServer) convertProfileToTgz(reader io.ReadCloser, contentType string) (string, error) {
	var tmpWithSuffix string
	switch contentType {
	case "application/gzip", "application/x-gzip", "application/x-gtar", "application/x-tar; charset=gzip":
		var err error
		// we are good, nothing to do
		tmpWithSuffix, err = market.TempUpload(reader, ".tar.gz")
		if err != nil {
			return "", err
		}
	case "application/zip", "application/x-zip-compressed":
		// need to convert zip to tar.gz
		logrus.Debug("convert zip archive to tar.gz")
		tmpZipUpload, err := market.TempUpload(reader, ".zip")
		defer func() {
			if logrus.GetLevel() < 5 {
				err := os.Remove(tmpZipUpload)
				if err != nil {
					logrus.Errorf("could not delete the cache file %s: %s", tmpZipUpload, err.Error())
				}
			} else {
				logrus.Debugf("Did not delete %s for troubleshooting purposes", tmpZipUpload)
			}
		}()
		if err != nil {
			return "", err
		}

		tmpWithSuffix, err = market.TempFileWithSuffix(".tar.gz")
		if err != nil {
			return "", err
		}

		err = util.ConvertZipToTarGz(tmpZipUpload, tmpWithSuffix, srv.firejailProfilePath)
		if err != nil {
			return "", err
		}
	}
	return tmpWithSuffix, nil
}

func (srv *PGProfileServer) storeProfile(owner string, cacheFile string) (inspec.CheckResult, error) {
	var inspecCheckResult inspec.CheckResult
	// Run InSpec check
	inspecCheckResult, err := market.CheckProfile(cacheFile, srv.firejailProfilePath)
	if err != nil {
		logrus.Errorf("Create CheckProfile error: %s", err.Error())
		inspecCheckResult.Summary.Valid = false
		inspecCheckResult.Errors = []inspec.CheckMessage{{Msg: err.Error()}}
		return inspecCheckResult, status.Error(codes.InvalidArgument, err.Error())
	}

	sha256, tar, info, err := srv.store.GetProfileInfo(cacheFile, srv.firejailProfilePath)
	if err != nil {
		logrus.Errorf("Create GetProfileInfo error: %s", err.Error())
		inspecCheckResult.Summary.Valid = false
		inspecCheckResult.Errors = []inspec.CheckMessage{{Msg: err.Error()}}
		return inspecCheckResult, status.Error(codes.Internal, err.Error())
	}

	// send the profile json to reporting component
	esInfo := inspec.Profile{}
	err = esInfo.FromJSON(info)
	if err != nil {
		logrus.Errorf("could not parse json: %s", err.Error())
		inspecCheckResult.Summary.Valid = false
		inspecCheckResult.Errors = []inspec.CheckMessage{{Msg: err.Error()}}
		return inspecCheckResult, status.Error(codes.Internal, err.Error())
	}

	err = srv.es.StoreProfile(esInfo)
	if err != nil {
		logrus.Errorf("could not store profile information in elastic: %s", err.Error())
		inspecCheckResult.Summary.Valid = false
		inspecCheckResult.Errors = []inspec.CheckMessage{{Msg: err.Error()}}
		return inspecCheckResult, status.Error(codes.Internal, err.Error())
	}

	// store profile in profile store
	err = srv.store.UploadProfile(sha256, owner, tar, info)
	if err != nil {
		logrus.Errorf("Create StoreProfile error: %s", err.Error())
		return inspecCheckResult, status.Error(codes.Internal, err.Error())
	}

	// fire profile created event
	go srv.fireEvent(event.ProfileCreatedEventName, owner, esInfo.Name, esInfo.Version)

	return inspecCheckResult, nil
}

func (srv *PGProfileServer) Create(stream profiles.ProfilesService_CreateServer) error {
	in, err := stream.Recv()
	if err != io.EOF && err != nil {
		return err
	}

	// ensure that metadata is set
	if in.GetMeta() == nil {
		return status.Error(codes.InvalidArgument, "meta must be specified for profile upload")
	}

	// fallback if no content type is set
	if len(in.GetMeta().ContentType) == 0 {
		in.GetMeta().ContentType = "application/x-gtar"
	}

	if in.Owner == "" {
		return status.Error(codes.InvalidArgument, "owner must be specified for profile upload.")
	}

	// verify that we accept the content
	logrus.Debugf("Content Type %s", in.GetMeta().ContentType)
	var reader io.ReadCloser
	if in.GetMeta().ContentType == "application/json" || in.GetMeta().ContentType == "application/json+lax" {
		namespace := in.Owner
		name := in.GetMeta().Name
		version := in.GetMeta().Version
		if len(namespace) == 0 && len(name) == 0 {
			logrus.Errorf("Could not understand the given request")
			return status.Error(codes.InvalidArgument, "no namespace given")
		}
		logrus.Infof("Install market profile %s into %s", name, namespace)
		// Verify that the profile exists
		_, err := srv.store.ReadMarket(name, version)
		if err != nil {
			// profile does not exist
			return status.Error(codes.NotFound, "could not find profile")
		}

		// install the profile into the namespace
		err = srv.store.InstallMarket(namespace, name, version)
		if err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		// fire profile created event
		go srv.fireEvent(event.ProfileCreatedEventName, in.Owner, in.Meta.Name, in.Meta.Version)

		// send an empty result, since it is not relevant for marketplace installation
		res := &profiles.CheckResult{}
		res.Summary = &profiles.ResultSummary{}
		res.Summary.Valid = true
		err = stream.SendAndClose(res)
		if err != nil {
			logrus.Error(err)
			return err
		}
	} else {
		// get reader
		reader = ioutil.NopCloser(bytes.NewReader(in.GetChunk().Data))
		cacheFile, err := srv.convertProfileToTgz(reader, in.GetMeta().ContentType)
		if err != nil {
			logrus.Errorf("error during zip to tar.gz conversion: %s", err.Error())
			return status.Error(codes.Internal, err.Error())
		}
		defer func() {
			if logrus.GetLevel() < 5 {
				err := os.Remove(cacheFile)
				if err != nil {
					logrus.Errorf("could not delete the cache file %s: %s", cacheFile, err.Error())
				}
			} else {
				logrus.Debugf("did not delete %s for troubleshooting purposes", cacheFile)
			}
		}()

		inspecCheckResult, err := srv.storeProfile(in.Owner, cacheFile)
		if err != nil {
			return err
		}

		checkResult := inspecCheckResultToGrpcCheckResult(inspecCheckResult)
		err = stream.SendAndClose(&checkResult)
		if err != nil {
			logrus.Error(err)
			return err
		}
	}

	return nil
}

func (srv *PGProfileServer) Read(ctx context.Context, in *profiles.ProfileDetails) (*profiles.Profile, error) {
	logrus.Infof("Reading profile: %+v", in)
	var profile *inspec.Profile
	var err error
	if in.Owner == "" {
		// market profile
		profile, err = srv.store.ReadMarket(in.Name, in.Version)
	} else {
		// namespaced profile
		profile, err = srv.store.Read(in.Owner, in.Name, in.Version)
	}

	if err != nil {
		logrus.Error(err)
		return nil, status.Error(codes.NotFound, "could not determine profile information")
	}

	p, err := conversion.ConvertToPSProfile(*profile, in.Owner)
	if err != nil {
		logrus.Error(err)
		return nil, status.Error(codes.Internal, "could not generate response")
	}
	return &p, nil
}

func (srv *PGProfileServer) ReadTar(in *profiles.ProfileDetails, stream profiles.ProfilesService_ReadTarServer) (err error) {
	logrus.Infof("Reading profile tar: %+v", in)
	var tarBlob []byte
	if len(in.Owner) > 0 {
		tarBlob, err = srv.store.ReadTar(in.Owner, in.Name, in.Version)
		if err != nil {
			logrus.Errorf("ReadTar ReadFile: %s", err.Error())
			return status.Error(codes.NotFound, "we could not find the requested profile")
		}
	} else {
		tarBlob, err = srv.store.ReadMarketTar(in.Name, in.Version)
		if err != nil {
			logrus.Errorf("ReadTar ReadFile: %s", err.Error())
			return status.Error(codes.NotFound, "we could not find the requested profile")
		}
	}

	profileData := &profiles.ProfileData{Data: tarBlob}
	err = stream.Send(profileData)
	if err != nil {
		logrus.Error(err)
		return status.Error(codes.Internal, "we could not find the requested profile")
	}
	return nil
}

// Check the existence of profile IDs(sha256) in the comp-*-profiles ES index
func (srv *PGProfileServer) MetaSearch(ctx context.Context, profileSha256 *profiles.Sha256) (*profiles.Missing, error) {
	logrus.Debugf("Checking if profiles with sha256 IDs %v are already stored in ElasticSearch", profileSha256.Sha256)

	profilesMetaMissing, err := srv.esClient.ProfilesMissing(profileSha256.Sha256)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	exists := profiles.Missing{
		MissingSha256: profilesMetaMissing,
	}

	return &exists, nil
}

// Delete never deletes a profile, it only deletes the associations
func (srv *PGProfileServer) Delete(ctx context.Context, in *profiles.ProfileDetails) (*pb.Empty, error) {
	logrus.Infof("Deleting profile: %+v", in)
	err := srv.store.Delete(in.Owner, in.Name, in.Version)
	if err != nil {
		logrus.Errorf("Delete: %s", err.Error())
		return nil, status.Error(codes.NotFound, "we could not delete the requested profile")
	}
	// fire profile deleted event
	go srv.fireEvent(event.ProfileDeletedEventName, in.Owner, in.Name, in.Version)
	return &pb.Empty{}, nil
}

func (srv *PGProfileServer) List(ctx context.Context, in *profiles.Query) (*profiles.Profiles, error) {
	sortOrder := in.Order.String()

	sortField := in.Sort
	if sortField == "" {
		sortField = "title"
	}

	filters := make(map[string][]string)
	for _, filter := range in.Filters {
		filters[filter.Type] = filter.Values
	}

	logrus.WithFields(logrus.Fields{
		"filters":   filters,
		"name":      in.Name,
		"namespace": in.Owner,
		"page":      in.Page,
		"per_page":  in.PerPage,
		"sortField": sortField,
		"sortOrder": sortOrder,
	}).Infof("Listing profiles")

	metadata, total, err := srv.store.ListProfilesMetadata(dbstore.ProfilesListRequest{
		Filters:   filters,
		Name:      in.Name,
		Namespace: in.Owner,
		Order:     sortOrder,
		Page:      int(in.Page),
		PerPage:   int(in.PerPage),
		Sort:      sortField,
	})
	if err != nil {
		return nil, err
	}

	// convert metadata to profile
	profiles := conversion.ConvertInspecMetadatasToProfiles(metadata, in.Owner)
	profiles.Total = int32(total)

	return profiles, nil
}

func (srv *PGProfileServer) RebuildElasticCache(ctx context.Context, in *pb.Empty) (*pb.Empty, error) {
	err := srv.rebuildElasticProfileCache()
	if err != nil {
		logrus.Error(err)
		return nil, status.Error(codes.Internal, "we could not rebuild the cache")
	}

	return &pb.Empty{}, nil
}

func (srv *PGProfileServer) MigrateDiskProfiles(ctx context.Context, in *pb.Empty) (*pb.Empty, error) {
	err := srv.migrateDiskProfiles()
	if err != nil {
		logrus.Error(err)
		return nil, status.Error(codes.Internal, "we could not migrate the profiles")
	}

	return &pb.Empty{}, nil
}

func (srv *PGProfileServer) fireEvent(eventType string, owner string, name string, version string) {
	event := srv.newEventMsg(eventType, owner, name, version)

	req := automate_event.PublishRequest{Msg: event}
	_, err := srv.eventsClient.Publish(context.Background(), &req)
	if err != nil {
		logrus.Warnf("Error publishing profiles event: %v", err)
		return
	}
}

func (srv *PGProfileServer) newEventMsg(eventType string, owner string, name string, version string) *automate_event.EventMsg {
	var (
		verbVal string
		tagsVal []string
		nameVal = name + " version " + version
		userVal = owner
	)

	switch eventType {

	case event.ProfileCreatedEventName:
		verbVal = "create"
		tagsVal = []string{"profile", userVal, "create", "compliance", event.ProfileCreatedEventName}

	case event.ProfileDeletedEventName:
		verbVal = "delete"
		tagsVal = []string{"profile", userVal, "delete", "compliance", event.ProfileDeletedEventName}

	default:
		logrus.Warnf("Could not generate event message for profile %s; unknown event type %s", nameVal, eventType)
	}

	return &automate_event.EventMsg{
		EventId: uuid.Must(uuid.NewV4()).String(),
		Type:    &automate_event.EventType{Name: eventType},
		Producer: &automate_event.Producer{
			Id:           "profile",
			ProducerName: "Profile",
			ProducerType: "system component",
		},
		Tags:      tagsVal,
		Published: ptypes.TimestampNow(),
		Actor: &automate_event.Actor{
			Id:          "",
			ObjectType:  "User",
			DisplayName: userVal,
		},
		Verb: verbVal,
		Object: &automate_event.Object{
			Id:          uuid.Must(uuid.NewV4()).String(),
			ObjectType:  "profile",
			DisplayName: nameVal,
		},
		Target: &automate_event.Target{
			Id:          "",
			ObjectType:  "Not Applicable",
			DisplayName: "Not Applicable",
		},
	}
}
