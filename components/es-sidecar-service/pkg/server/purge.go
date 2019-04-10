package server

import (
	"context"

	"github.com/golang/protobuf/ptypes/empty"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/version"
)

// PurgeTimeSeriesIndicesByAge deletes indexes in the form basename-YYYY-mm-dd older than the requested number of days.
func (server *EsSidecarServer) PurgeTimeSeriesIndicesByAge(ctx context.Context, in *api.PurgeRequest) (*api.PurgeResponse, error) {
	log.WithFields(log.Fields{
		"id":                     in.GetId(),
		"index-base-name":        in.GetIndex(),
		"delete-older-than-days": in.GetOlderThanDays(),
	}).Debug("purge of time-series index")

	err := server.es.DeleteTimeSeriesIndicesByAge(ctx, in.GetIndex(), int(in.GetOlderThanDays()))
	if err != nil {
		errStr := err.Error()
		return &api.PurgeResponse{
			Success: false,
			Message: errStr,
		}, errors.GrpcError(codes.Internal, errStr)
	}

	return &api.PurgeResponse{
		Success: true,
		Message: "Time series index purge completed",
	}, nil
}

// PurgeDocumentsFromIndexByAge deletes all documents containing a date-mapped field "end_time" older than
// in.OlderThanDays from in.Index.
func (server *EsSidecarServer) PurgeDocumentsFromIndexByAge(ctx context.Context, in *api.PurgeRequest) (*api.PurgeResponse, error) {
	logFields := log.Fields{
		"id":                     in.GetId(),
		"index":                  in.GetIndex(),
		"delete-older-than-days": in.GetOlderThanDays(),
		"custom-purge-field":     in.GetCustomPurgeField(),
	}

	log.WithFields(logFields).Debug("Initiating document purge from index")
	err := server.es.DeleteDocumentsFromIndexByAge(ctx, in.GetIndex(), int(in.GetOlderThanDays()), in.GetCustomPurgeField())
	if err == nil {
		log.WithFields(logFields).Info("Document purge completed successfully")
		return &api.PurgeResponse{Success: true}, nil
	} else {
		deleteErr, isDeleteError := err.(elastic.DeleteError)
		if isDeleteError {
			failures := make([]*api.PurgeResponse_Failure, 0, len(deleteErr.Failures))
			for _, failed := range deleteErr.Failures {
				failures = append(failures, &api.PurgeResponse_Failure{Id: failed.ID, Type: failed.Type})
			}
			errStr := err.Error()
			return &api.PurgeResponse{
				Success:  false,
				Message:  errStr,
				Failures: failures,
			}, errors.GrpcError(codes.Internal, errStr)
		}
	}
	return nil, errors.GrpcError(codes.Internal, err.Error())
}

// Version returns the Version of the ES Sidecar Service
func (server *EsSidecarServer) Version(ctx context.Context, request *empty.Empty) (*api.VersionResponse, error) {
	response := api.VersionResponse{
		Version: version.Version,
	}
	return &response, nil
}
