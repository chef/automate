package server

import (
	"context"

	"github.com/golang/protobuf/ptypes/empty"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/version"
)

// PurgeTimeSeriesIndicesByAge deletes indexes in the form basename-YYYY-mm-dd older than the requested number of days.
func (server *EsSidecarServer) PurgeTimeSeriesIndicesByAge(ctx context.Context, in *api.PurgeRequest) (*api.PurgeResponse, error) {
	logctx := log.WithFields(log.Fields{
		"id":                     in.GetId(),
		"index-base-name":        in.GetIndex(),
		"delete-older-than-days": in.GetOlderThanDays(),
	})

	err := server.es.DeleteTimeSeriesIndicesByAge(ctx, in.GetIndex(), int(in.GetOlderThanDays()))
	if err != nil {
		logctx.WithError(err).Error("failed to purge time series indices by age")
		errStr := err.Error()
		return &api.PurgeResponse{
			Success: false,
			Message: errStr,
		}, status.Error(codes.Internal, errStr)
	}

	logctx.Info("successfully purged time series indices by age")

	return &api.PurgeResponse{
		Success: true,
		Message: "Time series index purge completed",
	}, nil
}

// PurgeDocumentsFromIndexByAge deletes all documents containing a date-mapped field "end_time" older than
// in.OlderThanDays from in.Index.
func (server *EsSidecarServer) PurgeDocumentsFromIndexByAge(ctx context.Context, in *api.PurgeRequest) (*api.PurgeResponse, error) {
	logctx := log.WithFields(log.Fields{
		"id":                     in.GetId(),
		"index":                  in.GetIndex(),
		"delete-older-than-days": in.GetOlderThanDays(),
		"custom-purge-field":     in.GetCustomPurgeField(),
	})

	logctx.Debug("Initiating document purge from index")
	err := server.es.DeleteDocumentsFromIndexByAge(ctx, in.GetIndex(), int(in.GetOlderThanDays()), in.GetCustomPurgeField())
	if err != nil {
		res := &api.PurgeResponse{Success: false, Message: err.Error()}

		deleteErr, isDeleteError := err.(elastic.DeleteError)
		if isDeleteError {
			failures := make([]*api.PurgeResponse_Failure, 0, len(deleteErr.Failures))
			for _, failed := range deleteErr.Failures {
				failures = append(failures, &api.PurgeResponse_Failure{Id: failed.ID, Type: failed.Type})
			}
			logctx.WithError(err).WithField("failures", failures).Error("failed to purge document from index by age")
			res.Failures = failures
		}

		return res, status.Error(codes.Internal, err.Error())
	}

	logctx.Info("successfully purged documents from index by age")
	return &api.PurgeResponse{Success: true}, nil
}

// Version returns the Version of the ES Sidecar Service
func (server *EsSidecarServer) Version(ctx context.Context, request *empty.Empty) (*api.VersionResponse, error) {
	response := api.VersionResponse{
		Version: version.Version,
	}
	return &response, nil
}
