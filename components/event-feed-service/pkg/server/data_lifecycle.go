package server

import (
	"context"

	dls "github.com/chef/automate/api/interservice/data_lifecycle"
	es "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	log "github.com/sirupsen/logrus"
)

type DataLifecycleManageableServer struct {
	purgeClient   es.EsSidecarClient
	purgePolicies []PurgePolicy
}

type PurgePolicy struct {
	// PurgeOlderThanDays describes how long we keep time series data. Anything
	// older than this is allowed to be regularly removed.
	PurgeOlderThanDays int32
	// IndexName is the base name of the time series index that should be purged
	IndexName string
	// IMPLEMENTED FOR NON-TIME SERIES INDICES ONLY!
	// Optional field. Purge documents based on the time value in this field rather than
	// the default field, end_time. Values for this field come from client purge policies.
	// If this field is empty, purge defaults to end_time.
	CustomPurgeField string
}

func NewDataLifecycleManageableServer(purgeClient es.EsSidecarClient,
	purgeEventFeedAfterDays int) *DataLifecycleManageableServer {
	purgePolicies := []PurgePolicy{}
	if purgeEventFeedAfterDays >= 0 {
		purgePolicies = append(purgePolicies, PurgePolicy{
			IndexName:          persistence.IndexNameFeeds,
			PurgeOlderThanDays: int32(purgeEventFeedAfterDays),
			CustomPurgeField:   "pub_timestamp",
		})
		log.Debugf("event feed purge config: older than %d days", purgeEventFeedAfterDays)
	}
	return &DataLifecycleManageableServer{
		purgeClient:   purgeClient,
		purgePolicies: purgePolicies,
	}
}

func (server *DataLifecycleManageableServer) Purge(ctx context.Context,
	request *dls.PurgeRequest) (*dls.PurgeResponse, error) {
	purgeResponse := &dls.PurgeResponse{
		Id:              request.GetId(),
		ComponentStatus: make(map[string]*dls.PurgeStatus),
	}

	for _, policy := range server.purgePolicies {
		indexName := policy.IndexName

		log.WithFields(log.Fields{
			"purge_id":        request.GetId(),
			"older_than_days": policy.PurgeOlderThanDays,
			"index_name":      indexName,
		}).Info("Requesting purge")

		resp, err := server.purgeClient.PurgeDocumentsFromIndexByAge(ctx, &es.PurgeRequest{
			Id:               request.GetId(),
			Index:            indexName,
			OlderThanDays:    policy.PurgeOlderThanDays,
			CustomPurgeField: policy.CustomPurgeField,
		})

		if err != nil {
			log.WithFields(log.Fields{
				"purge_id":   request.GetId(),
				"index_name": indexName,
			}).WithError(err).Error("PurgeTimeSeries rpc call failed")

			// Errors are transformed into a purge status code and a message. We dont return
			// a grpc error so that we can allow multiple components to have their own status
			purgeResponse.ComponentStatus[indexName] = &dls.PurgeStatus{
				Status: dls.PurgeStatus_FAILED,
				Msg:    err.Error(),
			}
		} else {
			if resp.GetSuccess() {
				log.WithFields(log.Fields{
					"purge_id":   request.GetId(),
					"index_name": indexName,
				}).Info("Purge Success")

				purgeResponse.ComponentStatus[indexName] = &dls.PurgeStatus{
					Status: dls.PurgeStatus_SUCCESS,
					Msg:    resp.GetMessage(),
				}
			} else {
				log.WithFields(log.Fields{
					"purge_id":   request.GetId(),
					"index_name": indexName,
				}).Error("Purge Failed")

				purgeResponse.ComponentStatus[indexName] = &dls.PurgeStatus{
					Status: dls.PurgeStatus_FAILED,
					Msg:    resp.GetMessage(),
				}
			}
		}
	}

	return purgeResponse, nil
}
