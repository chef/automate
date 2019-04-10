package processor

import (
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/buger/jsonparser"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

// ChefActionTransmogrify translates an action protobuf into an InternalChefAction struct
func ChefActionTransmogrify(in <-chan message.ChefAction) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 100)
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"message":     "ChefAction",
				"buffer_size": len(out)},
			).Debug("Transforming ChefAction")

			recordedAt, err := time.Parse(time.RFC3339, msg.Action.GetRecordedAt())
			if err != nil {
				grpcErr := status.Errorf(
					codes.Internal,
					"Unable to transform Action RecordAt timestring to Time object: %s", err.Error(),
				)
				msg.FinishProcessing(grpcErr)
				continue
			}

			msg.InternalChefAction = backend.InternalChefAction{
				Id:               msg.Action.GetId(),
				MessageType:      msg.Action.GetMessageType(),
				MessageVersion:   msg.Action.GetMessageVersion(),
				EntityName:       msg.Action.GetEntityName(),
				EntityType:       msg.Action.GetEntityType(),
				ParentType:       msg.Action.GetParentType(),
				ParentName:       msg.Action.GetParentName(),
				Task:             msg.Action.GetTask(),
				OrganizationName: msg.Action.GetOrganizationName(),
				RemoteHostname:   msg.Action.GetRemoteHostname(),
				RunId:            msg.Action.GetRunId(),
				NodeId:           msg.Action.GetNodeId(),
				RecordedAt:       recordedAt,
				RemoteRequestId:  msg.Action.GetRemoteRequestId(),
				RequestId:        msg.Action.GetRequestId(),
				RevisionId:       msg.Action.GetRevisionId(),
				RequestorName:    msg.Action.GetRequestorName(),
				RequestorType:    msg.Action.GetRequestorType(),
				ServiceHostname:  msg.Action.GetServiceHostname(),
				UserAgent:        msg.Action.GetUserAgent(),
			}

			// If the entity type is policy, it means that we are dealing with a policy action and
			// we need to store the `data` field of the message
			if msg.InternalChefAction.EntityType == "policy" {
				log.WithFields(log.Fields{
					"message_id":  msg.ID,
					"entity_type": "policy",
				}).Debug("Parsing the 'data' field to be stored")

				// The current implementation of how we ingest policy file actions involves storing
				// the data field in elasticsearch so that the config-mgmt-service can, once again,
				// unmarshal this object and get the PolicyCookbooks information.
				//
				// Code:
				// => https://github.com/chef/automate/blob/master/components/config-mgmt-service/backend/elastic/actions.go#L49
				//
				// TODO: @afiune there should be something better we can do here but for now,
				// we will continue storing to avoid heavy modifications in the system.
				dataBytes, err := getActionDataFieldInBytes(msg.Action.GetContent())
				if err != nil {
					msg.FinishProcessing(err)
					continue
				}
				msg.InternalChefAction.Data = string(dataBytes)

				// If we don't have already a revision_id then we need to extract it from the
				// data field of the raw content of the request
				if msg.InternalChefAction.RevisionId == "" {
					log.WithFields(log.Fields{
						"message_id":  msg.ID,
						"entity_type": "policy",
					}).Debug("Setting revision_id for policy action from the 'data' field")

					revisionID, err := policyRevisionFromDataBytes(dataBytes)
					if err != nil {
						msg.FinishProcessing(err)
						continue
					}

					log.WithFields(log.Fields{
						"message_id":  msg.ID,
						"entity_type": "policy",
						"revision_id": revisionID,
					}).Debug("Revision id found")
					msg.InternalChefAction.RevisionId = revisionID
				}
			}

			out <- msg
		}
		close(out)
	}()

	return out
}

// getActionDataFieldInBytes - Retrieves the `data` field from a raw content of a message
func getActionDataFieldInBytes(content []byte) ([]byte, error) {
	dataBytes, dataType, _, err := jsonparser.Get(content, "data")
	if err != nil {
		// We couldn't get the data field
		return nil, status.Errorf(
			codes.Internal,
			"Unable to retrieve 'data' field from action message: %s", err.Error(),
		)
	}

	// This log entry will tell us exactly what type the data field is
	log.WithFields(log.Fields{
		"type": dataType,
	}).Debug("Reporting policy message 'data' field")

	// The `data` field could be either an String or a nested JSON object,
	// with this level of inconsistency we need to codify the following extra
	// parser to handle the case where this field is an string, if the field
	// is a nested JSON object we really don't do anything extra
	if dataType == jsonparser.String {
		// Parse the string to remove the escaped characters
		stringBytes, err := jsonparser.ParseString(dataBytes)
		if err != nil {
			return nil, status.Errorf(
				codes.Internal,
				"Unable to retrieve 'data' field from action message: %s", err.Error(),
			)
		}

		// Finally just cast it as an array of bytes
		dataBytes = []byte(stringBytes)
	}

	return dataBytes, nil
}

// policyRevisionFromDataBytes - Retrieves the revision_id from the raw content of the data
// field for policy actions
func policyRevisionFromDataBytes(dataBytes []byte) (string, error) {
	revisionID, err := jsonparser.GetString(dataBytes, "revision_id")
	if err != nil {
		return "", status.Errorf(
			codes.Internal,
			"Unable to retrieve revision_id from policy action: %s", err.Error(),
		)
	}

	return revisionID, nil
}
