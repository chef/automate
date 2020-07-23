package publisher

import (
	"context"
	"time"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/event_feed"
	event "github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	log "github.com/sirupsen/logrus"
)

func BuildEventPublisher(
	eventFeedServiceClient event_feed.EventFeedServiceClient) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		return eventPublisher(in, eventFeedServiceClient)
	}
}

func eventPublisher(in <-chan message.ChefAction,
	eventFeedServiceClient event_feed.EventFeedServiceClient) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 100)
	go func() {
		for msg := range in {
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
				continue
			}

			event, err := createEvent(msg)
			if err != nil {
				log.Errorf("unable to send event nodeID %s", msg.Action.NodeId)
				message.PropagateChefAction(out, &msg)
				continue
			}

			start := time.Now()
			_, err = eventFeedServiceClient.HandleEvent(context.Background(), event)
			if err != nil {
				log.Errorf("unable to send event nodeID %s", msg.Action.NodeId)
				msg.FinishProcessing(err)
				continue
			}
			dur := time.Since(start)
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
				"dur":         dur,
			}).Debug("Published to event-service")

			// We only need to store polyicy file information.
			if msg.InternalChefAction.EntityType != "policy" {
				msg.FinishProcessing(nil)
				continue
			}

			message.PropagateChefAction(out, &msg)
		}
		close(out)
	}()

	return out
}

func createEvent(msg message.ChefAction) (*automate_event.EventMsg, error) {
	timestamp, err := ptypes.TimestampProto(msg.InternalChefAction.RecordedAt)
	if err != nil {
		log.WithFields(log.Fields{
			"func":      "createEvent",
			"err":       err,
			"timestamp": msg.InternalChefAction.RecordedAt.String(),
		}).Warn("Unable to translate checkin time to timestamp proto")
		return &automate_event.EventMsg{}, err
	}

	return &automate_event.EventMsg{
		EventID: uuid.Must(uuid.NewV4()).String(),
		Type:    &automate_event.EventType{Name: event.EventFeedEventName},
		Producer: &automate_event.Producer{
			ID:           msg.InternalChefAction.EntityType,
			ProducerName: msg.InternalChefAction.EntityType,
			ProducerType: "chef_server",
		},
		Published: timestamp,
		Actor: &automate_event.Actor{
			ID:          msg.InternalChefAction.RemoteRequestId,
			ObjectType:  msg.InternalChefAction.RequestorType,
			DisplayName: msg.InternalChefAction.RequestorName,
		},
		Verb: msg.Action.Task,
		Object: &automate_event.Object{
			ID:          msg.InternalChefAction.Id,
			ObjectType:  msg.InternalChefAction.EntityType,
			DisplayName: msg.InternalChefAction.EntityName,
		},
		Target: &automate_event.Target{
			ID:          "",
			ObjectType:  "Not Applicable",
			DisplayName: msg.InternalChefAction.ServiceHostname,
		},
		Projects: msg.InternalChefAction.Projects,
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				"chef_organization": {
					Kind: &_struct.Value_StringValue{
						StringValue: msg.InternalChefAction.OrganizationName,
					},
				},
				"chef_infra_server": {
					Kind: &_struct.Value_StringValue{
						StringValue: msg.InternalChefAction.ServiceHostname,
					},
				},
				"parent_name": {
					Kind: &_struct.Value_StringValue{
						StringValue: msg.InternalChefAction.ParentName,
					},
				},
				"parent_id": {
					Kind: &_struct.Value_StringValue{
						StringValue: msg.InternalChefAction.ParentType,
					},
				},
			},
		},
	}, nil
}
