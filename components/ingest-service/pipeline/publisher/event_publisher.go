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
	eventFeedServiceClient event_feed.EventFeedServiceClient, numPublishers int) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		if numPublishers <= 0 || eventFeedServiceClient == nil {
			log.Info("Direct publish to event feed service is disabled")
			return actionsNoop(in)
		}

		out := make(chan message.ChefAction, 100)
		log.Infof("Starting event feed service publisher")
		for i := 0; i < numPublishers; i++ {
			go eventPublisher(in, eventFeedServiceClient, out)
		}
		return out
	}
}

func eventPublisher(in <-chan message.ChefAction,
	client event_feed.EventFeedServiceClient, out chan<- message.ChefAction) {
	for msg := range in {
		eventHandleMessage(client, msg, out)

	}
	close(out)
}

func eventHandleMessage(client event_feed.EventFeedServiceClient, msg message.ChefAction,
	out chan<- message.ChefAction) {
	if err := msg.Ctx.Err(); err != nil {
		msg.FinishProcessing(err)
		return
	}

	event, err := createEvent(msg)
	if err != nil {
		log.Errorf("unable to create event %v", msg.Action)
		msg.FinishProcessing(err)
		return
	}

	start := time.Now()
	_, err = client.HandleEvent(context.Background(), event)
	if err != nil {
		log.Errorf("unable to send event %v", msg.Action)
		msg.FinishProcessing(err)
		return
	}
	dur := time.Since(start)
	log.WithFields(log.Fields{
		"message_id":  msg.ID,
		"buffer_size": len(out),
		"dur":         dur,
	}).Debug("Published to event-service")

	message.PropagateChefAction(out, &msg)
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
		EventId: uuid.Must(uuid.NewV4()).String(),
		Type:    &automate_event.EventType{Name: event.EventFeedEventName},
		Producer: &automate_event.Producer{
			Id:           msg.InternalChefAction.EntityType,
			ProducerName: msg.InternalChefAction.EntityType,
			ProducerType: "chef_server",
		},
		Published: timestamp,
		Actor: &automate_event.Actor{
			Id:          msg.InternalChefAction.RemoteRequestId,
			ObjectType:  msg.InternalChefAction.RequestorType,
			DisplayName: msg.InternalChefAction.RequestorName,
		},
		Verb: msg.Action.Task,
		Object: &automate_event.Object{
			Id:          msg.InternalChefAction.Id,
			ObjectType:  msg.InternalChefAction.EntityType,
			DisplayName: msg.InternalChefAction.EntityName,
		},
		Target: &automate_event.Target{
			Id:          "",
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
