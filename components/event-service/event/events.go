package event

import (
	"context"
	"time"

	"errors"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/ingest"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	compliance_ingest "github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/grpc/secureconn"
)

const (
	clientTimeout = 60 * time.Second
)

//----------  EVENT HANDLER  ----------//

type EventHandlerClient interface {
	// TODO: CallOption?
	HandleEvent(ctx context.Context, in *api.EventMsg, opts ...grpc.CallOption) (*api.EventResponse, error)
}

type EventHandler interface {
	HandleEvent(*api.EventMsg)
	GetHandlerType() string
	GetClient() EventHandlerClient
}

type eventHandler struct {
	handlerType string
	client      EventHandlerClient
}

func newEventHandler(handlerType string, client EventHandlerClient) EventHandler {
	return eventHandler{
		handlerType: handlerType,
		client:      client,
	}
}

func (e eventHandler) HandleEvent(msg *api.EventMsg) {
	// TODO: add capabilities (timeout, etc.) to context?
	response, err := e.client.HandleEvent(context.Background(), msg)

	if err != nil {
		logrus.Warnf("Client event handler returned an error... event won't be handled %s", err.Error())
	} else {
		logrus.Debugf("Event handler returned with status %s", response)
	}
}

func (e eventHandler) GetHandlerType() string {
	return e.handlerType
}

func (e eventHandler) GetClient() EventHandlerClient {
	return e.client
}

//----------  EVENTS  ----------//

// Type Events is responsible for event publishing and processing. Currently, it has a
// hard-coded map of events to event handlers. When an event is published,
// Events gets the event off its input channel and matches the event with the correct handler
// function. Handler functions are started in their own goroutines.
type Events struct {
	in          chan *api.EventMsg
	registry    map[string][]string
	handlers    map[string]EventHandler // maps an event to an array of pointers to handlers
	cfg         *config.EventConfig
	connFactory *secureconn.Factory
}

func NewEvents(cfg *config.EventConfig, cf *secureconn.Factory) *Events {
	return &Events{
		in:          make(chan *api.EventMsg, cfg.ServiceConfig.EventLimit),
		handlers:    make(map[string]EventHandler),
		cfg:         cfg,
		connFactory: cf,
	}
}

// Listen for events on the input channel until event-service is terminated.
func (svc Events) Start(r map[string][]string) {
	logrus.Debug("Starting event listener loop...")

	if r == nil {
		logrus.Fatalf("Event Service not started... no event handlers in registry")
		return
	}

	svc.registry = r

	for event := range svc.in {
		logrus.Debugf("Processing event %v", event)
		eventHandlers, err := svc.getHandlers(event)
		if err != nil {
			logrus.Warnf("could not get handlers for event %s: %+v", event.GetType().GetName(), err)
			break
		}

		for _, eventHandler := range eventHandlers {
			go eventHandler.HandleEvent(event)
		}
	}
}

// Other services call publish on the event service api in order to notify the rest
// of the system of an event. Services should fire events in their own goroutines so
// that they are not blocked if the event input queue is full.
func (svc Events) Publish(event *api.EventMsg) {
	svc.in <- event
	logrus.Debugf("Published event %s", event.EventID)
}

// Should return a CallStatus?
func (svc Events) getClient(handlerType string) (EventHandlerClient, error) {

	ctx := context.Background()
	timeoutCtx, cancel := context.WithTimeout(ctx, clientTimeout)
	defer cancel()

	if handlerType == config.FEED_KEY {
		conn, err := svc.connFactory.DialContext(timeoutCtx, "compliance-service", svc.cfg.HandlerEndpoints.Feed, grpc.WithBlock())
		if err != nil {
			logrus.Errorf("Event service could not get event handler client; error grpc dialing automate-feed's event handler %s", err.Error())
			return nil, err
		}
		feedClient := automate_feed.NewFeedServiceClient(conn)
		if feedClient == nil {
			logrus.Errorf("CallHandler could not obtain NewFeedServiceClient")
			return nil, errors.New("CallHandler could not obtain NewFeedServiceClient")
		}
		return feedClient, nil
	} else if handlerType == config.CFG_KEY {
		conn, err := svc.connFactory.DialContext(timeoutCtx, "ingest-service", svc.cfg.HandlerEndpoints.CfgIngest, grpc.WithBlock())
		if err != nil {
			logrus.Errorf("Event service could not get event handler client; error grpc dialing ingest-service's event handler %s", err.Error())
			return nil, err
		}
		ingestClient := ingest.NewEventHandlerClient(conn)
		if ingestClient == nil {
			logrus.Errorf("CallHandler could not obtain NewEventHandlerClient")
			return nil, errors.New("CallHandler could not obtain NewEventHandlerClient")
		}
		return ingestClient, nil
	} else if handlerType == config.COMPLIANCE_INGEST_KEY {
		conn, err := svc.connFactory.DialContext(timeoutCtx, "compliance-service", svc.cfg.HandlerEndpoints.Feed, grpc.WithBlock())
		if err != nil {
			logrus.Errorf("Event service could not get event handler client; error grpc dialing automate-feed's event handler %s", err.Error())
			return nil, err
		}
		complianceIngesterClient := compliance_ingest.NewComplianceIngesterClient(conn)
		if complianceIngesterClient == nil {
			logrus.Errorf("CallHandler could not obtain NewComplianceIngesterClient")
			return nil, errors.New("CallHandler could not obtain NewComplianceIngesterClient")
		}
		return complianceIngesterClient, nil
	} else { // TODO: return appropriate error type
		return nil, errors.New("can't find client event handler for unrecognized event handler type")
	}
}

func (svc Events) getHandlers(e *api.EventMsg) ([]EventHandler, error) {

	var eventHandlers []EventHandler

	if keys := svc.registry[e.GetType().GetName()]; keys != nil {
		// for each key, get the corresponding handlers, lazily initializing if necessary
		for _, k := range keys {
			if h := svc.handlers[k]; h != nil {
				eventHandlers = append(eventHandlers, h)
			} else {
				// lazily initialize handler
				c, err := svc.getClient(k)
				if err != nil {
					return nil, err
				}
				eh := newEventHandler(k, c)
				eventHandlers = append(eventHandlers, eh)
				svc.handlers[k] = eh
			}
		}
	} else {
		logrus.Warnf("Unable to find event handler for '%s' event", e.GetType().GetName())
	}
	return eventHandlers, nil
}

func (svc Events) Stop() {}
