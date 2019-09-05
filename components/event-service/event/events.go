package event

import (
	"context"
	"sync"
	"time"

	"errors"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/api/interservice/ingest"
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

type Registry map[string][]string

//----------  EVENTS  ----------//

// Type EventService is responsible for event publishing and processing. Currently, it has a
// hard-coded map of events to event handlers. When an event is published,
// EventService gets the event off its input channel and matches the event with the correct handler
// function. Handler functions are started in their own goroutines.
type EventService struct {
	in                    chan *api.EventMsg
	registry              Registry
	handlers              map[string]EventHandler // maps an event to an array of pointers to handlers
	cfg                   *config.EventConfig
	connFactory           *secureconn.Factory
	eventProcessorStarted bool
	eventProcessorDone    chan struct{}
	sync.Mutex
}

func NewEventService(cfg *config.EventConfig, cf *secureconn.Factory) *EventService {
	return &EventService{
		in:                 make(chan *api.EventMsg, cfg.ServiceConfig.EventLimit),
		handlers:           make(map[string]EventHandler),
		cfg:                cfg,
		connFactory:        cf,
		eventProcessorDone: make(chan struct{}, 1),
		Mutex:              sync.Mutex{},
	}
}

// Start the event processor that listens on the in channel and fires off
// event handlers for each event received.
func (svc *EventService) Start(r Registry) error {
	if r == nil {
		return errors.New("a registry is required to start event handlers")
	}
	svc.registry = r

	if !svc.eventProcessorStarted {
		processEvents := func() {
			logrus.Info("Starting event processor")
			svc.Lock()
			svc.eventProcessorStarted = true
			svc.Unlock()

			for {
				select {
				case <-svc.eventProcessorDone:
					logrus.Info("Stopping event processor")
					svc.Lock()
					svc.eventProcessorStarted = false
					svc.Unlock()
					return
				default:
				}

				select {
				case event := <-svc.in:
					logrus.Debugf("Processing event %v", event)
					eventHandlers, err := svc.getHandlers(event)
					if err != nil {
						logrus.WithError(err).WithField("event", event).Error("failed to get event handlers for event")
						break
					}

					for _, eventHandler := range eventHandlers {
						go eventHandler.HandleEvent(event)
					}
				// If there are no incoming events break out every 500 seconds
				// to check if we've received the done signal.
				case <-time.After(5000 * time.Millisecond):
				}
			}
		}

		go processEvents()
	}

	return nil
}

// Other services call publish on the event service api in order to notify the rest
// of the system of an event. Services should fire events in their own goroutines so
// that they are not blocked if the event input queue is full.
func (svc *EventService) Publish(event *api.EventMsg) {
	svc.in <- event
	logrus.Debugf("Published event %s", event.EventID)
}

// Should return a CallStatus?
func (svc *EventService) getClient(handlerType string) (EventHandlerClient, error) {

	ctx := context.Background()
	timeoutCtx, cancel := context.WithTimeout(ctx, clientTimeout)
	defer cancel()

	switch handlerType {
	case config.ConfigMgmtEventName:
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
	case config.ComplianceEventName:
		conn, err := svc.connFactory.DialContext(timeoutCtx, "compliance-service", svc.cfg.HandlerEndpoints.Compliance, grpc.WithBlock())
		if err != nil {
			logrus.Errorf("Event service could not get event handler client; error grpc dialing compliance ingest's event handler %s", err.Error())
			return nil, err
		}
		complianceIngesterClient := compliance_ingest.NewComplianceIngesterClient(conn)
		if complianceIngesterClient == nil {
			logrus.Errorf("CallHandler could not obtain NewComplianceIngesterClient")
			return nil, errors.New("CallHandler could not obtain NewComplianceIngesterClient")
		}
		return complianceIngesterClient, nil
	case config.EventFeedEventName:
		conn, err := svc.connFactory.DialContext(timeoutCtx, "event-feed-service", svc.cfg.HandlerEndpoints.EventFeed, grpc.WithBlock())
		if err != nil {
			logrus.Errorf("Event service could not get event handler client; error grpc dialing event feed handler %s", err.Error())
			return nil, err
		}
		eventFeedClient := event_feed.NewEventFeedServiceClient(conn)
		if eventFeedClient == nil {
			logrus.Errorf("CallHandler could not obtain NewFeedServiceClient")
			return nil, errors.New("CallHandler could not obtain NewFeedServiceClient")
		}
		return eventFeedClient, nil
	default:
		return nil, errors.New("can't find client event handler for unrecognized event handler type")
	}
}

func (svc *EventService) getHandlers(e *api.EventMsg) ([]EventHandler, error) {

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

func (svc *EventService) Stop(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
	}

	if svc.eventProcessorStarted {
		svc.eventProcessorDone <- struct{}{}
	}

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(5 * time.Millisecond):
			if !svc.eventProcessorStarted {
				return nil
			}
		}
	}
}
