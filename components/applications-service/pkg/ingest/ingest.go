package ingest

import (
	"fmt"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/golang/protobuf/proto"
	"github.com/pkg/errors"
)

const (
	numWorkers       = 50
	eventsBufferSize = 100
)

type Ingester struct {
	cfg                   *config.Applications
	storageClient         storage.Client
	natsClient            *nats.NatsClient
	eventsCh              chan []byte
	workerInputQueue      chan<- *habitat.HealthCheckEvent
	workerTaskQueue       <-chan *habitat.HealthCheckEvent
	workerOutputQueue     chan error
	totalEventsFailed     int64
	totalEventsSuccessful int64
	totalEventsProcessed  int64
}

func New(c *config.Applications, client storage.Client) *Ingester {
	var (
		events = make(chan []byte, eventsBufferSize)
		queue  = make(chan *habitat.HealthCheckEvent)
		out    = make(chan error, eventsBufferSize)
	)

	return &Ingester{
		cfg:                   c,
		storageClient:         client,
		eventsCh:              events,
		workerInputQueue:      queue,
		workerTaskQueue:       queue,
		workerOutputQueue:     out,
		totalEventsFailed:     0,
		totalEventsSuccessful: 0,
		totalEventsProcessed:  0,
	}

}

// Connect initiates the connection to the NATS server. It's separate from
// Run() so connection errors can be handled easily while still running the
// ingester in a goroutine.
func (i *Ingester) Connect() error {
	natsClient := nats.NewDefaults(
		fmt.Sprintf("nats://%s:%d", i.cfg.Events.Host, i.cfg.Events.Port),
		i.cfg.Events.ClusterID,
		*i.cfg.TLSConfig,
	)

	// Trigger NATS Subscription
	err := natsClient.ConnectAndSubscribe(i.eventsCh)
	if err != nil {
		return errors.Wrapf(err, "could not connect to nats-streaming")
	}
	i.natsClient = natsClient
	return nil
}

func (i *Ingester) Run() {
	for n := 0; n <= numWorkers; n++ {
		go i.runWorkerLoop()
	}
	// Receive digested messages from the event channel and
	// send them to the datastore for processing
	// TODO @afiune Move this logic into an ingestion pipeline
	for {
		select {
		// TODO: @afiune We should have a way to have multi-message-type ingestion
		case eventBytes := <-i.eventsCh:
			// Unmarshal the data and send the message to the channel
			var habMsg habitat.HealthCheckEvent
			err := proto.Unmarshal(eventBytes, &habMsg)
			if err != nil {
				log.WithFields(log.Fields{
					"data":    string(eventBytes),
					"subject": i.natsClient.Subject(),
				}).Error("Unknown message, dropping")
				continue
			}

			i.workerInputQueue <- &habMsg
		}
	}
}

func (i *Ingester) runWorkerLoop() {
	for {
		select {
		case event := <-i.workerTaskQueue:
			err := i.storageClient.IngestHealthCheckEvent(event)
			i.workerOutputQueue <- err
		case err := <-i.workerOutputQueue:
			i.totalEventsProcessed++

			if err != nil {
				log.WithError(err).Error("Unable to ingest habitat event")
				i.totalEventsFailed++
			} else {
				i.totalEventsSuccessful++
			}

			log.WithFields(log.Fields{
				"total_events_processed":  i.totalEventsProcessed,
				"total_events_failed":     i.totalEventsFailed,
				"total_events_successful": i.totalEventsSuccessful,
			}).Debug("event processed")
		}
	}
}

// Sends a message through the ingestion "pipeline" (aka our events channel)
func (i *Ingester) IngestMessage(msg []byte) {
	i.eventsCh <- msg
}

// Returns the total number of events processed by the ingestor client
func (i *Ingester) EventsProcessed() int64 {
	return i.totalEventsProcessed
}

// Returns the total number of event messages in the ingestor client queue
func (i *Ingester) QueueLen() int {
	return len(i.eventsCh)
}
