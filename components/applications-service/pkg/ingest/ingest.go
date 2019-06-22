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
	cfg              *config.Applications
	storageClient    storage.Client
	natsClient       *nats.NatsClient
	eventsCh         chan []byte
	workerInputQueue chan<- *habitat.HealthCheckEvent
	workerTaskQueue  <-chan *habitat.HealthCheckEvent
}

func New(c *config.Applications, client storage.Client) *Ingester {

	q := make(chan *habitat.HealthCheckEvent)

	return &Ingester{
		cfg:              c,
		storageClient:    client,
		eventsCh:         make(chan []byte, eventsBufferSize),
		workerInputQueue: q,
		workerTaskQueue:  q,
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
		case eventBytes := <-i.eventsCh:

			// TODO: @afiune We should have a way to have multi-message-type ingestion
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
			if err != nil {
				log.WithError(err).Error("Unable to ingest habitat event")
			}
		}
	}
}

// Sends a message through the ingestion "pipeline" (aka our events channel)
func (i *Ingester) IngestMessage(msg []byte) {
	i.eventsCh <- msg
}
