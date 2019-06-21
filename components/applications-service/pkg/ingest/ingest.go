package ingest

import (
	"fmt"

	stan "github.com/nats-io/go-nats-streaming"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/components/applications-service/pkg/storage/postgres"
	"github.com/golang/protobuf/proto"
	"github.com/pkg/errors"
)

const (
	numWorkers       = 50
	eventsBufferSize = 100
)

type Ingester struct {
	Cfg              *config.Applications
	DBClient         *postgres.Postgres
	natsClient       *nats.NatsClient
	EventsCh         chan *stan.Msg
	workerInputQueue chan<- *habitat.HealthCheckEvent
	workerTaskQueue  <-chan *habitat.HealthCheckEvent
}

func New(c *config.Applications, db *postgres.Postgres) *Ingester {

	q := make(chan *habitat.HealthCheckEvent)

	return &Ingester{
		Cfg:              c,
		DBClient:         db,
		EventsCh:         make(chan *stan.Msg, eventsBufferSize),
		workerInputQueue: q,
		workerTaskQueue:  q,
	}

}

// Connect initiates the connection to the NATS server. It's separate from
// Run() so connection errors can be handled easily while still running the
// ingester in a goroutine.
func (i *Ingester) Connect() error {
	natsClient := nats.NewDefaults(
		fmt.Sprintf("nats://%s:%d", i.Cfg.Events.Host, i.Cfg.Events.Port),
		i.Cfg.Events.ClusterID,
		*i.Cfg.TLSConfig,
	)

	// Trigger NATS Subscription
	err := natsClient.ConnectAndSubscribe(i.EventsCh)
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
		case event := <-i.EventsCh:

			// TODO: @afiune We should have a way to have multi-message-type ingestion
			// Unmarshal the data and send the message to the channel
			var habMsg habitat.HealthCheckEvent
			err := proto.Unmarshal(event.Data, &habMsg)
			if err != nil {
				log.WithFields(log.Fields{
					"data":    string(event.Data),
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
			err := i.DBClient.IngestHealthCheckEvent(event)
			if err != nil {
				log.WithError(err).Error("Unable to ingest habitat event")
			}
		}
	}
}
