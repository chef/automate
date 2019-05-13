package server

import (
	"fmt"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/components/applications-service/pkg/storage/postgres"
)

const numWorkers = 50

type Ingester struct {
	Cfg              *config.Applications
	DBClient         *postgres.Postgres
	natsClient       *nats.NatsClient
	workerInputQueue chan<- *habitat.HealthCheckEvent
	workerTaskQueue  <-chan *habitat.HealthCheckEvent
}

func NewIngester(c *config.Applications, db *postgres.Postgres) *Ingester {

	q := make(chan *habitat.HealthCheckEvent)

	return &Ingester{
		Cfg:              c,
		DBClient:         db,
		workerInputQueue: q,
		workerTaskQueue:  q,
	}

}

// Connect initiates the connection to the NATS server. It's separate from
// Run() so connection errors can be handled easily while still running the
// ingester in a goroutine.
func (i *Ingester) Connect() error {
	url := "nats://%s:%d"

	natsClient := nats.NewDefaults(
		fmt.Sprintf(url, i.Cfg.Events.Host, i.Cfg.Events.Port),
		i.Cfg.Events.ClusterID,
		*i.Cfg.TLSConfig,
	)

	// Trigger NATS Subscription
	err := natsClient.ConnectAndSubscribe()
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
		case event := <-i.natsClient.HabServiceEventCh:
			i.workerInputQueue <- event
		}
	}
}

func (i *Ingester) runWorkerLoop() {
	for {
		select {
		case event := <-i.workerTaskQueue:
			err := i.DBClient.IngestHealthCheckEvent(event)
			if err != nil {
				log.WithFields(log.Fields{
					"error": err.Error(),
				}).Error("Unable to ingest habitat event")
			}
		}
	}
}
