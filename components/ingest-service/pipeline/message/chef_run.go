package message

import (
	"context"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/backend"
)

type ChefRunPipe func(<-chan ChefRun) <-chan ChefRun

type ChefRun struct {
	ID               uuid.UUID
	QueueTime        time.Time
	Run              chef.Run
	Node             backend.Node
	NodeRun          backend.Run
	NodeAttribute    backend.NodeAttribute
	BulkableRequests []elastic.BulkableRequest
	Ctx              context.Context
	errc             chan<- error
}

func NewChefRun(ctx context.Context, run *chef.Run, err chan<- error) ChefRun {
	return ChefRun{
		uuid.Must(uuid.NewV4()),
		time.Now(),
		*run,
		backend.Node{},
		backend.Run{},
		backend.NodeAttribute{},
		[]elastic.BulkableRequest{},
		ctx,
		err,
	}
}

func (chefRun *ChefRun) FinishProcessing(err error) {
	chefRun.errc <- err

	if err == nil {
		// Adding new metric; Time for a message to go through the pipeline
		log.WithFields(log.Fields{
			"message_id": chefRun.ID,
			"message":    "ChefRun",
			"metric":     "pipeline",
			"type":       "ingest_time",
			"ms":         chefRun.ClockProcessingTime(),
		}).Info("Message ingested successfully")
	}
}

// ClockProcessingTime will return the time that has passed (in milliseconds) since
// the message got started until 'Now()'
//
// Useful to check the time that messages take to go through the pipeline
func (chefRun *ChefRun) ClockProcessingTime() int64 {
	var (
		t       = time.Now()
		elapsed = t.Sub(chefRun.QueueTime)
	)

	return elapsed.Nanoseconds() / int64(time.Millisecond)
}
