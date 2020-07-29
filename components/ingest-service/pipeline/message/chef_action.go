package message

import (
	"context"
	"time"

	uuid "github.com/gofrs/uuid"
	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/backend"
)

type ChefActionPipe func(<-chan ChefAction) <-chan ChefAction

func PropagateChefAction(out chan<- ChefAction, msg *ChefAction) {
	select {
	case out <- *msg:
	case <-msg.Ctx.Done():
		msg.FinishProcessing(msg.Ctx.Err())
	}
}

type ChefAction struct {
	ID                 uuid.UUID
	QueueTime          time.Time
	Action             *chef.Action
	InternalChefAction backend.InternalChefAction
	BulkableRequests   []elastic.BulkableRequest
	Ctx                context.Context
	errc               chan<- error
}

func NewChefAction(ctx context.Context, action *chef.Action, err chan<- error) ChefAction {
	return ChefAction{
		uuid.Must(uuid.NewV4()),
		time.Now(),
		action,
		backend.InternalChefAction{},
		[]elastic.BulkableRequest{},
		ctx,
		err,
	}
}

func (chefAction ChefAction) FinishProcessing(err error) {
	chefAction.errc <- err

	if err == nil {
		// Adding new metric; Time for a message to go through the pipeline
		log.WithFields(log.Fields{
			"parent_type": chefAction.InternalChefAction.ParentType,
			"parent_name": chefAction.InternalChefAction.ParentName,
			"message_id":  chefAction.ID,
			"message":     "ChefAction",
			"metric":      "pipeline",
			"type":        "ingest_time",
			"ms":          chefAction.ClockProcessingTime(),
			"entity_name": chefAction.InternalChefAction.EntityName,
			"entity_type": chefAction.InternalChefAction.EntityType,
			"entity_task": chefAction.InternalChefAction.Task,
		}).Info("Chef Action message ingested successfully")
	}
}

// ClockProcessingTime will return the time that has passed (in milliseconds) since
// the message got started until 'Now()'
//
// Useful to check the time that messages take to go through the pipeline
func (chefAction *ChefAction) ClockProcessingTime() int64 {
	var (
		t       = time.Now()
		elapsed = t.Sub(chefAction.QueueTime)
	)

	return elapsed.Nanoseconds() / int64(time.Millisecond)
}
