package pipeline

import (
	chef "github.com/chef/automate/api/external/ingest/request"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/ingest-service/pipeline/processor"
	"github.com/chef/automate/components/ingest-service/pipeline/publisher"
	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

// ChefActionPipeline pipeline to process Chef actions from client or server
type ChefActionPipeline struct {
	in      chan<- message.ChefAction
	counter *int64 // Number of messages that the pipeline has ran
}

// NewChefActionPipeline Create a new chef action pipeline
func NewChefActionPipeline(client backend.Client) ChefActionPipeline {
	var (
		in            = make(chan message.ChefAction, 100)
		counter int64 = 0
	)

	chefActionPipeline(in,
		processor.BuildChefActionPerform(client),
		processor.ChefActionTransmogrify,
		processor.BuildActionMsgToBulkRequestTransformer(client),
		publisher.BuildBulkActionPublisher(client),
		processor.CountActions(&counter),
	)

	return ChefActionPipeline{in, &counter}
}

func (caPipeline *ChefActionPipeline) GetTotalMessages() int64 {
	return *caPipeline.counter
}

// Run a chef client action through the pipeline
func (caPipeline *ChefActionPipeline) Run(action *chef.Action, errc chan<- error) {
	chefAction := message.NewChefAction(context.Background(), action, errc)

	log.WithFields(log.Fields{
		"message_id":  chefAction.ID,
		"message":     "ChefAction",
		"buffer_size": len(caPipeline.in),
	}).Debug("Running message through the pipeline")

	caPipeline.in <- chefAction
}

// Close the Pipeline and do all clean up
func (caPipeline *ChefActionPipeline) Close() {
	close(caPipeline.in)
}

func chefActionPipeline(source <-chan message.ChefAction, pipes ...message.ChefActionPipe) {
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for chefAction := range source {
			chefAction.FinishProcessing(nil)
		}
	}()
}
