package pipeline

import (
	"context"

	log "github.com/sirupsen/logrus"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/interservice/authz"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/ingest-service/pipeline/processor"
	"github.com/chef/automate/components/ingest-service/pipeline/publisher"
)

// ChefActionPipeline pipeline to process Chef actions from client or server
type ChefActionPipeline struct {
	in      chan<- message.ChefAction
	counter *int64 // Number of messages that the pipeline has ran
}

// NewChefActionPipeline Create a new chef action pipeline
func NewChefActionPipeline(client backend.Client, authzClient authz.ProjectsServiceClient,
	configMgmtClient cfgmgmt.CfgMgmtServiceClient, eventFeedServiceClient event_feed.EventFeedServiceClient,
	messageBufferSize, maxNumberOfBundledActionMsgs int) ChefActionPipeline {

	var (
		in            = make(chan message.ChefAction, messageBufferSize)
		counter int64 = 0
	)

	chefActionPipeline(in,
		processor.BuildChefActionPerform(client),
		processor.ChefActionTransmogrify,
		processor.BuildActionProjectTagger(authzClient),
		publisher.BuildEventPublisher(eventFeedServiceClient, maxNumberOfBundledActionMsgs),
		publisher.BuildConfigMgmtPublisher(configMgmtClient, maxNumberOfBundledActionMsgs),
		processor.CountActions(&counter),
	)

	return ChefActionPipeline{in, &counter}
}

func (caPipeline *ChefActionPipeline) GetTotalMessages() int64 {
	return *caPipeline.counter
}

// Run a chef client action through the pipeline
func (caPipeline *ChefActionPipeline) Run(ctx context.Context, action *chef.Action, errc chan<- error) error {
	chefAction := message.NewChefAction(ctx, action, errc)

	log.WithFields(log.Fields{
		"message_id":  chefAction.ID,
		"message":     "ChefAction",
		"buffer_size": len(caPipeline.in),
	}).Debug("Running message through the pipeline")

	select {
	case caPipeline.in <- chefAction:
	default:
		return ErrQueueFull
	}
	return nil
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
