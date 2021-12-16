package pipeline

import (
	"errors"

	chef "github.com/chef/automate/api/external/ingest/request"

	"context"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/ingest-service/pipeline/processor"
	"github.com/chef/automate/components/ingest-service/pipeline/publisher"
	"github.com/chef/automate/components/ingest-service/serveropts"
	log "github.com/sirupsen/logrus"
)

var ErrQueueFull = errors.New("Message rejected because queue is full")

// ChefRunPipeline pipeline to process Chef client runs
type ChefRunPipeline struct {
	in      chan<- message.ChefRun
	counter *int64 // Number of messages that the pipeline has ran
}

// NewChefRunPipeline Create a new chef run pipeline
func NewChefRunPipeline(client backend.Client, authzClient authz.ProjectsServiceClient,
	nodeMgrClient manager.NodeManagerServiceClient,
	chefIngestRunPipelineConfig serveropts.ChefIngestRunPipelineConfig,
	messageBufferSize int) ChefRunPipeline {
	var (
		in            = make(chan message.ChefRun, messageBufferSize)
		counter int64 = 0
	)

	chefRunPipeline(in,
		processor.MessageValidator,
		processor.BuildTransmogrify(chefIngestRunPipelineConfig.NumberOfMsgsTransformers),
		processor.ChefRunCorrections,
		processor.BuildRunProjectTagger(authzClient),
		publisher.BuildNodeManagerPublisher(nodeMgrClient,
			chefIngestRunPipelineConfig.NumberOfNodemanagerPublishers),
		processor.BuildRunMsgToBulkRequestTransformer(client),
		publisher.BuildMsgDistributor(
			publisher.BuildBulkRunPublisher(client, chefIngestRunPipelineConfig.MaxNumberOfBundledMsgs),
			chefIngestRunPipelineConfig.NumberOfPublishers,
			chefIngestRunPipelineConfig.MaxNumberOfBundledMsgs),
		processor.CountRuns(&counter),
	)

	return ChefRunPipeline{in, &counter}
}

func (crPipeline *ChefRunPipeline) GetTotalMessages() int64 {
	return *crPipeline.counter
}

// Run a chef client run through the pipeline
func (crPipeline *ChefRunPipeline) Run(ctx context.Context, run *chef.Run, errc chan<- error) error {
	chefRun := message.NewChefRun(ctx, run, errc)

	log.WithFields(log.Fields{
		"message_id":  chefRun.ID,
		"message":     "ChefRun",
		"buffer_size": len(crPipeline.in),
	}).Debug("Running message through the pipeline")

	select {
	case crPipeline.in <- chefRun:
	default:
		return ErrQueueFull
	}
	return nil
}

// Close the Pipeline and do all clean up
func (crPipeline *ChefRunPipeline) Close() {
	close(crPipeline.in)
}

func chefRunPipeline(source <-chan message.ChefRun, pipes ...message.ChefRunPipe) {
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for chefRun := range source {
			chefRun.FinishProcessing(nil)
		}
	}()
}
