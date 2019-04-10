package pipeline

import (
	chef "github.com/chef/automate/api/external/ingest/request"

	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/ingest-service/pipeline/processor"
	"github.com/chef/automate/components/ingest-service/pipeline/publisher"
	log "github.com/sirupsen/logrus"
)

// ChefRunPipeline pipeline to process Chef client runs
type ChefRunPipeline struct {
	in      chan<- message.ChefRun
	counter *int64 // Number of messages that the pipeline has ran
}

// NewChefRunPipeline Create a new chef run pipeline
func NewChefRunPipeline(client backend.Client, authzClient iam_v2.ProjectsClient) ChefRunPipeline {
	var (
		in            = make(chan message.ChefRun, 100)
		counter int64 = 0
	)

	chefRunPipeline(in,
		processor.BuildTransmogrify(9),
		processor.ChefRunCorrections,
		processor.BuildRunProjectTagger(authzClient),
		processor.BuildRunMsgToBulkRequestTransformer(client),
		publisher.BuildBulkRunPublisher(client),
		processor.CountRuns(&counter),
	)

	return ChefRunPipeline{in, &counter}
}

func (crPipeline *ChefRunPipeline) GetTotalMessages() int64 {
	return *crPipeline.counter
}

// Run a chef client run through the pipeline
func (crPipeline *ChefRunPipeline) Run(run *chef.Run, errc chan<- error) {
	chefRun := message.NewChefRun(context.Background(), run, errc)

	log.WithFields(log.Fields{
		"message_id":  chefRun.ID,
		"message":     "ChefRun",
		"buffer_size": len(crPipeline.in),
	}).Debug("Running message through the pipeline")

	crPipeline.in <- chefRun
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
