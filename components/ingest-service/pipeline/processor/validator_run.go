package processor

import (
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

// MessageValidator - Validate the message before it moves further in the pipeline.
func MessageValidator(in <-chan message.ChefRun) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 100)
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
			}).Debug("MessageValidator ChefRun")

			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
				continue
			}

			if msg.Run.EntityUuid == "" {
				msg.FinishProcessing(status.Errorf(codes.InvalidArgument, "the entity_uuid is missing from the message"))
				continue
			}

			if msg.Run.RunId == "" {
				msg.FinishProcessing(status.Errorf(codes.InvalidArgument, "the run_id is missing from the message"))
				continue
			}

			message.PropagateChefRun(out, &msg)
		}
		close(out)
	}()

	return out
}
