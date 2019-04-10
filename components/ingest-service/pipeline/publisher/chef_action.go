package publisher

import (
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// BuildChefAction Builds the publishers
func BuildChefAction(client backend.Client, numberOfPublishers int) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		out := make(chan message.ChefAction, 100)

		for i := 0; i < numberOfPublishers; i++ {
			ChefAction(in, client, out, i)
		}

		return out
	}
}

// ChefAction The Chef action publisher pipe. This stores the chef action information in the datastore.
func ChefAction(in <-chan message.ChefAction, client backend.Client, out chan<- message.ChefAction, number int) {
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"publisher_id": number,
				"message_id":   msg.ID,
				"buffer_size":  len(out),
			}).Debug("Publishing ChefAction")

			err := client.InsertAction(msg.Ctx, msg.InternalChefAction)
			if err != nil {
				msg.FinishProcessing(status.Errorf(codes.Internal, err.Error()))
			} else {
				out <- msg
			}
		}
		close(out)
	}()
}
