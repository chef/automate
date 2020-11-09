package processor

import (
	elastic "github.com/olivere/elastic/v7"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

func BuildRunMsgToBulkRequestTransformer(client backend.Client) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return runMsgToBulkRequestTransformer(in, client)
	}
}

func runMsgToBulkRequestTransformer(in <-chan message.ChefRun, client backend.Client) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 2000)
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
				"message":     "ChefRun",
			}).Debug("Transforming to bulk")

			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
				continue
			}

			msg.BulkableRequests = []elastic.BulkableRequest{
				client.CreateBulkRunUpdateRequest(msg.NodeRun),
				client.CreateBulkNodeUpdateRequest(msg.Node),
				client.CreateBulkNodeAttributeUpdateRequest(msg.NodeAttribute),
			}

			message.PropagateChefRun(out, &msg)
		}
		close(out)
	}()

	return out
}
