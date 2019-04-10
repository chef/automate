package processor

import (
	"github.com/olivere/elastic"
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
			msg.BulkableRequests = []elastic.BulkableRequest{
				client.CreateBulkRunUpdateRequest(msg.NodeRun),
				client.CreateBulkNodeUpdateRequest(msg.Node),
				client.CreateBulkNodeAttributeUpdateRequest(msg.NodeAttribute),
			}

			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
				"message":     "ChefRun",
			}).Debug("Transforming to bulk")

			out <- msg
		}
		close(out)
	}()

	return out
}

func BuildActionMsgToBulkRequestTransformer(client backend.Client) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		return actionMsgToBulkRequestTransformer(in, client)
	}
}

func actionMsgToBulkRequestTransformer(in <-chan message.ChefAction, client backend.Client) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 1000)
	go func() {
		for msg := range in {
			msg.BulkableRequests = []elastic.BulkableRequest{
				client.CreateBulkActionRequest(msg.InternalChefAction),
			}

			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
				"message":     "ChefAction",
			}).Debug("Transforming to bulk")

			out <- msg
		}
		close(out)
	}()

	return out
}
