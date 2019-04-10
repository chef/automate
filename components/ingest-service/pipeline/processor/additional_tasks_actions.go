package processor

import (
	"context"

	log "github.com/sirupsen/logrus"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

func BuildChefActionPerform(client backend.Client) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		return chefActionPerform(in, client)
	}
}

func chefActionPerform(in <-chan message.ChefAction, client backend.Client) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 100)
	go func() {
		for msg := range in {

			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"action":      msg.Action.Task,
				"entity_type": msg.Action.EntityType,
				"message":     "ChefAction",
				"buffer_size": len(out),
			}).Debug("Performing action")

			action := msg.Action

			if action.GetTask() == "delete" && action.GetEntityType() == "node" {
				deleted, err := ProcessNodeDelete(msg.Ctx, action, client)

				if err != nil {
					log.WithFields(log.Fields{"error": err.Error()}).Error("Message failure")
				} else {
					log.WithFields(log.Fields{
						"message_type":    "delete_node",
						"number_deleted:": deleted,
					}).Debug("Marked nodes for deletion")
				}
			}
			if filterSelfCCR(msg.Ctx, action, client) {
				msg.FinishProcessing(nil)
			} else {
				out <- msg
			}
		}
		close(out)
	}()

	return out
}

func filterSelfCCR(ctx context.Context, action *chef.Action, client backend.Client) bool {
	if action.GetTask() == "update" &&
		action.GetEntityType() == "node" &&
		action.GetEntityName() == action.GetRequestorName() {
		log.WithFields(log.Fields{
			"message_type": "update node",
		}).Debug("Not persisting self updated node action")
		return true
	}
	return false
}

// ProcessNodeDelete
func ProcessNodeDelete(ctx context.Context, action *chef.Action, client backend.Client) (int, error) {
	if action.GetNodeId() != "" {
		return client.DeleteNodeByID(ctx, action.GetNodeId())
	}

	return client.DeleteNodeByFields(ctx,
		action.GetOrganizationName(),
		action.GetServiceHostname(),
		action.GetEntityName())
}
