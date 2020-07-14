package publisher

import (
	"time"

	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

func BuildRolloutsPublisher(client cfgmgmt.CfgMgmtClient, numPublishers int) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		if numPublishers <= 0 || client == nil {
			log.Info("Direct publish to config management service is disabled")
			return actionsNoop(in)
		}

		out := make(chan message.ChefAction, 100)
		log.Infof("Starting config-management-service publisher")
		for i := 0; i < numPublishers; i++ {
			go rolloutPublisher(in, client, out)
		}
		return out
	}
}

func rolloutPublisher(in <-chan message.ChefAction, client cfgmgmt.CfgMgmtClient, out chan<- message.ChefAction) {
	for msg := range in {
		handleMessage(client, msg, out)

	}
	close(out)
}

func handleMessage(client cfgmgmt.CfgMgmtClient, msg message.ChefAction, out chan<- message.ChefAction) {
	if err := msg.Ctx.Err(); err != nil {
		msg.FinishProcessing(err)
		return
	}

	defer message.PropagateChefAction(out, &msg)

	if !isPolicyUpdateAction(msg) {
		return
	}

	start := time.Now()

	err := publishZeData(client, msg)

	logCtx := log.WithFields(log.Fields{
		"message_id":  msg.ID,
		"buffer_size": len(out),
		"dur":         time.Since(start),
	})

	if err != nil {
		logCtx.WithError(err).Error("Failed to publish actions message to config management service")
		return
	}
	logCtx.Debug("Published actions message to config manangement service")
	return
}

func isPolicyUpdateAction(msg message.ChefAction) bool {
	// We only handle policyfile pushes to the combined policyfile push endpoint,
	// which is $chef_server_url/policy_groups/$policy_group/policies/$policy_name
	// When using this endpoint, Chef Server sends the policy group name as the
	// "parent" entity.
	// Chef Server offers other policyfile APIs but these are not used in practice.

	// entity_name=jenkins entity_task=update entity_type=policy parent_name=foo3 parent_type=policy_group type=ingest_time
	a := msg.InternalChefAction
	return a.EntityType == "policy" && a.ParentType == "policy_group"
}

func publishZeData(client cfgmgmt.CfgMgmtClient, message message.ChefAction) error {
	// TODO: put some code here :) ... or inline it into handleMessage
	// We can get the policy group from the ParentName field when the
	// ParentType is "policy_group".
	// It comes from here-ish: https://github.com/chef/chef-server/blob/1dd58e0236cdaa21014b356758f53d6c080fc28e/src/oc_erchef/apps/oc_chef_wm/src/oc_chef_action.erl#L219
	// parent_name=foo3 parent_type=policy_group
	return nil
}
