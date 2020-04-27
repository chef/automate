package processor

import (
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

// ChefRunCorrections - This processor makes updates and corrections to the raw data from the chef run message.
func ChefRunCorrections(in <-chan message.ChefRun) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 100)
	go func() {
		for msg := range in {
			log.WithFields(log.Fields{
				"message_id":  msg.ID,
				"buffer_size": len(out),
			}).Debug("Corrections ChefRun")

			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessing(err)
				continue
			}

			cleanUpErrorTitle(&msg)

			message.PropagateChefRun(out, &msg)
		}
		close(out)
	}()

	return out
}

// This method cleans up an error message coming from chef server.
// Because 412 "Precondition Failed" is not user friendly, the message
// was changed to "Error Resolving Cookbooks for Run List."
func cleanUpErrorTitle(chefRun *message.ChefRun) {
	if chefRun.NodeRun.Error.Description.Title == "412 \"Precondition Failed\"" ||
		chefRun.NodeRun.Error.Description.Title == "Error Resolving Cookbooks for Run List:" {
		chefRun.NodeRun.Error.Description.Title = "Error Resolving Cookbooks for Run List."
	}
}
