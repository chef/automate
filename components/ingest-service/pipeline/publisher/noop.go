package publisher

import (
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

func noop(in <-chan message.ChefRun) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 10)
	go func() {
		for msg := range in {
			message.PropagateChefRun(out, &msg)
		}
	}()
	return out
}

func actionsNoop(in <-chan message.ChefAction) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 10)
	go func() {
		for msg := range in {
			message.PropagateChefAction(out, &msg)
		}
	}()
	return out
}
