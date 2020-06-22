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
