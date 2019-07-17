package publisher

import (
	"time"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

type ActivePipe struct {
	in  chan message.ChefRun
	out <-chan message.ChefRun
}

// BuildMsgDistributor a message distributor that sends all messages to the first child pipe until it is
// full then start sending messages to the next child pipe in the line. When all the pipes are full the thread waits and
// then checks to pipes again until it can place the message in a child pipe.
func BuildMsgDistributor(childPipeBuilder message.ChefRunPipe, numProcessors, childPipeInboxSize int) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return fillOnePipeAtATime(in, childPipeBuilder, numProcessors, childPipeInboxSize)
	}
}

func fillOnePipeAtATime(in <-chan message.ChefRun,
	childPipeBuilder message.ChefRunPipe, numProcessors, childPipeInboxSize int) <-chan message.ChefRun {

	out := make(chan message.ChefRun, 100)
	pipes := buildActivePipes(childPipeBuilder, numProcessors, childPipeInboxSize)
	mergeOutChannels(pipes, out)

	go func() {
		for msg := range in {
			messageProcessed := false
			for !messageProcessed {
				for _, pipe := range pipes {
					if !pipe.isFull() {
						pipe.add(msg)
						messageProcessed = true
						break
					}
				}
				// This is only called when all the pipes are full
				if !messageProcessed {
					// Wait and check the pipes again for a spot for the msg
					time.Sleep(time.Millisecond * 10)
				}
			}
		}
		close(out)
	}()

	return out
}

func mergeOutChannels(pipes []ActivePipe, out chan message.ChefRun) {
	for _, pipe := range pipes {
		go func(subPipeOut <-chan message.ChefRun) {
			for msg := range subPipeOut {
				out <- msg
			}
		}(pipe.out)
	}
}

func buildActivePipes(pipeBuilder message.ChefRunPipe, numProcessors, childPipeInboxSize int) []ActivePipe {
	activePipes := make([]ActivePipe, numProcessors)
	for index := range activePipes {
		in := make(chan message.ChefRun, childPipeInboxSize)
		out := pipeBuilder(in)

		activePipes[index] = ActivePipe{
			in:  in,
			out: out,
		}
	}

	return activePipes
}

func (pipe ActivePipe) isFull() bool {
	return len(pipe.in) == cap(pipe.in)
}

func (pipe ActivePipe) add(msg message.ChefRun) {
	pipe.in <- msg
}
