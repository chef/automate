package publisher

import (
	"time"

	"github.com/chef/automate/components/ingest-service/pipeline/message"
	log "github.com/sirupsen/logrus"
)

// BuildMsgDistributor a message distributor that sends all messages to the first child pipe until it is
// full then start sending messages to the next child pipe in the line. When all the pipes are full the thread waits and
// then checks to pipes again until it can place the message in a child pipe.
func BuildMsgDistributor(childPipeBuilder message.ChefRunPipe, numProcessors, childPipeInboxSize int) message.ChefRunPipe {
	log.WithFields(log.Fields{
		"numProcessors": numProcessors,
	}).Debug("BuildMsgDistributor")
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return msgDistributor(in, childPipeBuilder, numProcessors, childPipeInboxSize)
	}
}

func msgDistributor(in <-chan message.ChefRun,
	childPipeBuilder message.ChefRunPipe, numProcessors, childPipeInboxSize int) <-chan message.ChefRun {
	pipeInChannels, out := buildChildPipes(childPipeBuilder, numProcessors, childPipeInboxSize)

	go func() {
		for msg := range in {
			sendMessage(pipeInChannels, msg)
		}
		close(out)
	}()

	return out
}

func sendMessage(pipeInChannels []chan message.ChefRun, msg message.ChefRun) {
	for {
		messageProcessed := distributeMessage(pipeInChannels, msg)

		if messageProcessed {
			return
		}

		log.WithFields(log.Fields{
			"number_of_publishers": len(pipeInChannels),
		}).Warn("All elasticsearch publishers are full")
		time.Sleep(time.Millisecond * 10)
	}
}

// The first channel is filled before sending messages to any other channel. Once the first channel is
// filled, the second channel recieves all the messages that can not fit into the first channel,
// and so on with the rest of the channels.
// If all the channels are full then false is returned
func distributeMessage(pipeInChannels []chan message.ChefRun, msg message.ChefRun) bool {
	for _, pipeInChannel := range pipeInChannels {
		select {
		case pipeInChannel <- msg:
			return true
		default:
		}
	}
	return false
}

func mergeOutChannels(pipeOutChannels []<-chan message.ChefRun) chan message.ChefRun {
	mergedOut := make(chan message.ChefRun, 100)
	for _, pipeOut := range pipeOutChannels {
		go func(pipeOut <-chan message.ChefRun) {
			for msg := range pipeOut {
				mergedOut <- msg
			}
		}(pipeOut)
	}

	return mergedOut
}

func buildChildPipes(pipeBuilder message.ChefRunPipe, numProcessors,
	childPipeInboxSize int) ([]chan message.ChefRun, chan message.ChefRun) {
	inChannels := make([]chan message.ChefRun, numProcessors)
	outChannels := make([]<-chan message.ChefRun, numProcessors)
	for index := range inChannels {
		in := make(chan message.ChefRun, childPipeInboxSize)
		out := pipeBuilder(in)
		inChannels[index] = in
		outChannels[index] = out
	}

	mergedOut := mergeOutChannels(outChannels)

	return inChannels, mergedOut
}
