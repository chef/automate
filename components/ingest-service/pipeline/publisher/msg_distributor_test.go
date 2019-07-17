package publisher

import (
	"context"
	"testing"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/stretchr/testify/assert"
)

// Create 3 full pipe 'in' channels and test the 'distributeMessage' function to ensure it returns
// false that the message could not be sent.
func TestDistributeMessageWithFullInChannels(t *testing.T) {
	pipeInChannels := []chan message.ChefRun{
		make(chan message.ChefRun),
		make(chan message.ChefRun),
		make(chan message.ChefRun),
	}

	errc := make(chan error)

	msg := message.NewChefRun(context.Background(), &chef.Run{}, errc)

	messageSent := distributeMessage(pipeInChannels, msg)

	assert.False(t, messageSent)
}

// Create 3 pipe 'in' channels not full and test the 'distributeMessage' function to ensure it send
// the message to the first one
func TestDistributeMessageWithAllNonFullInChannels(t *testing.T) {
	pipeInChannels := []chan message.ChefRun{
		make(chan message.ChefRun, 1),
		make(chan message.ChefRun, 1),
		make(chan message.ChefRun, 1),
	}

	errc := make(chan error)

	msg := message.NewChefRun(context.Background(), &chef.Run{}, errc)

	messageSent := distributeMessage(pipeInChannels, msg)

	assert.True(t, messageSent)
	assert.Equal(t, 1, len(pipeInChannels[0]))
	assert.Equal(t, 0, len(pipeInChannels[1]))
	assert.Equal(t, 0, len(pipeInChannels[2]))
}

// Create 3 pipe 'in' channels with the first one full and the other not full. Ensure the message
// is sent to the second channel
func TestDistributeMessageWithFirstChanFullAndOthersNonFull(t *testing.T) {
	pipeInChannels := []chan message.ChefRun{
		make(chan message.ChefRun, 0),
		make(chan message.ChefRun, 1),
		make(chan message.ChefRun, 1),
	}

	errc := make(chan error)

	msg := message.NewChefRun(context.Background(), &chef.Run{}, errc)

	messageSent := distributeMessage(pipeInChannels, msg)

	assert.True(t, messageSent)
	assert.Equal(t, 0, len(pipeInChannels[0]))
	assert.Equal(t, 1, len(pipeInChannels[1]))
	assert.Equal(t, 0, len(pipeInChannels[2]))
}

// Create 3 pipe 'in' channels with the first and second ones full and the third not full. Ensure the message
// is sent to the third channel
func TestDistributeMessageWithFirstSecondChanFullAndThirdNonFull(t *testing.T) {
	pipeInChannels := []chan message.ChefRun{
		make(chan message.ChefRun, 0),
		make(chan message.ChefRun, 0),
		make(chan message.ChefRun, 1),
	}

	errc := make(chan error)

	msg := message.NewChefRun(context.Background(), &chef.Run{}, errc)

	messageSent := distributeMessage(pipeInChannels, msg)

	assert.True(t, messageSent)
	assert.Equal(t, 0, len(pipeInChannels[0]))
	assert.Equal(t, 0, len(pipeInChannels[1]))
	assert.Equal(t, 1, len(pipeInChannels[2]))
}

// Ensure that messages that go into the 'out' pipe channels come out of the mergedOut channel
func TestDistributeMergedOutChannel(t *testing.T) {
	pipeChannels := []chan message.ChefRun{
		make(chan message.ChefRun, 100),
		make(chan message.ChefRun, 100),
		make(chan message.ChefRun, 100),
	}
	pipeOutChannels := make([]<-chan message.ChefRun, len(pipeChannels))
	for index, pipeChannel := range pipeChannels {
		pipeOutChannels[index] = pipeChannel
	}

	errc := make(chan error)

	mergedOut := mergeOutChannels(pipeOutChannels)

	msg := message.NewChefRun(context.Background(), &chef.Run{}, errc)

	pipeChannels[0] <- msg
	outMsg := <-mergedOut
	assert.Equal(t, msg.ID, outMsg.ID)

	pipeChannels[1] <- msg
	outMsg = <-mergedOut
	assert.Equal(t, msg.ID, outMsg.ID)

	pipeChannels[2] <- msg
	outMsg = <-mergedOut
	assert.Equal(t, msg.ID, outMsg.ID)
}
