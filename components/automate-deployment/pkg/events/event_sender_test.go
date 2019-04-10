// Tests for EventSender
//
// Enable printf debug statements in test code by setting environment
// variable WITH_PRINTF=1. Enable app logs with WITH_LOGS=1.
//
// Add printf debug with helper debu(fmt, args).
//
package events

import (
	"errors"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"

	api "github.com/chef/automate/api/interservice/deployment"
)

// Prefix with test to mitigate namespace pollution
var testDeploymentID = "test-deploy-1234"
var testEnableDebugPrinting = false

func setup() EventSender {
	if os.Getenv("WITH_LOGS") != "" {
		logrus.SetLevel(logrus.DebugLevel)
		logrus.SetOutput(os.Stdout)
		testEnableDebugPrinting = true
	}
	if os.Getenv("WITH_PRINTF") != "" {
		testEnableDebugPrinting = true
	}
	return NewMemoryEventSender(testDeploymentID)
}

// TODO do we need a sender.stop() function that will stop the runLoop there?
// func teardown() {
// 	sender = nil
// }

type eventClient struct {
	received []*api.DeployEvent
	ctlChan  chan string
	doneChan chan int
}

func newEventClient() *eventClient {
	return &eventClient{
		received: make([]*api.DeployEvent, 0, 10),
		ctlChan:  make(chan string),
		doneChan: make(chan int),
	}
}

func (ec *eventClient) receiveEvents(sender EventSender) {
	go func() {
		err := sender.StreamTo(ec.makeSendFun())
		debu("streamTo returned: %v", err)
		ec.doneChan <- 1
	}()
}

func (ec *eventClient) makeSendFun() func(*api.DeployEvent) error {
	return func(e *api.DeployEvent) error {
		select {
		case ctlMsg := <-ec.ctlChan:
			switch ctlMsg {
			case "stop":
				return errors.New("eventClient stopped")
			default:
				break
			}
		default:
			// nothing to do
		}
		debu("appending: %+v", e)
		ec.received = append(ec.received, e)

		return nil
	}
}

func sendEvents(es EventSender, n int) {
	for i := 0; i < n-1; i++ {
		es.Deploy(api.DeployEvent_RUNNING)
	}
	es.Deploy(api.DeployEvent_COMPLETE_OK)
	es.TaskComplete()
}

func TestStreamTo(t *testing.T) {
	t.Run("one client, concurrent start",
		func(t *testing.T) {
			sender := setup()
			client := newEventClient()
			client.receiveEvents(sender)
			go func() {
				sendEvents(sender, 5)
			}()

			// await client completion before assertions
			<-client.doneChan
			// count is off by one because of the TaskComplete event
			assert.Equal(t, 6, len(client.received))
			for i := 0; i < 5; i++ {
				assert.Equal(t, uint64(i+1), client.received[i].Sequence)
			}
		})

	t.Run("one client, late start, history received",
		func(t *testing.T) {
			sender := setup()
			go func() {
				sendEvents(sender, 6)
			}()
			time.Sleep(500 * time.Millisecond)
			client := newEventClient()
			client.receiveEvents(sender)

			// await client completion
			<-client.doneChan
			// count is off by one because of the TaskComplete event
			assert.Equal(t, 7, len(client.received))
			for i := 0; i < 6; i++ {
				assert.Equal(t, uint64(i+1), client.received[i].Sequence)
			}
		})
}

func debu(msg string, args ...interface{}) {
	if testEnableDebugPrinting {
		fmt.Printf(msg+"\n", args)
	}
}
