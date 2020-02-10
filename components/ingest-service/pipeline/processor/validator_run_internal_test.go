package processor

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

// The message should pass through the pipe
func TestMessageValidator(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)

	errc := make(chan error)

	run := &chef.Run{
		EntityUuid: "node_id",
		RunId:      "run_id",
	}

	inbox <- message.NewChefRun(context.Background(), run, errc)
	close(inbox)

	out := MessageValidator(inbox)

	<-out
}

func TestMessageValidatorMissingEntityUUID(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)

	errc := make(chan error)

	run := &chef.Run{
		EntityUuid: "", // missing
		RunId:      "run_id",
	}

	inbox <- message.NewChefRun(context.Background(), run, errc)
	close(inbox)

	MessageValidator(inbox)

	err := <-errc

	assert.Error(t, err)
}

func TestMessageValidatorMissingRunId(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)

	errc := make(chan error)

	run := &chef.Run{
		EntityUuid: "node_id",
		RunId:      "", // missing
	}

	inbox <- message.NewChefRun(context.Background(), run, errc)
	close(inbox)

	MessageValidator(inbox)

	err := <-errc

	assert.Error(t, err)
}
