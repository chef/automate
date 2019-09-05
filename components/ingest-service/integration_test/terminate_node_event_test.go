package integration_test

import (
	"context"
	"testing"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"

	aEvent "github.com/chef/automate/api/interservice/event"
	event "github.com/chef/automate/components/event-service/config"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

func TestMarkForDeleteNodeTerminated(t *testing.T) {
	var (
		ctx        = context.Background()
		instanceID = "instanceid"
		event      = &aEvent.EventMsg{
			EventID: uuid.Must(uuid.NewV4()).String(),
			Type:    &aEvent.EventType{Name: event.NodeTerminatedEventName},
			Producer: &aEvent.Producer{
				ID:           "urn:chef:compliance:mgrpolling",
				ProducerName: "Node Manager Polling",
				ProducerType: "system component",
			},
			Tags:      []string{},
			Published: ptypes.TimestampNow(),
			Actor: &aEvent.Actor{
				ID:          "",
				ObjectType:  "nodemanager",
				DisplayName: "nodemanager",
			},
			Verb: "terminate",
			Object: &aEvent.Object{
				ID:          instanceID,
				ObjectType:  "instance ID",
				DisplayName: instanceID,
			},
			Target: &aEvent.Target{
				ID:          "",
				ObjectType:  "Not Applicable",
				DisplayName: "Not Applicable",
			},
		}
		nodeID = newUUID()
		node   = []iBackend.Node{
			iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: nodeID,
				},
				Exists:  true,
				CloudID: instanceID,
			},
		}
	)

	suite.IngestNodes(node)
	defer suite.DeleteAllDocuments()

	// One node before node termination
	actualNodes, err := suite.GetNodes(1)
	assert.NoError(t, err)
	assert.Equal(t, 1, len(actualNodes))

	_, err = suite.EventHandlerServer.HandleEvent(ctx, event)
	assert.NoError(t, err)
	suite.RefreshIndices(mappings.NodeState.Index)

	// zero nodes after node terminated event deletes node
	actualNodes, err = suite.GetNodes(1)
	assert.NoError(t, err)
	assert.Equal(t, 0, len(actualNodes))
}
