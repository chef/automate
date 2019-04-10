package integration_test

import (
	"context"
	"fmt"
	"os"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	olivere "github.com/olivere/elastic"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/event-service/server"
)

type Suite struct {
}

func NewSuite() *Suite {
	s := new(Suite)
	return s
}

func (s *Suite) GlobalSetup() {
	doInit()
}

func (s *Suite) GlobalTeardown() {
}

// for integration testing with feed service
func (s *Suite) deleteAllDocuments() {
	// ES query to match all documents
	q := olivere.RawStringQuery("{\"match_all\":{}}")

	_, err := esClient.DeleteByQuery().
		Index(indices...).
		Type(types...).
		Query(q).
		IgnoreUnavailable(true).
		Refresh("true").
		WaitForCompletion(true).
		Do(context.Background())

	if err != nil {
		fmt.Printf("Could not delete ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(2)
	}
}

func (s *Suite) refreshIndices(indices ...string) {
	_, err := esClient.Refresh(indices...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'refresh' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

func (s *Suite) createEvents(amountToCreate int) []*api.EventMsg {
	var events = []*api.EventMsg{}

	for i := 0; i < amountToCreate; i++ {
		event := &api.EventMsg{
			EventID: uuid.Must(uuid.NewV4()).String(),
			Type:    &api.EventType{Name: server.ScanJobCreated},
			Producer: &api.Producer{
				ID:           "urn:chef:compliance:scan-component",
				ProducerName: "Scanner",
				ProducerType: "system component",
			},
			Tags:      []string{"scanjobs", "create"},
			Published: ptypes.TimestampNow(),
			Actor: &api.Actor{
				ID:          "urn:mycompany:user:fred",
				ObjectType:  "User",
				DisplayName: "Fred",
			},
			Verb: "create",
			Object: &api.Object{
				ID:          uuid.Must(uuid.NewV4()).String(),
				ObjectType:  "scanjobs", // entity types are scanjobs, profile
				DisplayName: "Scan Job",
			},
			Target: &api.Target{
				ID:          "urn:mycompany:environment:production",
				ObjectType:  "Environment",
				DisplayName: "Production",
			},
		}
		events = append(events, event)
	}
	return events
}
