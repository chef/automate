//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package integration_test

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	cfgBackend "github.com/chef/automate/components/config-mgmt-service/backend"
	cfgElastic "github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	iElastic "github.com/chef/automate/components/ingest-service/backend/elastic"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/components/ingest-service/server"
	"github.com/golang/mock/gomock"
	"github.com/olivere/elastic"
	"github.com/spf13/viper"
	"google.golang.org/grpc"
)

// TODO @afiune most of this file is very similar to the suite_test.go living
// inside config-mgmt-service, we could have a single suite file for both (or more)

// Suite helps you manipulate various stages of your tests, it provides
// common functionality like; Initialization and deletion hooks, ingestion of
// Nodes, Runs, and more. If you have some functionality that is repetitive across
// multiple tests, consider putting it here so that we have them available globally
//
// This struct holds:
// * A JobScheduler, properly the internal job scheduler for the ingest service, required for the JobSchedulerServer
// * A ConfigManager, the configuration manager for the service, required for the JobSchedulerServer
// * A ChefIngestServer, the exposed GRPC Server that ingest Chef Data
// * A JobSchedulerServer, the exposed GRPC Server to start, stop, configure and run jobs
// * A CfgMgmt backend client, that you can leverate to verify Chef Data.
//   => Check this for the list of things this client can do:
//      https://github.com/github.com/chef/automate/components/config-mgmt-service/blob/master/backend/client.go#L1
// * An Ingest backend client, that you can leverate to do all sorts of ingestion.
//   => Check this for the list of things this client can do:
//      https://github.com/github.com/chef/automate/components/ingest-service/blob/master/backend/client.go#L1
// * An Elasticsearch client, that you can use to throw ES queries.
//   => Docs: https://godoc.org/gopkg.in/olivere/elastic.v5
type Suite struct {
	JobScheduler           *server.JobScheduler
	ConfigManager          *config.Manager
	ChefIngestServer       *server.ChefIngestServer
	JobSchedulerServer     *server.JobSchedulerServer
	EventHandlerServer     *server.AutomateEventHandlerServer
	cfgmgmt                cfgBackend.Client
	ingest                 iBackend.Client
	client                 *elastic.Client
	projectsClient         *iam_v2.MockProjectsClient
	eventServiceClientMock *EventServiceClientMock
}

type EventServiceClientMock struct {
	LastEventSent *automate_event.EventMsg
}

func (esc *EventServiceClientMock) Publish(ctx context.Context, in *automate_event.PublishRequest,
	opts ...grpc.CallOption) (*automate_event.PublishResponse, error) {
	esc.LastEventSent = in.Msg
	return &automate_event.PublishResponse{}, nil
}
func (esc *EventServiceClientMock) Subscribe(ctx context.Context, in *automate_event.SubscribeRequest,
	opts ...grpc.CallOption) (*automate_event.SubscribeResponse, error) {
	return &automate_event.SubscribeResponse{}, nil
}
func (esc *EventServiceClientMock) Start(ctx context.Context, in *automate_event.StartRequest,
	opts ...grpc.CallOption) (*automate_event.StartResponse, error) {
	return &automate_event.StartResponse{}, nil
}
func (esc *EventServiceClientMock) Stop(ctx context.Context, in *automate_event.StopRequest,
	opts ...grpc.CallOption) (*automate_event.StopResponse, error) {
	return &automate_event.StopResponse{}, nil
}

// Initialize the test suite
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the tests and print an error message
//
// NOTE: This function expects ES to be already up and running.
// (@afiune) We are going to start ES from the studio
func NewSuite(url string) *Suite {
	s := new(Suite)

	// Create a new elastic Client
	esClient, err := elastic.NewClient(
		elastic.SetURL(url),
		elastic.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch client from %q: %s\n", url, err)
		os.Exit(1)
	}

	s.client = esClient
	s.cfgmgmt = cfgElastic.New(elasticsearchUrl)
	iClient, err := iElastic.New(elasticsearchUrl)
	if err != nil {
		fmt.Printf("Could not create ingest backend client from %q: %s\n", url, err)
		os.Exit(3)
	}

	s.ingest = iClient
	s.projectsClient = iam_v2.NewMockProjectsClient(gomock.NewController(nil))
	s.projectsClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ProjectCollectionRulesResp{}, nil)

	s.eventServiceClientMock = &EventServiceClientMock{}

	// TODO @afiune Modify the time of the jobs
	s.JobScheduler = server.NewJobScheduler()
	s.ConfigManager = config.NewManager()
	// TODO Handle the Close() functions
	//defer JobScheduler.Close()
	//defer ConfigManager.Close()

	// A global ChefIngestServer instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := suite.ChefIngestServer.ProcessChefAction(ctx, &req)
	// ```
	s.ChefIngestServer = server.NewChefIngestServer(s.ingest, s.projectsClient)
	s.EventHandlerServer = server.NewAutomateEventHandlerServer(iClient, *s.ChefIngestServer,
		s.projectsClient, s.eventServiceClientMock)

	// A global JobSchedulerServer instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// // To test the 'marked nodes missing' job
	// res, err := suite.JobSchedulerServer.MarkNodesMissing(ctx, &req)
	//
	// // To test the 'mark missing nodes for deletion' job
	// res, err := suite.JobSchedulerServer.MarkMissingNodesForDeletion(ctx, &req)
	// ```
	s.JobSchedulerServer = server.NewJobSchedulerServer(s.ingest, s.JobScheduler, s.ConfigManager)

	return s
}

// GlobalSetup is the place where you prepare anything that we need before
// executing all our test suite, at the moment we are just initializing ES Indices
func (s *Suite) GlobalSetup() {
	// TODO @afiune We really need to fix this with AIA-159, we should automatically create indices
	s.ingest.InitializeStore(context.Background())
	viper.SetConfigFile(cFile)
}

// GlobalTeardown is the place where you tear everything down after we have finished
// executing all our test suite
func (s *Suite) GlobalTeardown() {
	os.Remove(cFile)
}

// GetNode retrives a Chef Node
func (s *Suite) GetNode(id string) (cfgBackend.Node, error) {
	return s.cfgmgmt.GetNode(id)
}

// GetNodes retrives X Chef Nodes
func (s *Suite) GetNodes(x int) ([]cfgBackend.Node, error) {
	filterMap := make(map[string][]string, 0)
	return s.cfgmgmt.GetNodes(1, x, "node_name", true, filterMap)
}

// GetNonExistingNodes retrives X Chef Nodes that doesn't actually exist :thinking:
//
// We need this custom function since we need a way to verify the nodes we update and
// we can't use the 'cfgmgmt.GetInventoryNodes()' because it doesn't get all the fields
// ex. 'node.timestamp'
func (s *Suite) GetNonExistingNodes(x int) ([]cfgBackend.Node, error) {
	searchResult, err := s.client.Search().
		Index(mappings.NodeState.Index).
		From(0).Size(x). // take documents from {start} to {perPage}
		Do(context.Background())
	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var nodes []cfgBackend.Node
	var n cfgBackend.Node
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			err := json.Unmarshal(*hit.Source, &n)
			if err != nil {
				fmt.Printf("Error unmarshalling the node object: %s", err)
			} else {
				nodes = append(nodes, n)
			}
		}
	}

	return nodes, nil
}

// IngestNodes ingests a number of nodes and at the end, refreshes the node-state index
func (s *Suite) IngestNodes(nodes []iBackend.Node) {
	// Insert nodes
	for _, node := range nodes {
		s.ingest.InsertNode(context.Background(), node)
	}

	// Refresh Indices
	s.RefreshIndices(mappings.NodeState.Index)
}

// RefreshIndices will refresh the provided ES Index or list of Indices
//
// Example 1: To refresh a single index, the node-state index
// ```
// suite.RefreshIndices(mappings.NodeState.Index)
// ```
//
// Example 2: To refresh all indices
// ```
// suite.RefreshIndices(suite.Indices()...)
// ```
func (s *Suite) RefreshIndices(indices ...string) {
	// Verify that the provided indices exists, if not remove them
	indices = s.verifyIndices(indices...)

	// If there are no valid Indices, stop processing
	if len(indices) == 0 {
		return
	}

	_, err := s.client.Refresh(indices...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'refresh' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

// verifyIndices receives a list of indices and verifies that they exist,
// if the index doesn't exist, it removes it from the list and returns only
// the valid indices that exists in ES
func (s *Suite) verifyIndices(indices ...string) []string {
	var validIndices = make([]string, 0)

	for _, index := range indices {
		if s.indexExists(index) {
			validIndices = append(validIndices, index)
		}
	}

	return validIndices
}

// indexExists tells you if an ES Index exists or not
func (s *Suite) indexExists(i string) bool {
	exists, _ := s.client.IndexExists(i).Do(context.Background())
	return exists
}

// DeleteAllDocuments will clean every single document from all ES Indices
//
// You should call this method on every single test as the following example:
// ```
//  func TestGrpcFunc(t *testing.T) {
//    // Here we are ingesting a number of nodes
//    suite.IngestNodes(nodes)
//
//    // Immediately after the ingestion add the hook to clean all documents,
//    // by using `defer` you will ensure that the next test will have clean
//    // data regardless if this test passes or fails
//    defer suite.DeleteAllDocuments()
//  }
// ```
func (s *Suite) DeleteAllDocuments() {
	// ES Query to match all documents
	q := elastic.RawStringQuery("{\"match_all\":{}}")

	// Make sure we clean them all!
	indices, _ := s.client.IndexNames()

	_, err := s.client.DeleteByQuery().
		Index(indices...).
		Type(s.types()...).
		Query(q).
		IgnoreUnavailable(true).
		Refresh("true").
		WaitForCompletion(true).
		Do(context.Background())

	if err != nil {
		fmt.Printf("Could not 'clean' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

// types returns the list of ES types registered in the Ingest service code base
func (s *Suite) types() []string {
	types := make([]string, len(mappings.AllMappings))

	for i, esMap := range mappings.AllMappings {
		types[i] = esMap.Type
	}

	return types
}

// Indices returns the list of ES indices registered in the Ingest service code base
func (s *Suite) Indices() []string {
	indices := make([]string, len(mappings.AllMappings))

	for i, esMap := range mappings.AllMappings {
		if esMap.Timeseries {
			indices[i] = esMap.IndexTimeseriesFmt(time.Now())
		} else {
			indices[i] = esMap.Index
		}
	}

	return indices
}
