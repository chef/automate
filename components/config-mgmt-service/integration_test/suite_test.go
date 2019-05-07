//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"os"
	"time"

	iBackend "github.com/chef/automate/components/ingest-service/backend"
	iElastic "github.com/chef/automate/components/ingest-service/backend/elastic"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"
)

// Suite helps you manipulate various stages of your tests, it provides
// common functionality like; Initialization and deletion hooks, ingestion of
// Nodes, Runs, and more. If you have some functionality that is repetitive across
// multiple tests, consider putting it here so that we have them available globally
//
// This struct holds:
// * A Ingest backend client, that you can leverate to do all sorts of ingestion.
//   => Check this for the list of things this client can do:
//      https://github.com/github.com/chef/automate/components/ingest-service/blob/master/backend/client.go#L1
// * A Elasticsearch client, that you can use to throw ES queries.
//   => Docs: https://godoc.org/gopkg.in/olivere/elastic.v5
type Suite struct {
	ingest iBackend.Client
	client *elastic.Client
}

// Initialize the test suite
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the tests and print an error message
func NewSuite(url string) *Suite {
	s := new(Suite)

	// Create a new elastic Client
	esClient, err := elastic.NewClient(
		elastic.SetURL(url),
		elastic.SetSniff(false),
	)

	if err != nil {
		fmt.Printf("Could not create elasticsearch client from '%s': %s\n", url, err)
		os.Exit(1)
	}

	s.client = esClient

	iClient, err := iElastic.New(elasticsearchUrl)
	if err != nil {
		fmt.Printf("Could not create ingest backend client from '%s': %s\n", url, err)
		os.Exit(3)
	}

	s.ingest = iClient
	return s
}

// GlobalSetup is the place where you prepare anything that we need before
// executing all our test suite, at the moment we are just initializing ES Indices
func (s *Suite) GlobalSetup() {
	s.ingest.InitializeStore(context.Background())
}

// GlobalTeardown is the place where you tear everything down after we have finished
// executing all our test suite, at the moment we are just deleting ES Indices
func (s *Suite) GlobalTeardown() {
	// Make sure we clean them all!
	indices, _ := s.client.IndexNames()

	// If there are no valid Indices, stop processing
	if len(indices) == 0 {
		return
	}

	_, err := s.client.DeleteIndex(indices...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'delete' ES indices: '%s'\nError: %s", indices, err)
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

// indexExists tells you if an ES Index exists or not
func (s *Suite) indexExists(i string) bool {
	exists, _ := s.client.IndexExists(i).Do(context.Background())
	return exists
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

// IngestRuns ingests a number of runs and at the end, refreshes converge-history index
func (s *Suite) IngestRuns(runs []iBackend.Run) {
	// Insert runs
	for _, run := range runs {
		s.ingest.InsertRun(context.Background(), run)
	}

	// Refresh Indices
	s.RefreshIndices(mappings.ConvergeHistory.Index + "-*")
}

// IngestNodeAttributes ingests a number of node attribute(s) and at the end, refreshes converge-history index
func (s *Suite) IngestNodeAttributes(nodeAttributes []iBackend.NodeAttribute) {
	// Insert node attribute(s)
	for _, attribute := range nodeAttributes {
		s.ingest.InsertNodeAttribute(context.Background(), attribute)
	}

	// Refresh Indices
	s.RefreshIndices(mappings.NodeAttribute.Index)
}

// IngestActions ingests a number of chef-server action(s) and at the end, refreshes actions index
func (s *Suite) IngestActions(actions []iBackend.InternalChefAction) {
	// Insert action(s)
	for _, action := range actions {
		err := s.ingest.InsertAction(context.Background(), action)
		if err != nil {
			log.WithFields(log.Fields{"error": err}).Error("Error ingesting action")
		}
	}

	// Refresh Indices
	s.RefreshIndices(mappings.Actions.Index + "-*")
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}
