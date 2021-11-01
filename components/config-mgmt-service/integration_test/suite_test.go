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

	"github.com/pkg/errors"
	elastic "gopkg.in/olivere/elastic.v6"

	cElastic "github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/backend/postgres"
	"github.com/chef/automate/components/config-mgmt-service/config"
	grpc "github.com/chef/automate/components/config-mgmt-service/grpcserver"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	iElastic "github.com/chef/automate/components/ingest-service/backend/elastic"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/chef/automate/lib/grpc/auth_context"
	platform_config "github.com/chef/automate/lib/platform/config"
)

// Global variables
var (
	// The elasticsearch URL is coming from the environment variable ELASTICSEARCH_URL
	//elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")
	elasticsearchUrl string

	esBackend *cElastic.Backend

	// A global CfgMgmt Server instance to call any rpc function
	//
	// From any test you can directly call:
	// ```
	// res, err := cfgmgmt.GetNodesCounts(ctx, &req)
	// ```
	cfgmgmt *grpc.CfgMgmtServer
)

const (
	pgDatabaseName       = "chef_config_mgmt_service"
	A2_SVC_NAME          = "A2_SVC_NAME"
	A2_SVC_PATH          = "A2_SVC_PATH"
	defaultA2ServiceName = "config-mgmt-service"
	defaultA2ServicePath = "/hab/svc/config-mgmt-service"
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

// Just returns a new struct. You have to call GlobalSetup() to setup the
// backend connections and such.
func NewSuite() *Suite {
	return new(Suite)
}

// GlobalSetup makes backend connections to elastic and postgres. It also sets
// global vars to usable values.
func (s *Suite) GlobalSetup() error {
	// set global elasticsearchUrl
	var haveURL bool
	elasticsearchUrl, haveURL = os.LookupEnv("ELASTICSEARCH_URL")
	if !haveURL {
		return errors.New("The environment variable ELASTICSEARCH_URL must be set for integration tests to run")
	}

	// set global esBackend
	esBackend = cElastic.New(elasticsearchUrl)

	// set global cfgmgmt
	var err error
	cfgmgmt, err = newCfgMgmtServer(esBackend)

	if err != nil {
		return err
	}

	// Create a new elastic Client
	esClient, err := elastic.NewClient(
		elastic.SetURL(elasticsearchUrl),
		elastic.SetSniff(false),
	)

	if err != nil {
		return errors.Wrapf(err, "Could not create elasticsearch client from %q", elasticsearchUrl)
	}

	s.client = esClient

	iClient, err := iElastic.New(elasticsearchUrl)
	if err != nil {
		return errors.Wrapf(err, "Could not create ingest backend client from %q", elasticsearchUrl)
	}

	s.ingest = iClient
	s.ingest.InitializeStore(context.Background())

	return nil
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

	indicesToDelete := make([]string, 0)
	for _, index := range indices {
		//don't ever delete node run info.. we'll do that after each test when needed
		if index != mappings.IndexNameNodeRunInfo {
			indicesToDelete = append(indicesToDelete, index)
		}
	}
	time.Sleep(2 * time.Second)
	_, err := s.client.DeleteIndex(indicesToDelete...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'delete' ES indicesToDelete: '%s'\nError: %s", indicesToDelete, err)
		os.Exit(3)
	}
}

// newCfgMgmtServer initializes a CfgMgmtServer with the default config
// and points to our preferred backend that is elasticsearch.
//
// This function expects ES and postgres to be already up and running.
func newCfgMgmtServer(esBackend *cElastic.Backend) (*grpc.CfgMgmtServer, error) {
	_, haveSvcName := os.LookupEnv(A2_SVC_NAME)
	_, haveSvcPath := os.LookupEnv(A2_SVC_PATH)
	if !(haveSvcName && haveSvcPath) {
		_ = os.Setenv(A2_SVC_NAME, defaultA2ServiceName)
		_ = os.Setenv(A2_SVC_PATH, defaultA2ServicePath)
	}

	uri, err := platform_config.PGURIFromEnvironment(pgDatabaseName)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to get pg uri from environment variables")
	}

	cfg := config.Default()
	cfg.Postgres = config.Postgres{
		URI:        uri,
		Database:   pgDatabaseName,
		SchemaPath: "/src/components/config-mgmt-service/backend/postgres/schema/sql/",
	}

	err = postgres.DestructiveReMigrateForTest(&cfg.Postgres)
	if err != nil {
		return nil, err
	}

	cfg.SetBackend(esBackend)
	srv := grpc.NewCfgMgmtServer(cfg)
	err = srv.ConnectPg()
	if err != nil {
		return nil, err
	}
	return srv, nil
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

// IngestInitialNodes ingests a number of nodes and at the end, refreshes the node-state index
func (s *Suite) IngestInitialNodes(nodes []iBackend.UpsertNode) {
	// Insert nodes
	for _, node := range nodes {
		s.ingest.InsertUpsertNode(context.Background(), node)
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

// IngestRuns ingests a number of runs and at the end, refreshes converge-history index
func (s *Suite) IngestNodeRunDateInfo(runs []iBackend.Run) {
	// Insert runs
	for _, run := range runs {
		s.ingest.InsertNodeRunDateInfo(context.Background(), run)
	}

	// Refresh Indices
	s.RefreshIndices(mappings.ConvergeHistory.Index + "-*")
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "")
}
