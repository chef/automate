//
//  Author:: Salim Afiune <afiune@chef.io>, Gina Peers <gpeers@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"os"

	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	fserver "github.com/chef/automate/components/compliance-service/api/automate-feed/server"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/feed/persistence"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	olivere "github.com/olivere/elastic"
)

// Suite helps you manipulate various stages of your tests. It provides
// common functionality like initialization and deletion hooks and more.
// If you have some functionality that repeats across multiple tests,
// consider putting it here so that we have it available to the Feed Service
// at a global level.
//
// This struct holds:
// * A Feeds backend client (FeedStore) for feeds persistence operations.
// * An Elasticsearch client for ES queries.
//   => Docs: https://godoc.org/gopkg.in/olivere/elastic.v5
type Suite struct {
	feedServer  automate_feed.FeedServiceServer
	feedBackend persistence.FeedStore
	esBackend   relaxting.ES2Backend
	esClient    *olivere.Client
	indices     []string
	types       []string
}

// Initialize the test suite
//
// TODO: add check for Elasticsearch connectivity.
// If we can't connect, we'll skip the tests and
// print an error message
func NewSuite(url string) *Suite {
	s := new(Suite)

	esBackend := relaxting.ES2Backend{
		ESUrl: elasticsearchUrl,
	}

	// for non-grpc calls
	s.feedBackend = persistence.NewFeedStore(&esBackend)

	// create olivere elastic client
	client, err := esBackend.ES2Client()

	if err != nil {
		fmt.Printf("Could not create Elasticsearch client from '%s': %s\n", url, err)
		os.Exit(1)
	}

	s.esClient = client
	s.indices = []string{mappings.IndexNameFeeds}
	s.types = []string{mappings.DocType}

	s.feedServer = fserver.New(&config.FeedConfig{}, &esBackend)
	return s
}

// Set up global test fixtures
func (s *Suite) GlobalSetup() {
}

// Tear down global test fixtures
func (s *Suite) GlobalTeardown() {
	// Make sure we clean them all!

	toDelete := s.verifyIndices(s.indices...)
	// if there are no valid Indices, stop processing
	if len(toDelete) == 0 {
		return
	}

	_, err := s.esClient.DeleteIndex(toDelete...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'delete' ES indices: '%s'\nError: %s", s.indices, err)
		os.Exit(3)
	}
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
	q := olivere.RawStringQuery("{\"match_all\":{}}")

	// Make sure we clean them all!
	indices := s.indices

	_, err := s.esClient.DeleteByQuery().
		Index(indices...).
		Type(s.types...).
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

func (s *Suite) RefreshIndices(indices ...string) {
	// Verify that the provided indices exists, if not remove them
	indices = s.verifyIndices(indices...)

	// If there are no valid Indices, stop processing
	if len(indices) == 0 {
		return
	}

	_, err := s.esClient.Refresh(indices...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'refresh' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

// verifyIndices receives a list of indices and verifies that they exist.
// If an index doesn't exist, it is removed from the list. Only existing
// indices are returned.
func (s *Suite) verifyIndices(indices ...string) []string {
	var validIndices = make([]string, 0)

	for _, index := range indices {
		if s.indexExists(index) {
			validIndices = append(validIndices, index)
		}
	}

	return validIndices
}

func (s *Suite) indexExists(i string) bool {
	exists, _ := s.esClient.IndexExists(i).Do(context.Background())
	return exists
}
