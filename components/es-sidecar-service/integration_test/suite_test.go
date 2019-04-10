package integration_test

import (
	"context"
	"fmt"
	"os"

	es_sidecar "github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/components/es-sidecar-service/pkg/server"
	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"
)

const (
	// Elasticsearch indexes
	TimeSeriesTestIndex = "test-index"
	TimeSeriesDaysBack  = 15
)

type Suite struct {
	esURL string
	// direct ES connection for any needed setup, verification, cleanup
	esClient    *elastic.Client
	esSidecar   *es_sidecar.Elastic
	purgeServer *server.EsSidecarServer
}

func NewSuite(url string) *Suite {
	s := new(Suite)

	log.SetLevel(log.DebugLevel)
	log.SetOutput(os.Stderr)

	esClient, err := elastic.NewClient(
		elastic.SetURL(url),
	)

	if err != nil {
		fmt.Printf("Could not create elasticsearch client for url '%s': %s\n", url, err)
		os.Exit(1)
	}

	esSidecar, err := es_sidecar.New(url)
	if err != nil {
		fmt.Printf("Could not create sidecar ES client for url '%s': %s\n", url, err)
		os.Exit(2)
	}

	bc := es_sidecar.BackupsConfig{}

	s.esSidecar = esSidecar
	s.esClient = esClient
	s.esURL = url

	s.purgeServer = server.NewEsSidecarServer(esSidecar, &bc)
	return s
}

func (suite *Suite) GlobalSetup() {
	// Must ensure we're clean to start with
	suite.DeleteAllIndices()
}

func (suite *Suite) DeleteAllIndices() {
	indices, err := suite.esClient.IndexNames()
	if err != nil {
		fmt.Printf("Could not retrieve index list: %s\n", err)
	}
	suite.esClient.DeleteIndex(indices...).Do(context.Background())
}

func (suite Suite) GlobalTeardown() {
	suite.DeleteAllIndices()

}
