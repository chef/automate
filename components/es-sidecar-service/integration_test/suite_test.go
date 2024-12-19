package integration_test

import (
	"context"
	"crypto/tls"
	"fmt"
	"net/http"
	"os"

	es_sidecar "github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/components/es-sidecar-service/pkg/server"
	elastic "github.com/olivere/elastic/v7"
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

	tr := &http.Transport{
		TLSClientConfig: &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		},
	}
	client := &http.Client{Transport: tr}

	esClient, err := elastic.NewSimpleClient(
		elastic.SetURL(url),
		elastic.SetSniff(false),
		elastic.SetHttpClient(client),
		elastic.SetBasicAuth("admin", "admin"),
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

	indicesToDelete := make([]string, 0)
	for _, v := range indices {
		if v == ".plugins-ml-config" || v == ".opensearch-observability" || v == ".opendistro_security" {
			continue
		} else {
			indicesToDelete = append(indicesToDelete, v)
		}
	}
	suite.esClient.DeleteIndex(indicesToDelete...).Do(context.Background())
}

func (suite Suite) GlobalTeardown() {
	suite.DeleteAllIndices()

}
