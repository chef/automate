package integration_test

import (
	"context"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

var (
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")
	suite            = NewSuite(elasticsearchUrl)
)

const (
	GRPC_PORT = 10390
	GRPC_HOST = "localhost"
)

const testTypeName = "test-data"

// TestMain provides setup and teardown to surround test execution.
// teardown everything after we have finished testing.
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	suite.GlobalSetup()
	exitCode := m.Run()
	suite.GlobalTeardown()
	os.Exit(exitCode)
}

func AssertGRPCErrorCode(t *testing.T, err error, code codes.Code) {
	t.Run(fmt.Sprintf("verifying grpc error code '%v'", code), func(t *testing.T) {
		if assert.NotNil(t, err) {
			assert.Contains(t, err.Error(), "rpc error:")
			errStatus, ok := status.FromError(err)
			if assert.True(t, ok) {
				assert.Equal(t, code, errStatus.Code())
			}
		}
	})
}

type testDocument struct {
	EndDate       time.Time `json:"end_time"`
	id_for_search string
}

func addDocToIndex(t *testing.T, indexName string, doc testDocument, id string) {
	res, err := suite.esClient.Index().
		Index(indexName).
		Type(testTypeName).
		Id(id).
		BodyJson(doc).
		Refresh("true").
		Do(context.Background())

	assert.NotNil(t, res)
	assert.Nil(t, err)
}
