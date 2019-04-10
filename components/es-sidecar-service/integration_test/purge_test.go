package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	api "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/stretchr/testify/assert"
)

const (
	docPurgeTestIndexName = "doc-delete-test-index"
)

func TestPurgeTimeSeriesIndicesByAge(t *testing.T) {
	cases := []struct {
		startingCount          int
		olderThanDays          int32
		expectedRemainingCount int
	}{
		{startingCount: 10, olderThanDays: 9, expectedRemainingCount: 9},
		{startingCount: 10, olderThanDays: 5, expectedRemainingCount: 5},
		{startingCount: 10, olderThanDays: 2, expectedRemainingCount: 2},
		{startingCount: 10, olderThanDays: 1, expectedRemainingCount: 1},
		{startingCount: 10, olderThanDays: 0, expectedRemainingCount: 0},
	}

	for _, test := range cases {
		t.Run(
			fmt.Sprintf("When starting with %v indices and removing older than %v days, %v remain",
				test.startingCount, test.olderThanDays, test.expectedRemainingCount),
			func(t *testing.T) {
				doPurgeTest(t, TimeSeriesTestIndex, test.startingCount, test.olderThanDays, test.expectedRemainingCount)
			},
		)
	}
}

func TestPurgeTimeSeriesIndicesByAge_IndexNotFound(t *testing.T) {
	req := &api.PurgeRequest{
		Id:            "purge-id-2",
		Index:         "invalid-index",
		OlderThanDays: 5,
	}
	res, err := suite.purgeServer.PurgeTimeSeriesIndicesByAge(context.Background(), req)
	assert.Nil(t, err)
	// Should still be reported as success - the action was completed, there was just nothing to do.
	assert.Equal(t, res.Success, true)

}

type purgeDocTestCase struct {
	id            string
	olderThanDays int32
	docsExpected  int64
}

func TestPurgeDocumentsFromIndexByAge(t *testing.T) {
	now := time.Now()

	// Note the additional -1 minute - this ensures
	allDocs := []testDocument{
		testDocument{id_for_search: "3-day", EndDate: now.AddDate(0, 0, -3)},
		testDocument{id_for_search: "2-day", EndDate: now.AddDate(0, 0, -2)},
		testDocument{id_for_search: "1-day", EndDate: now.AddDate(0, 0, -1)},
	}
	cases := []purgeDocTestCase{
		// We added two docs, make sure that we end  with two docs
		// when neither falls inside of our date range
		{olderThanDays: 5, docsExpected: 3},
		{olderThanDays: 4, docsExpected: 3},
		// Edge condition: our 3 day old doc is actually a ms or so older than three days,
		// so we'll expect to see it deleted here as well.
		{olderThanDays: 3, docsExpected: 2},
		// Similarly, our 2-day doc is a ms or so older than two days, so it'll get deleted
		{olderThanDays: 2, docsExpected: 1},
		// Older than 0 should remove anything left.
		{olderThanDays: 0, docsExpected: 0},
	}
	for _, test := range cases {
		desc := fmt.Sprintf("Deleting docs older than %v days should leave behind %v docs",
			test.olderThanDays, test.docsExpected)

		t.Run(desc, func(t *testing.T) {
			doDocPurgeTest(t, allDocs, test)
		})
	}
}

func doDocPurgeTest(t *testing.T, startingDocs []testDocument, test purgeDocTestCase) {
	ctx := context.Background()
	suite.esSidecar.MakeMeAnIndex(ctx, docPurgeTestIndexName)

	for _, doc := range startingDocs {
		addDocToTestIndex(t, doc, doc.id_for_search)
	}
	req := &api.PurgeRequest{
		Id:            "purge-id-1",
		Index:         docPurgeTestIndexName,
		OlderThanDays: test.olderThanDays,
	}

	delRes, delErr := suite.purgeServer.PurgeDocumentsFromIndexByAge(ctx, req)
	assert.Nil(t, delErr)
	assert.NotNil(t, delRes)

	count, err := suite.esClient.Count(docPurgeTestIndexName).Do(ctx)
	assert.Nil(t, err)
	assert.Equal(t, test.docsExpected, count)

	suite.esClient.DeleteIndex(docPurgeTestIndexName).Do(context.Background())

}

func addDocToTestIndex(t *testing.T, doc testDocument, id string) {
	addDocToIndex(t, docPurgeTestIndexName, doc, id)
}

func doPurgeTest(t *testing.T, name string, startingIndexCount int, olderThanDays int32, expectedRemainingCount int) {
	// setup our test by creating time series indices as 'testindex-20xx.xx.xx, dating backwards starting today.
	suite.esSidecar.CreateTimeNamedIndices(context.Background(), time.Now(), name, startingIndexCount)

	req := &api.PurgeRequest{
		Id:            "purge-id-1",
		Index:         TimeSeriesTestIndex,
		OlderThanDays: olderThanDays,
	}

	res, err := suite.purgeServer.PurgeTimeSeriesIndicesByAge(context.Background(), req)
	assert.Nil(t, err)
	assert.Equal(t, true, res.Success)
	// Note: Seeing unexpected failures where your count isn't matching expected?
	// Make sure you've deleted any other indices created in other tests prior to calling doPurgeTest.

	// the count will be off.
	indices, _ := suite.esClient.IndexNames()
	assert.Equal(t, expectedRemainingCount, len(indices))
}
