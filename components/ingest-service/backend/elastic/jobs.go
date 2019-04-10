//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

// This file contains functions that are being used by
// the job scheduler

package elastic

import (
	"context"
	"encoding/json"
	"time"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/olivere/elastic"
)

// MarkNodesMissing marks all nodes that have passed the specified threshold as 'missing'
func (es *Backend) MarkNodesMissing(ctx context.Context, threshold string) (int, error) {
	// TODO: (afiune) Update me when we get rid of the AllMappings array
	var (
		updateCount int
		nodesIndex  = mappings.NodeState.Alias
	)

	// The range query that will gather the nodes that have
	// not checked in the threshold, also do not retrieve nodes
	// that already on status=missing or marked for delete (exists=false)
	rangeQueryThreshold := elastic.NewRangeQuery(fieldCheckin).Lt("now-" + threshold)
	termQueryStatusMissing := elastic.NewTermsQuery("status", "missing")
	termQueryMarkedForDelete := elastic.NewTermsQuery(nodeExists, "false")

	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold).
		MustNot(termQueryStatusMissing).
		MustNot(termQueryMarkedForDelete)

	// Use a ScrollService since this is a batch process that might
	// have a large amount of data, think about them as a cursor on
	// a traditional database.
	scrollService := es.client.
		Scroll(nodesIndex).
		Query(boolQuery).
		Size(100).      // The size of the pages to scroll
		KeepAlive("5m") // Time ES will keep the cursor open

	// Make sure that we clear the scroll service
	defer scrollService.Clear(ctx)

	for {
		// Scroll through the results by pages
		searchResult, err := scrollService.Do(ctx)
		if err != nil {
			break
		}

		// Generate the BulkRequest from the hits from previous search
		bulkRequest := es.client.Bulk()
		for _, hit := range searchResult.Hits.Hits {
			updateReq := elastic.NewBulkUpdateRequest().
				Index(nodesIndex).
				Type(hit.Type).
				Id(hit.Id).
				Doc(struct {
					Status    string    `json:"status"`
					Timestamp time.Time `json:"timestamp"`
				}{
					Status:    "missing",
					Timestamp: time.Now().UTC(),
				})
			bulkRequest = bulkRequest.Add(updateReq)
		}

		// Execute the BulkRequest
		bulkResponse, err := bulkRequest.Do(ctx)

		// Keep a count of how many nodes we updated
		updateCount = updateCount + len(bulkResponse.Updated())

		// If one Bulk failed, lets exit
		if err != nil {
			return updateCount, err
		}
	}

	return updateCount, nil
}

// DeleteMarkedNodes will delete all the nodes from elasticsearch that no longer
// exists for the eyes of the config-mgmt-service. (that is, the nodes that has exists=false)
// The time constraint is the provided threshold
func (es *Backend) DeleteMarkedNodes(ctx context.Context, threshold string) (updateCount int, err error) {
	nodesIndex := mappings.NodeState.Alias
	allConvergeHistoryIndex := mappings.ConvergeHistory.Index + "-*"

	// The range query that will gather the nodes that have
	// have been marked for delete longer than the threshold,
	rangeQueryThreshold := elastic.NewRangeQuery(timestamp).Lt("now-" + threshold)
	// Collect only nodes that have been marked for delete (exists=false)
	termQueryMarkedForDelete := elastic.NewTermsQuery("exists", "false")

	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold).
		Must(termQueryMarkedForDelete)

	// Use a ScrollService since this is a batch process that might
	// have a large amount of data, think about them as a cursor on
	// a traditional database.
	scrollService := es.client.
		Scroll(nodesIndex).
		Query(boolQuery).
		Size(100).      // The size of the pages to scroll
		KeepAlive("5m") // Time ES will keep the cursor open

	for {
		// Scroll through the results by pages
		searchResult, err := scrollService.Do(ctx)
		if err != nil {
			break
		}

		// Generate the BulkRequest from the hits from previous search
		bulkRequest := es.client.Bulk()
		convergeHistoryQuery := elastic.NewBoolQuery()
		for _, hit := range searchResult.Hits.Hits {

			// Delete node-state nodes matching
			bulkRequest.Add(elastic.NewBulkDeleteRequest().
				Index(nodesIndex).
				Type(hit.Type).
				Id(hit.Id))

			// Delete node-attribute nodes matching
			bulkRequest.Add(elastic.NewBulkDeleteRequest().
				Index(mappings.NodeAttribute.Index).
				Type(mappings.NodeAttribute.Type).
				Id(hit.Id))

			var node backend.Node
			err = json.Unmarshal(*hit.Source, &node)
			if err != nil {
				break
			}

			convergeHistoryQuery = convergeHistoryQuery.
				Should(elastic.NewTermsQuery("entity_uuid", node.EntityUuid))
		}

		// Delete all the matching converge-history nodes for this scroll
		bulkIndexByScrollResponse, err := elastic.NewDeleteByQueryService(es.client).
			Index(allConvergeHistoryIndex).
			Type(mappings.ConvergeHistory.Type).
			Query(convergeHistoryQuery).
			Do(ctx)

		if err != nil {
			break
		}

		updateCount = updateCount + int(bulkIndexByScrollResponse.Deleted)

		// Execute the BulkRequest
		bulkResponse, err := bulkRequest.Do(ctx)

		// Keep a count of how many nodes we deleted
		updateCount = updateCount + len(bulkResponse.Deleted())

		// If one Bulk failed, lets exit
		if err != nil {
			break
		}
	}

	// Clear Scroll
	scrollService.Clear(ctx)

	return
}

// MarkMissingNodesForDeletion will mark all the nodes that have been missing for over the
// specified threshold time as 'ready for deletion'. (that is, mark the node as exists=false)
func (es *Backend) MarkMissingNodesForDeletion(ctx context.Context, threshold string) (int, error) {
	var (
		updateCount int
		docIDs      []string
		nodesIndex  = mappings.NodeState.Alias
	)

	// The range query that will gather the nodes that have been missing
	// for over the threshold time and nodes that are NOT already marked
	// for delete (exists=false)
	rangeQueryThreshold := elastic.NewRangeQuery(timestamp).Lt("now-" + threshold)
	termQueryMarkedForDelete := elastic.NewTermsQuery(nodeExists, "false")
	termQueryStatusMissing := elastic.NewTermsQuery("status", "missing")

	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold).
		Must(termQueryStatusMissing).     // Only nodes that are missing
		MustNot(termQueryMarkedForDelete) // But are not already marked for delete

	// Use a ScrollService since this is a batch process that might
	// have a large amount of data, think about them as a cursor on
	// a traditional database.
	scrollService := es.client.
		Scroll(nodesIndex).
		Query(boolQuery).
		Size(100).      // The size of the pages to scroll
		KeepAlive("5m") // Time ES will keep the cursor open

	// Make sure that we clear the scroll service
	defer scrollService.Clear(ctx)

	for {
		// Scroll through the results by pages
		searchResult, err := scrollService.Do(ctx)
		if err != nil {
			break
		}

		docIDs = make([]string, 0)
		for _, hit := range searchResult.Hits.Hits {
			docIDs = append(docIDs, hit.Id)
		}

		// Execute the BulkRequest
		bulkUpdates, err := es.deleteNodeByDocID(ctx, docIDs)

		// Keep a count of how many nodes we updated
		updateCount = updateCount + bulkUpdates

		// If one Bulk failed, lets exit
		if err != nil {
			return updateCount, err
		}
	}

	return updateCount, nil
}
