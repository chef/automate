//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
//

package elastic

import (
	"context"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	elastic "github.com/olivere/elastic/v7"
)

// InsertRun inserts a Chef Client Run into converge-history-YYYY.MM.DD index
func (es *Backend) InsertRun(ctx context.Context, run backend.Run) error {
	mapping := mappings.ConvergeHistory
	// We have to use the `end_time` of the CCR to ingest it
	// in the right ES index (converge-history)
	endTime := run.EndTime
	err := es.addDataToTimeseriesIndex(ctx, mapping, endTime, run)
	return err
}

func (es *Backend) CreateBulkRunUpdateRequest(run backend.Run) elastic.BulkableRequest {
	mapping := mappings.ConvergeHistory
	endTime := run.EndTime
	return es.createBulkUpdateRequestToTimeseriesIndex(mapping, endTime, run)
}
