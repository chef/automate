//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/olivere/elastic"
)

// InsertAction inserts a Chef Client Action into action-YYYY.MM.DD index
func (es *Backend) InsertAction(ctx context.Context, action backend.InternalChefAction) error {
	mapping := mappings.Actions
	// Using time field to ingest to correctly dated index
	time := action.RecordedAt
	err := es.addDataToTimeseriesIndex(ctx, mapping, time, action)
	return err
}

func (es *Backend) CreateBulkActionRequest(action backend.InternalChefAction) elastic.BulkableRequest {
	mapping := mappings.Actions
	time := action.RecordedAt
	return es.createBulkUpdateRequestToTimeseriesIndex(mapping, time, action)
}
