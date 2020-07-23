//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"encoding/json"
	"time"

	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	log "github.com/sirupsen/logrus"
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

func (es *Backend) GetActions(pageSize int, cursorDate time.Time,
	cursorID string, ascending bool) ([]backend.InternalChefAction, int64, error) {
	actions := []backend.InternalChefAction{}

	searchService := es.client.Search().
		Index(mappings.Actions.Index+"-*").
		Size(pageSize).
		Sort("recorded_at", ascending).
		Sort("id", ascending)

	if cursorDate.Unix() != 0 && cursorID != "" {
		milliseconds := cursorDate.UnixNano() / int64(time.Millisecond)
		searchService = searchService.SearchAfter(milliseconds, cursorID) // the date has to be in milliseconds
	}

	searchResult, err := searchService.Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return actions, 0, err
	}

	for _, hit := range searchResult.Hits.Hits {
		var action backend.InternalChefAction
		err := json.Unmarshal(*hit.Source, &action)
		if err != nil {
			log.WithFields(log.Fields{
				"object": hit.Source,
			}).WithError(err).Debug("Error unmarshalling the action object")
		} else {
			actions = append(actions, action)
		}
	}

	// sort actions, because if ascending = true they will not be in the descending order.
	if ascending {
		es.reverseActions(actions)
	}

	return actions, searchResult.Hits.TotalHits, nil
}

func (es *Backend) reverseActions(actions []backend.InternalChefAction) {
	length := len(actions)
	for i := length/2 - 1; i >= 0; i-- {
		opp := length - 1 - i
		actions[i], actions[opp] = actions[opp], actions[i]
	}
}
