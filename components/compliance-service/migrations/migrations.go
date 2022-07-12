package migrations

import (
	"context"
	"encoding/json"

	"github.com/olivere/elastic/v7"
	"github.com/sirupsen/logrus"
)

type Node struct {
	NodeUUID  string `json:"node_uuid"`
	EndTime   string `json:"end_time"`
	DayLatest bool   `json:"day_latest"`
}

func GetNodesDayLatestTrue(client *elastic.Client, ctx context.Context) ([]Node, error) {
	var nodes []Node
	termQueryDayLatest := elastic.NewTermQuery("day_latest", true)

	boolQuery := elastic.NewBoolQuery().
		Must(termQueryDayLatest)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"end_time",
		"day_latest",
	)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(10000)

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index("comp-7*").
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(ctx)

	if err != nil {
		return nil, err
	}

	if searchResult.TotalHits() > 0 {
		logrus.Printf("Found a total of %d ESInSpecReport\n", searchResult.TotalHits())
		// Iterate through results
		for _, hit := range searchResult.Hits.Hits {
			var node Node
			if hit.Source != nil {
				err := json.Unmarshal(hit.Source, &node)
				if err != nil {
					logrus.Errorf("Received error while unmarshling %+v", err)
				}

			}
			nodes = append(nodes, node)
		}
	}
	return nodes, nil
}

func SetNodesDayLatestFalse(client *elastic.Client, ctx context.Context) error {

	return nil
}
