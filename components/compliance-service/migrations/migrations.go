package migrations

import (
	"context"
	"encoding/json"
	"time"

	"github.com/olivere/elastic/v7"
	"github.com/sirupsen/logrus"
)

type Node struct {
	NodeUUID  string `json:"node_uuid"`
	EndTime   string `json:"end_time"`
	DayLatest bool   `json:"day_latest"`
}

var setNodes map[string]Node

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
	today := time.Now()
	nodes, err := GetNodesDayLatestTrue(client, ctx)
	if err != nil {
		return nil
	}
	for _, node := range nodes {
		nodeEndTime, err := time.Parse(time.RFC3339, node.EndTime)
		if err != nil {
			logrus.Error("cannot parse")
		}
		if nodeEndTime.Before(today) {
			logrus.Info("Time: ", nodeEndTime)
			if err = UpdateNodes(client, ctx, node.NodeUUID); err != nil {
				logrus.Errorf("cannot update for: %+v", err)
			}
		}

	}
	return nil
}

func UpdateNodes(client *elastic.Client, ctx context.Context, nodeID string) error {
	termQueryDayLatestTrue := elastic.NewTermQuery("day_latest", true)
	termQueryThisNode := elastic.NewTermsQuery("node_uuid", nodeID)

	boolQueryDayLatestThisNodeNotThisReport := elastic.NewBoolQuery().
		Must(termQueryDayLatestTrue).
		Must(termQueryThisNode)

	script := elastic.NewScript("ctx._source.day_latest = false")

	// Updating in all the Indices
	_, err := elastic.NewUpdateByQueryService(client).
		Index("comp-7*").
		Query(boolQueryDayLatestThisNodeNotThisReport).
		Script(script).
		Refresh("false").
		Do(ctx)

	return err
}
