package migrations

import (
	"context"
	"encoding/json"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/olivere/elastic/v7"
	"github.com/sirupsen/logrus"
)

func GetNodesDayLatestTrue(client *elastic.Client, ctx context.Context) ([]relaxting.NodesUpgradation, error) {
	var nodes []relaxting.NodesUpgradation

func GetNodesDayLatestTrue(client *elastic.Client, ctx context.Context) ([]Node, error) {
	var nodes []Node
	termQueryDayLatest := elastic.NewTermQuery("day_latest", true)
	termQueryDailyLatest := elastic.NewTermQuery("daily_latest", true)

	boolQuery := elastic.NewBoolQuery().
		Must(termQueryDayLatest).
		Must(termQueryDailyLatest)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"end_time",
		"day_latest",
	)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(100000)

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(indices).
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
			var node relaxting.NodesUpgradation
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
	nodes, err := GetNodesDayLatestTrue(client, ctx)
	if err != nil {
		return nil
	}
	for _, node := range nodes {
		logrus.Print(node)
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

func GetLast90DaysIndices() (string, error) {

	time90daysAgo := time.Now().Add(-24 * time.Hour * 90)

	// Making a filter query to get all the indices from today to 90 days back
	filters := map[string][]string{"start_time": {time90daysAgo.Format(time.RFC3339)}, "end_time": {time.Now().Format(time.RFC3339)}}

	// Getting all the indices
	esIndexs, err := relaxting.GetEsIndex(filters, false)
	if err != nil {
		logrus.Errorf("Cannot get indexes: %+v", err)
		return "", err
	}
	return esIndexs, nil
}
