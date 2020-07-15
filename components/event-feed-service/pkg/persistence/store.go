package persistence

import (
	"context"

	olivere "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	project_update_lib "github.com/chef/automate/lib/authz"
)

var (
	ProjectTag              = "projects"
	ProducerTypeTag         = "producer_object_type"
	ChefServerProducerIDTag = "chef_server"
)

type FeedStore interface {
	// @param (context, indexName)
	DeleteIndex(ctx context.Context, index string) error
	// @param (context, jobID)
	JobStatus(context.Context, string) (project_update_lib.JobStatus, error)
	// @param (context, indexName)
	DoesIndexExists(context.Context, string) (bool, error)
	// @param (context, previousIndex)
	ReindexFeedsToLatest(context.Context, string) (string, error)
	// @param (context)
	InitializeStore(context.Context) error
	CreateFeedEntry(entry *feed.FeedEntry) (bool, error)
	GetFeed(query *feed.FeedQuery) ([]*feed.FeedEntry, int64, error)
	GetFeedSummary(query *feed.FeedSummaryQuery) (map[string]int64, error)
	GetActionLine(filters map[string][]string, startDate string, endDate string, timezone string, interval int, action string) (*feed.ActionLine, error)
}

func NewFeedStore(esClient *olivere.Client) FeedStore {
	return NewElasticFeedStore(esClient)
}
