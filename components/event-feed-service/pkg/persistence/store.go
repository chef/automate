package persistence

import (
	"github.com/chef/automate/components/event-feed-service/pkg/util"
	project_update_lib "github.com/chef/automate/lib/authz"
	olivere "github.com/olivere/elastic"
	"golang.org/x/net/context"
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
	CreateFeedEntry(entry *util.FeedEntry) (bool, error)
	GetFeed(query *util.FeedQuery) ([]*util.FeedEntry, int64, error)
	GetFeedSummary(query *util.FeedSummaryQuery) (map[string]int64, error)
	GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*util.ActionLine, error)
}

func NewFeedStore(esClient *olivere.Client) FeedStore {
	return NewElasticFeedStore(esClient)
}
