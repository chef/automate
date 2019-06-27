package persistence

import (
	"github.com/chef/automate/components/event-feed-service/pkg/util"
	olivere "github.com/olivere/elastic"
	"golang.org/x/net/context"
)

type FeedStore interface {
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
