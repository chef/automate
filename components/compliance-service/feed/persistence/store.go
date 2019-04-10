//
//  Author:: Gina Peers <gpeers@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package persistence

import (
	"github.com/chef/automate/components/compliance-service/feed/persistence/elastic"
	"github.com/chef/automate/components/compliance-service/feed/util"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

type FeedStore interface {
	CreateFeedConsumerNetwork(network *util.FeedConsumerNetwork) (bool, error)
	CreateFeedEntry(entry *util.FeedEntry) (bool, error)
	CreateFeedEntries(entries []*util.FeedEntry) (bool, error)
	GetFeedConsumerNetwork(query *util.FeedConsumerNetworkQuery) ([]*util.FeedConsumerNetwork, error)
	GetFeed(query *util.FeedQuery) ([]*util.FeedEntry, int64, error)
	GetFeedSummary(query *util.FeedSummaryQuery) (map[string]int64, error)
	GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*util.ActionLine, error)
}

func NewFeedStore(conn *relaxting.ES2Backend) FeedStore {
	return elastic.ElasticFeedStore{Conn: conn}
}
