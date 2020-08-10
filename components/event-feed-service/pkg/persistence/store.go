package persistence

import (
	"context"

	olivere "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	project_update_lib "github.com/chef/automate/lib/authz"
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

	JobCancel(context.Context, string) error
	UpdateProjectTags(context.Context, map[string]*authz.ProjectRules) ([]string, error)

	//SerializedProjectUpdate
	ListProjectUpdateTasks(ctx context.Context) ([]project_update_lib.SerializedProjectUpdateTask, error)
	RunProjectUpdateTask(ctx context.Context, projectUpdateID string, params map[string]string, projectTaggingRules map[string]*authz.ProjectRules) (project_update_lib.SerializedProjectUpdateTaskID, project_update_lib.SerializedProjectUpdateTaskStatus, error)
	MonitorProjectUpdateTask(ctx context.Context, projectUpdateID string, id project_update_lib.SerializedProjectUpdateTaskID) (project_update_lib.SerializedProjectUpdateTaskStatus, error)
	CancelProjectUpdateTask(ctx context.Context, projectUpdateID string, id project_update_lib.SerializedProjectUpdateTaskID) error
}

func NewFeedStore(esClient *olivere.Client) FeedStore {
	return NewElasticFeedStore(esClient)
}
