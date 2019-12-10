package backend

import (
	"context"

	"github.com/olivere/elastic"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	project_update_lib "github.com/chef/automate/lib/authz"
)

type Client interface {
	// @param None
	Initializing() bool
	// @param (context, UTC time)
	InitializeStore(context.Context) error
	// @param (context, UTC time, data)
	InsertNode(context.Context, Node) error
	// @param (context, UTC time, data)
	InsertRun(context.Context, Run) error
	// @param (context, UTC time, data)
	InsertNodeAttribute(context.Context, NodeAttribute) error
	// @param (context, backend.InternalChefAction)
	InsertAction(context.Context, InternalChefAction) error
	// @param (context, threshold)
	MarkNodesMissing(context.Context, string) (int, error)
	// @param (context, threshold)
	DeleteMarkedNodes(context.Context, string) (int, error)
	// @param (context, threshold)
	MarkMissingNodesForDeletion(context.Context, string) (int, error)
	// @param (context, nodeIDs []string)
	MarkForDeleteMultipleNodesByID(ctx context.Context, nodeIDs []string) (int, error)
	// @param (context, nodeID string)
	DeleteNodeByID(ctx context.Context, nodeID string) (int, error)
	// @param (context, orgName string, remoteHostname string, nodeName string)
	DeleteNodeByFields(ctx context.Context, orgName string, remoteHostname string, nodeName string) (int, error)
	// @param (context, indexName)
	DeleteIndex(context.Context, string) error
	// @param (context, indexName)
	DoesIndexExists(context.Context, string) (bool, error)
	// @param (context, aliasName)
	DoesAliasExists(context.Context, string) bool
	// @param (context, templateName)
	DeleteTemplate(context.Context, string) error
	// @param (context, aliasName)
	RemoveAlias(context.Context, string, string) error
	// @param (context, aliasName)
	CreateAlias(context.Context, string, string) error
	// @param (context, cursorID)
	GetNodeBasics(context.Context, string) ([]NodeBasics, error)
	// @param (context, indexName)
	GetAllTimeseriesIndiceNames(context.Context, string) ([]string, error)
	// @param (context, nodeID)
	RecordLivenessPing(context.Context, Liveness) error
	// @param (context, instanceID)
	FindNodeIDByInstanceId(context.Context, string) ([]string, error)
	// @param (data)
	CreateBulkNodeUpdateRequest(Node) elastic.BulkableRequest
	// @param (data)
	CreateBulkNodeAttributeUpdateRequest(NodeAttribute) elastic.BulkableRequest
	// @param (data)
	CreateBulkRunUpdateRequest(Run) elastic.BulkableRequest
	// @param (data)
	CreateBulkActionRequest(InternalChefAction) elastic.BulkableRequest
	// @param (context, bulkableRequests)
	SendBulkRequest(context.Context, []elastic.BulkableRequest) error
	// @param (context, projectRules)
	UpdateNodeProjectTags(context.Context, map[string]*iam_v2.ProjectRules) (string, error)
	// @param (context, projectRules)
	UpdateActionProjectTags(context.Context, map[string]*iam_v2.ProjectRules) (string, error)
	// @param (context, jobID)
	JobStatus(context.Context, string) (project_update_lib.JobStatus, error)
	// @param (context, jobID)
	JobCancel(context.Context, string) error

	UpdateProjectTags(context.Context, map[string]*iam_v2.ProjectRules) ([]string, error)

	// Migration contracts
	ReindexInsightstoConvergeHistory(context.Context, string, string) error
	GetInsightsRunData(context.Context, string, string) ([]InsightsRunNodePayLoadData, error)
	GetLatestA1NodeRun(context.Context, string, string) (InsightsRun, bool, error)
	ReindexNodeStateA1(context.Context, string) error
	UpdateNode(context.Context, string, InsightsRun, []VersionedCookbook) error
	EmptyNodeLatestRunID(context.Context, string) error
	ReindexInsightstoActions(context.Context, string, string) error
	RefreshIndex(context.Context, string) error
	GetNodeCount(context.Context, string) (int64, error)
	// @param (context, previousIndex)
	// @return (taskID, error)
	ReindexNodeStateToLatest(context.Context, string) (string, error)
}
