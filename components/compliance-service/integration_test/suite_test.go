package integration_test

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/ingest/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	nodes "github.com/chef/automate/components/nodemanager-service/api/nodes"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/gofrs/uuid"
	empty "github.com/golang/protobuf/ptypes/empty"
	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
)

// Suite helps you manipulate various stages of your tests, it provides
// common functionality.
type Suite struct {
	elasticClient          *elastic.Client
	ingesticESClient       *ingestic.ESClient
	ComplianceIngestServer *server.ComplianceIngestServer
	ProjectsClientMock     *ProjectsClientMock
	NodeManagerMock        *NodeManagerMock
	NotifierMock           *NotifierMock
	EventServiceClientMock *EventServiceClientMock
}

// Initialize the test suite
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the tests and print an error message
func NewSuite(url string) *Suite {
	s := new(Suite)

	// Create a new elastic Client
	esclient, err := elastic.NewClient(
		elastic.SetURL(url),
		elastic.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch client from %q: %s\n", url, err)
		os.Exit(1)
	}

	s.elasticClient = esclient
	s.ingesticESClient = ingestic.NewESClient(esclient)
	s.ingesticESClient.InitializeStore(context.Background())

	s.ProjectsClientMock = &ProjectsClientMock{}
	s.NodeManagerMock = &NodeManagerMock{}
	s.NotifierMock = &NotifierMock{}
	s.EventServiceClientMock = &EventServiceClientMock{}

	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, "", s.NotifierMock,
		s.ProjectsClientMock, s.EventServiceClientMock)

	return s
}

// GlobalSetup is the place where you prepare anything that we need before
// executing all our test suite, at the moment we are just initializing ES Indices
func (s *Suite) GlobalSetup() {

}

// GlobalTeardown is the place where you tear everything down after we have finished
// executing all our test suite
func (s *Suite) GlobalTeardown() {

}

func (s *Suite) GetAllReportsESInSpecReport() ([]*relaxting.ESInSpecReport, error) {
	reports := make([]*relaxting.ESInSpecReport, 0)

	index := fmt.Sprintf("%s-%s", mappings.ComplianceRepDate.Index, "*")

	searchResult, err := s.elasticClient.Search().
		Index(index).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
		).
		Do(context.Background())

	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := relaxting.ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esInSpecReport)
				if err != nil {
					logrus.Errorf("GetReport unmarshal error: %s", err.Error())
					return reports, err
				}
			}
			reports = append(reports, &esInSpecReport)
		}
	}

	return reports, err
}

func (s *Suite) GetAllSummaryESInSpecSummary() ([]*relaxting.ESInSpecSummary, error) {
	summaries := make([]*relaxting.ESInSpecSummary, 0)

	index := fmt.Sprintf("%s-%s", mappings.ComplianceSumDate.Index, "*")

	searchResult, err := s.elasticClient.Search().
		Index(index).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
		).
		Do(context.Background())

	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecSummary := relaxting.ESInSpecSummary{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esInSpecSummary)
				if err != nil {
					logrus.Errorf("GetSummary unmarshal error: %s", err.Error())
					return summaries, err
				}
			}
			summaries = append(summaries, &esInSpecSummary)
		}
	}

	return summaries, err
}

func (s *Suite) WaitForESJobToComplete(esJobID string) {
	isJobComplete, err := s.ingesticESClient.JobStatus(context.Background(), esJobID)
	if err != nil {
		fmt.Printf("Failed waiting for elasticsearch job to complete")
		os.Exit(1)
	}
	for !isJobComplete.Completed {
		time.Sleep(time.Millisecond * 5)
		isJobComplete, err = s.ingesticESClient.JobStatus(context.Background(), esJobID)
		if err != nil {
			fmt.Printf("Failed waiting for elasticsearch job to complete")
			os.Exit(1)
		}
	}
}

// InsertInspecReports ingests a number of reports and at the end, refreshes the report index
func (s *Suite) InsertInspecReports(reports []*relaxting.ESInSpecReport) {
	endTime := time.Now()
	// Insert reports
	for _, report := range reports {
		id, err := uuid.NewV4()
		if err != nil {
			os.Exit(3)
		}
		err = s.ingesticESClient.InsertInspecReport(context.Background(), id.String(), endTime, report)
		if err != nil {
			os.Exit(3)
		}
	}

	index := fmt.Sprintf("%s-%s", mappings.ComplianceRepDate.Index, "*")

	// Refresh Indices
	s.RefreshIndices(index)
}

// InsertInspecSummaries ingests a number of summaries and at the end, refreshes the summary index
func (s *Suite) InsertInspecSummaries(summaries []*relaxting.ESInSpecSummary) {
	endTime := time.Now()
	// Insert summaries
	for _, summary := range summaries {
		id, err := uuid.NewV4()
		if err != nil {
			os.Exit(3)
		}
		err = s.ingesticESClient.InsertInspecSummary(context.Background(), id.String(), endTime, summary)
		if err != nil {
			os.Exit(3)
		}
	}

	index := fmt.Sprintf("%s-%s", mappings.ComplianceSumDate.Index, "*")

	// Refresh Indices
	s.RefreshIndices(index)
}

// RefreshIndices will refresh the provided ES Index or list of Indices
//
// Example 1: To refresh a single index, the node-state index
// ```
// suite.RefreshIndices(mappings.NodeState.Index)
// ```
//
// Example 2: To refresh all indices
// ```
// suite.RefreshIndices(suite.Indices()...)
// ```
func (s *Suite) RefreshIndices(indices ...string) {
	// Verify that the provided indices exists, if not remove them
	indices = s.verifyIndices(indices...)

	// If there are no valid Indices, stop processing
	if len(indices) == 0 {
		return
	}

	_, err := s.elasticClient.Refresh(indices...).Do(context.Background())
	if err != nil {
		fmt.Printf("Could not 'refresh' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

// verifyIndices receives a list of indices and verifies that they exist,
// if the index doesn't exist, it removes it from the list and returns only
// the valid indices that exists in ES
func (s *Suite) verifyIndices(indices ...string) []string {
	var validIndices = make([]string, 0)

	for _, index := range indices {
		if s.indexExists(index) {
			validIndices = append(validIndices, index)
		}
	}

	return validIndices
}

// indexExists tells you if an ES Index exists or not
func (s *Suite) indexExists(i string) bool {
	exists, _ := s.elasticClient.IndexExists(i).Do(context.Background())
	return exists
}

// DeleteAllDocuments will clean every single document from all ES Indices
//
// You should call this method on every single test as the following example:
// ```
//  func TestGrpcFunc(t *testing.T) {
//    // Here we are ingesting a number of nodes
//    suite.IngestNodes(nodes)
//
//    // Immediately after the ingestion add the hook to clean all documents,
//    // by using `defer` you will ensure that the next test will have clean
//    // data regardless if this test passes or fails
//    defer suite.DeleteAllDocuments()
//  }
// ```
func (s *Suite) DeleteAllDocuments() {
	// ES Query to match all documents
	q := elastic.RawStringQuery("{\"match_all\":{}}")

	// Make sure we clean them all!
	indices, _ := s.elasticClient.IndexNames()

	_, err := s.elasticClient.DeleteByQuery().
		Index(indices...).
		Type(s.types()...).
		Query(q).
		IgnoreUnavailable(true).
		Refresh("true").
		WaitForCompletion(true).
		Do(context.Background())

	if err != nil {
		fmt.Printf("Could not 'clean' ES documents from indices: '%v'\nError: %s", indices, err)
		os.Exit(3)
	}
}

// types returns the list of ES types registered in the Ingest service code base
func (s *Suite) types() []string {
	types := make([]string, len(mappings.AllMappings))

	for i, esMap := range mappings.AllMappings {
		types[i] = esMap.Type
	}

	return types
}

// Indices returns the list of ES indices registered in the Ingest service code base
func (s *Suite) Indices() []string {
	indices := make([]string, len(mappings.AllMappings))

	for i, esMap := range mappings.AllMappings {
		if esMap.Timeseries {
			indices[i] = esMap.IndexTimeseriesFmt(time.Now())
		} else {
			indices[i] = esMap.Index
		}
	}

	return indices
}

type EventServiceClientMock struct {
	LastEventSent *event.EventMsg
}

func (n *EventServiceClientMock) Publish(ctx context.Context, in *event.PublishRequest,
	opts ...grpc.CallOption) (*event.PublishResponse, error) {
	n.LastEventSent = in.Msg
	return &event.PublishResponse{}, nil
}
func (n *EventServiceClientMock) Subscribe(ctx context.Context, in *event.SubscribeRequest,
	opts ...grpc.CallOption) (*event.SubscribeResponse, error) {
	return &event.SubscribeResponse{}, nil
}
func (n *EventServiceClientMock) Start(ctx context.Context, in *event.StartRequest,
	opts ...grpc.CallOption) (*event.StartResponse, error) {
	return &event.StartResponse{}, nil
}
func (n *EventServiceClientMock) Stop(ctx context.Context, in *event.StopRequest,
	opts ...grpc.CallOption) (*event.StopResponse, error) {
	return &event.StopResponse{}, nil
}

type NotifierMock struct {
}

func (n *NotifierMock) Send(context.Context, *notifications.Event) {

}

func (n *NotifierMock) QueueSize() int {
	return 0
}

type NodeManagerMock struct {
}

func (nm *NodeManagerMock) Create(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*manager.Ids, error) {
	return &manager.Ids{}, nil
}

func (nm *NodeManagerMock) Read(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*manager.NodeManager, error) {
	return &manager.NodeManager{}, nil
}
func (nm *NodeManagerMock) Update(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) Delete(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodes(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*manager.Ids, error) {
	return &manager.Ids{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodeStateStopped(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) DeleteWithNodeStateTerminated(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) List(ctx context.Context, in *manager.Query,
	opts ...grpc.CallOption) (*manager.NodeManagers, error) {
	return &manager.NodeManagers{}, nil
}

func (nm *NodeManagerMock) Connect(ctx context.Context, in *manager.NodeManager,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) ConnectManager(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*empty.Empty, error) {
	return &empty.Empty{}, nil
}

func (nm *NodeManagerMock) SearchNodeFields(ctx context.Context, in *manager.FieldQuery,
	opts ...grpc.CallOption) (*manager.Fields, error) {
	return &manager.Fields{}, nil
}

func (nm *NodeManagerMock) SearchNodes(ctx context.Context, in *manager.NodeQuery,
	opts ...grpc.CallOption) (*manager.Nodes, error) {
	return &manager.Nodes{}, nil
}

func (nm *NodeManagerMock) ProcessNode(ctx context.Context, in *manager.NodeMetadata,
	opts ...grpc.CallOption) (*manager.ProcessNodeResponse, error) {
	return &manager.ProcessNodeResponse{}, nil
}

func (nm *NodeManagerMock) ChangeNodeState(ctx context.Context, in *manager.NodeState,
	opts ...grpc.CallOption) (*manager.ChangeNodeStateResponse, error) {
	return &manager.ChangeNodeStateResponse{}, nil
}

func (nm *NodeManagerMock) GetNodeWithSecrets(ctx context.Context, in *manager.Id,
	opts ...grpc.CallOption) (*nodes.Node, error) {
	return &nodes.Node{}, nil
}

func (nm *NodeManagerMock) SearchManagerNodes(ctx context.Context, in *manager.NodeQuery,
	opts ...grpc.CallOption) (*manager.ManagerNodes, error) {
	return &manager.ManagerNodes{}, nil
}

type ProjectsClientMock struct {
}

func (pm *ProjectsClientMock) UpdateProject(ctx context.Context, in *iam_v2.UpdateProjectReq,
	opts ...grpc.CallOption) (*iam_v2.UpdateProjectResp, error) {
	return &iam_v2.UpdateProjectResp{}, nil
}

func (pm *ProjectsClientMock) CreateProject(ctx context.Context, in *iam_v2.CreateProjectReq,
	opts ...grpc.CallOption) (*iam_v2.CreateProjectResp, error) {
	return &iam_v2.CreateProjectResp{}, nil
}
func (pm *ProjectsClientMock) GetProject(ctx context.Context, in *iam_v2.GetProjectReq,
	opts ...grpc.CallOption) (*iam_v2.GetProjectResp, error) {
	return &iam_v2.GetProjectResp{}, nil
}

func (pm *ProjectsClientMock) DeleteProject(ctx context.Context, in *iam_v2.DeleteProjectReq,
	opts ...grpc.CallOption) (*iam_v2.DeleteProjectResp, error) {
	return &iam_v2.DeleteProjectResp{}, nil
}

func (pm *ProjectsClientMock) ListProjects(ctx context.Context, in *iam_v2.ListProjectsReq,
	opts ...grpc.CallOption) (*iam_v2.ListProjectsResp, error) {
	return &iam_v2.ListProjectsResp{}, nil
}

func (pm *ProjectsClientMock) ListProjectRules(ctx context.Context, in *iam_v2.ListProjectRulesReq,
	opts ...grpc.CallOption) (*iam_v2.ProjectCollectionRulesResp, error) {
	return &iam_v2.ProjectCollectionRulesResp{}, nil
}

func (pm *ProjectsClientMock) GetProjectRules(ctx context.Context, in *iam_v2.GetProjectRulesReq,
	opts ...grpc.CallOption) (*iam_v2.GetProjectRulesResp, error) {
	return &iam_v2.GetProjectRulesResp{}, nil
}

func (pm *ProjectsClientMock) HandleEvent(ctx context.Context, in *event.EventMsg,
	opts ...grpc.CallOption) (*event.EventResponse, error) {
	return &event.EventResponse{}, nil
}

func (pm *ProjectsClientMock) ProjectUpdateStatus(ctx context.Context,
	req *iam_v2.ProjectUpdateStatusReq, opts ...grpc.CallOption) (*iam_v2.ProjectUpdateStatusResp, error) {
	return &iam_v2.ProjectUpdateStatusResp{}, nil
}
