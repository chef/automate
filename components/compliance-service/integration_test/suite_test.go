package integration_test

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"testing"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/ingest/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	nodes "github.com/chef/automate/components/nodemanager-service/api/nodes"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/lib/grpc/auth_context"

	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/jsonpb"
	empty "github.com/golang/protobuf/ptypes/empty"
	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
)

var complianceReportIndex = fmt.Sprintf("%s-%s", mappings.ComplianceRepDate.Index, "*")
var complianceSummaryIndex = fmt.Sprintf("%s-%s", mappings.ComplianceSumDate.Index, "*")

// Suite helps you manipulate various stages of your tests, it provides
// common functionality.
type Suite struct {
	elasticClient          *elastic.Client
	ingesticESClient       *ingestic.ESClient
	ComplianceIngestServer *server.ComplianceIngestServer
	ProjectsClientMock     *iam_v2.MockProjectsClient
	NodeManagerMock        *NodeManagerMock
	NotifierMock           *NotifierMock
	EventServiceClientMock *event.MockEventServiceClient
	ConfigManager          *config.ConfigManager
}

// Initialize the test suite
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the tests and print an error message
func NewGlobalSuite() *Suite {
	s := new(Suite)

	// Create a new elastic Client
	esclient, err := elastic.NewClient(
		elastic.SetURL(elasticsearchUrl),
		elastic.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch client from %q: %s\n", elasticsearchUrl, err)
		os.Exit(1)
	}

	s.elasticClient = esclient
	s.ingesticESClient = ingestic.NewESClient(esclient)
	s.ingesticESClient.InitializeStore(context.Background())

	s.ProjectsClientMock = iam_v2.NewMockProjectsClient(gomock.NewController(nil))
	s.ProjectsClientMock.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ListRulesForAllProjectsResp{}, nil)
	s.NodeManagerMock = &NodeManagerMock{}
	s.NotifierMock = &NotifierMock{}
	s.EventServiceClientMock = event.NewMockEventServiceClient(gomock.NewController(nil))
	s.EventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&event.PublishResponse{}, nil)

	configFile := "/tmp/.compliance-service.toml"
	os.Remove(configFile)
	s.ConfigManager, err = config.NewConfigManager(configFile)
	if err != nil {
		fmt.Printf("Could not create config manager with file %q. %v\n", configFile, err)
		os.Exit(3)
	}

	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, "", s.NotifierMock,
		s.ProjectsClientMock, s.EventServiceClientMock, s.ConfigManager)

	return s
}

func NewLocalSuite(t *testing.T) *Suite {
	s := new(Suite)

	// Create a new elastic Client
	esclient, err := elastic.NewClient(
		elastic.SetURL(elasticsearchUrl),
		elastic.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch client from %q: %s\n", elasticsearchUrl, err)
		os.Exit(1)
	}

	s.elasticClient = esclient
	s.ingesticESClient = ingestic.NewESClient(esclient)
	s.ingesticESClient.InitializeStore(context.Background())

	s.ProjectsClientMock = iam_v2.NewMockProjectsClient(gomock.NewController(t))
	s.NodeManagerMock = &NodeManagerMock{}
	s.NotifierMock = &NotifierMock{}
	s.EventServiceClientMock = event.NewMockEventServiceClient(gomock.NewController(t))
	configFile := "/tmp/.compliance-service.toml"
	os.Remove(configFile)
	s.ConfigManager, err = config.NewConfigManager(configFile)
	if err != nil {
		fmt.Printf("Could not create config manager with file %q. %v\n", configFile, err)
		os.Exit(3)
	}

	s.ComplianceIngestServer = server.NewComplianceIngestServer(s.ingesticESClient,
		s.NodeManagerMock, "", s.NotifierMock,
		s.ProjectsClientMock, s.EventServiceClientMock, s.ConfigManager)

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

func (s *Suite) ingestReport(fileName string, f func(*compliance.Report)) error {
	fileData, err := os.Open(fileName)
	if err != nil {
		return err
	}
	defer fileData.Close()
	var iReport compliance.Report
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	if err = unmarshaler.Unmarshal(fileData, &iReport); err != nil {
		return err
	}

	f(&iReport)

	ctx := context.Background()
	_, err = s.ComplianceIngestServer.ProcessComplianceReport(ctx, &iReport)

	return err
}

func waitFor(f func() bool) {
	period := time.Millisecond * 10
	for {
		if f() {
			break
		}

		time.Sleep(period)
	}
}

func (s *Suite) GetAllReportsESInSpecReport() ([]*relaxting.ESInSpecReport, error) {
	reports := make([]*relaxting.ESInSpecReport, 0)

	searchResult, err := s.elasticClient.Search().
		Index(complianceReportIndex).
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
					logrus.Errorf("GetAllReportsESInSpecReport unmarshal error: %s", err.Error())
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

	searchResult, err := s.elasticClient.Search().
		Index(complianceSummaryIndex).
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
					logrus.Errorf("GetAllSummaryESInSpecSummary unmarshal error: %s", err.Error())
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
func (s *Suite) InsertInspecReports(reports []*relaxting.ESInSpecReport) ([]string, error) {
	ids := make([]string, len(reports))

	endTime := time.Now()
	// Insert reports
	for i, report := range reports {
		id := newUUID()
		ids[i] = id

		err := s.ingesticESClient.InsertInspecReport(context.Background(), id, endTime, report)
		if err != nil {
			return nil, err
		}
	}

	s.RefreshComplianceReportIndex()

	return ids, nil
}

// InsertInspecSummaries ingests a number of summaries and at the end, refreshes the summary index
func (s *Suite) InsertInspecSummaries(summaries []*relaxting.ESInSpecSummary) ([]string, error) {
	ids := make([]string, len(summaries))

	endTime := time.Now()
	// Insert summaries
	for i, summary := range summaries {
		id := newUUID()
		ids[i] = id

		err := s.ingesticESClient.InsertInspecSummary(context.Background(), id, endTime, summary)
		if err != nil {
			return nil, err
		}
	}

	s.RefreshComplianceSummaryIndex()

	return ids, nil
}

func (s *Suite) RefreshComplianceReportIndex() {
	s.RefreshIndices(complianceReportIndex)
}

func (s *Suite) RefreshComplianceSummaryIndex() {
	s.RefreshIndices(complianceSummaryIndex)
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
	var err error
	// ES Query to match all documents
	q := elastic.RawStringQuery("{\"match_all\":{}}")

	maxNumberOfTries := 3
	tries := 0
	for ; tries < maxNumberOfTries; tries++ {
		_, err = s.elasticClient.DeleteByQuery("_all").
			Query(q).
			IgnoreUnavailable(true).
			Refresh("true").
			WaitForCompletion(true).
			Do(context.Background())

		if err != nil {
			_, err := s.elasticClient.Refresh().Do(context.Background())
			if err != nil {
				fmt.Printf("Could not 'refresh' indices.\nError: %s", err)
				os.Exit(3)
			}
			continue
		}

		break
	}

	if tries == maxNumberOfTries {
		fmt.Printf("Could not 'clean' ES documents.\nError: %s", err)
		os.Exit(3)
	}
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

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}
