//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package mock

import (
	"context"
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
)

// This Mock backend is a very simple model of how a backend client should look
// like when implementing the `backend.Client` interface
type Backend struct {
	client string
}

// Initialize the Mock backend client
//
// Normally you would like to inject its own config parameters coming from the
// `config.Service` and also if there is a client you need to initialize, here
// is the place to do it since this struct will be available for you in all the
// func you need to implement from the `backend.Client` interface
func New() Backend {
	return Backend{client: "dummy"}
}

// Implement the backend.Client interface
//
// In order to be a proper backend you have to implement all the contracts that
// the Client interface has setup, this will ensure that the endpoints we have
// defined will always be able to get the data from any backend we configure
func (m Backend) NodeExists(id string, filters map[string][]string) (bool, error) {
	return false, nil
}

func (m Backend) GetNodesCounts(filters map[string][]string,
	startDate string, endDate string) (backend.NodesCounts, error) {
	ns := backend.NodesCounts{}
	return ns, nil
}

func (m Backend) GetNodes(page int, perPage int, sf string,
	asc bool, filters map[string][]string, startDate, endDate string) ([]backend.Node, error) {
	var nodes []backend.Node
	n := *new(backend.Node)
	n.NodeName = "mock"
	nodes = append(nodes, n)
	return nodes, nil
}

func (m Backend) GetRuns(n string, page int, perPage int, filters map[string][]string, start string, end string) ([]backend.AbridgedConverge, error) {
	var runs []backend.AbridgedConverge
	r := backend.AbridgedConverge{
		StartTime: "this morning",
		RunID:     "123-abc",
		EndTime:   "later this morning",
		Status:    "success",
	}
	runs = append(runs, r)
	return runs, nil
}

func (m Backend) GetRun(run_id string, lastCcr time.Time) (backend.Run, error) {
	n := new(backend.Run)
	n.RunID = run_id
	n.NodeName = "node1"
	return *n, nil
}

func (m Backend) GetRunsCounts(filters map[string][]string, nodeID string, start string, end string) (backend.RunsCounts, error) {
	ns := backend.RunsCounts{}
	return ns, nil
}

func (m Backend) GetAttribute(nodeID string) (backend.NodeAttribute, error) {
	return backend.NodeAttribute{}, nil
}

func (m Backend) GetListForField(searchTerm string, filters map[string][]string) ([]string, error) {
	var fieldResults []string
	fieldResults = append(fieldResults, "field_result_1")
	fieldResults = append(fieldResults, "field_result_2")
	return fieldResults, nil
}

func (m Backend) GetSuggestions(term string, text string, filters map[string][]string) ([]backend.Suggestion, error) {
	suggestions := []backend.Suggestion{
		{
			Text:  "Node 1",
			Score: 4.4892697,
		},
		{
			Text:  "ubuntu",
			Score: 3.9768348,
		},
	}
	return suggestions, nil
}

func (m Backend) GetPolicyCookbooks(revisionID string) (backend.PolicyCookbooks, error) {
	return backend.PolicyCookbooks{
		PolicyName: "infra_base",
		CookbookLocks: []*backend.PolicyCookbookLock{
			{
				CookbookName: "apt",
				PolicyID:     "any",
			},
			{
				CookbookName: "cookbook",
				PolicyID:     "paula_smith",
			},
		},
	}, nil
}

func (es Backend) GetDateOfOldestConvergeIndices() (time.Time, bool, error) {
	return time.Time{}, false, nil
}

func (m Backend) GetInventoryNodes(ctx context.Context, start time.Time,
	end time.Time, filters map[string][]string, cursorDate time.Time,
	cursorID string, pageSize int, sortField string,
	ascending bool) ([]backend.InventoryNode, error) {
	var nodes []backend.InventoryNode
	n := *new(backend.InventoryNode)
	n.EntityUUID = "mock"
	nodes = append(nodes, n)
	return nodes, nil
}

func (m Backend) GetNodesPageByCursor(context.Context, time.Time,
	time.Time, map[string][]string, interface{},
	string, int, string, bool) ([]backend.Node, error) {
	var nodes []backend.Node
	n := *new(backend.Node)
	n.Environment = "mock"
	nodes = append(nodes, n)
	return nodes, nil
}

func (m Backend) GetRunsPageByCursor(context.Context, string, time.Time,
	time.Time, map[string][]string, time.Time,
	string, int, bool) ([]backend.Run, error) {
	var runs []backend.Run
	r := *new(backend.Run)
	r.Environment = "mock"
	runs = append(runs, r)
	return runs, nil
}

func (m Backend) GetCheckinCountsTimeSeries(time.Time, time.Time,
	map[string][]string) ([]backend.CountPeroid, error) {

	return []backend.CountPeroid{}, nil
}

func (es Backend) GetCreateCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	return []backend.CountPeroid{}, nil
}

func (es Backend) GetDeletedCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	return []backend.CountPeroid{}, nil
}

func (m Backend) GetErrors(int32, map[string][]string) ([]*backend.ChefErrorCount, error) {
	return nil, nil
}

func (m Backend) GetMissingNodeDurationCounts(durations []string) ([]backend.CountedDuration, error) {
	return nil, nil
}

func (m Backend) GetNodeMetadataCounts(filters map[string][]string, types []string, startDate,
	endDate string) ([]backend.TypeCount, error) {
	return nil, nil
}

func (m Backend) GetNodeRunsDailyStatusTimeSeries(nodeID string, startTime,
	endTime time.Time) ([]backend.RunDurationStatus, error) {
	return nil, nil
}

func (es Backend) GetLatestRunRolloutBreakdownCounts() (*backend.NodeSegmentRolloutProgress, error) {
	return nil, nil
}

func (es Backend) GetUniqueNodesCount(int64, time.Time) (int64, error) {
	return 0, nil
}
