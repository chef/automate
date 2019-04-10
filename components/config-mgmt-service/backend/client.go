//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend

import (
	"context"
	"time"

	ingest "github.com/chef/automate/components/ingest-service/backend"
)

// We would like this backend interface to be pluggable, therefore we will
// be creating a middle abstraction to call whatever backend we use or will
// use in the future. Think about it as a set of contracts that the API
// requires to fulfill the requests
type Client interface {
	// @params (nodeid)
	GetNode(string) (Node, error)
	// @params (page, perPage, sortField, ascending, filters)
	GetNodes(int, int, string, bool, map[string][]string) ([]Node, error)
	// @params (filters)
	GetNodesCounts(map[string][]string) (NodesCounts, error)
	// @params (node_id, page, per_page, filters, start, end)
	GetRuns(string, int, int, map[string][]string, string, string) ([]AbridgedConverge, error)
	// @params (filters start end)
	GetRunsCounts(map[string][]string, string, string) (RunsCounts, error)
	// @params (run_id, last_ccr)
	GetRun(string, time.Time) (Run, error)
	// @params (node_id)
	GetAttribute(string) (NodeAttribute, error)
	// @params (searchTerm)
	GetListForField(string) ([]string, error)
	// @params (type, text)
	GetSuggestions(string, string) ([]Suggestion, error)
	// @param (revision_id)
	GetPolicyCookbooks(string) (PolicyCookbooks, error)
	// @params (filters, start, end, pageSize, cursorDate, cursorID, ascending)
	GetActions(map[string][]string, time.Time, time.Time, int, time.Time, string, bool) ([]Action, int64, error)
	// @params (filters, start, end)
	GetActionEventTypeCounts(map[string][]string, time.Time, time.Time) (map[string]int64, error)
	// @params (filters, start, end)
	GetActionEventTaskCounts(map[string][]string, time.Time, time.Time) (map[string]int64, error)
	// @params (start, end, timezone, bucketSizeInHours, eventAction)
	GetEventString(map[string][]string, string, string, string, int, string) (EventString, error)
	// @params ()
	// returns (oldestIndexDate, indicesExist, error)
	GetDateOfOldestConvergeIndices() (time.Time, bool, error)
	// @params (ctx, start, end, filters, cursorDate, cursorID, pageSize, sortField, ascending)
	// returns (Node, error)
	GetInventoryNodes(context.Context, time.Time,
		time.Time, map[string][]string, time.Time,
		string, int, string, bool) ([]InventoryNode, error)
	// @params (ctx, start, end, filters, cursorField, cursorID, pageSize, sortField, ascending)
	// returns (Node, error)
	GetNodesPageByCurser(context.Context, time.Time,
		time.Time, map[string][]string, interface{},
		string, int, string, bool) ([]Node, error)
}

// Types that we consume from the ingest-service
//
// We would like to use a few struct definitions that we have in the ingestion
// service since it is the origin of our data. Therefore we will just import
// them as our own so we can call them as `backend.Type`
type Run ingest.Run
type Node ingest.Node
type NodeAttribute ingest.NodeAttribute
type ChefError ingest.ChefError
type ExpandedRunList ingest.ExpandedRunList
type Action ingest.InternalChefAction

type Deprecation ingest.Deprecation

type Resource ingest.Resource

type InventoryNode struct {
	EntityUUID       string     `json:"entity_uuid"`
	OrganizationName string     `json:"organization_name"`
	Checkin          time.Time  `json:"checkin"`
	EC2              ingest.Ec2 `json:"ec2"`
	Platform         string     `json:"platform"`
	PlatformFamily   string     `json:"platform_family"`
	PlatformVersion  string     `json:"platform_version"`
	ChefVersion      string     `json:"chef_version"`
	// There is a bug in the current code where
	// the last ccr time is stored as lastCCRReceived.
	LastCCRReceived time.Time `json:"lastCCRReceived"`
	NodeName        string    `json:"node_name"`
	Fqdn            string    `json:"fqdn"`
}

// NodesCounts type
//
// Summary of all the nodes counts
type NodesCounts struct {
	Success int64 `json:"success"`
	Failure int64 `json:"failure"`
	Missing int64 `json:"missing"`
	Total   int64 `json:"total"`
}

func (ns *NodesCounts) ComputeTotalNodes() {
	ns.Total = ns.Success + ns.Failure + ns.Missing
}

// RunsCounts type
//
// Summary of all the runs counts
type RunsCounts struct {
	Success int64 `json:"success"`
	Failure int64 `json:"failure"`
	Total   int64 `json:"total"`
}

// A Converge, sometimes called a Run, abridged to contain
// only the fields returned by the nodes/:node_id/runs
// endpoint, as opposed to the more data rich
// nodes/:node_id/runs/:run_id endpoint
type AbridgedConverge struct {
	StartTime string `json:"start_time"`
	RunID     string `json:"run_id"`
	EndTime   string `json:"end_time"`
	Status    string `json:"status"`
}

type Suggestion struct {
	Text  string  `json:"text"`
	Score float32 `json:"score"`
}

type PolicyCookbooks struct {
	PolicyName    string               `json:"policy_name"`
	CookbookLocks []PolicyCookbookLock `json:"cookbook_locks"`
}

type PolicyCookbookLock struct {
	CookbookName string `json:"cookbook_name"`
	PolicyID     string `json:"policy_id"`
}

type EventString struct {
	EventAction      string            `json:"event_action"`
	EventsCollection []EventCollection `json:"events_collection"`
}

type EventCollection struct {
	EventsCount []EventCount `json:"events_count"`
}

type EventCount struct {
	Name  string `json:"name"`
	Count int64  `json:"count"`
}
