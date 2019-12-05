package service

import (
	"context"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	"google.golang.org/grpc"
)

var (
	dataFeedPollTaskName = cereal.NewTaskName("data-feed-poll")
)

type DataFeedPollTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
	db        *dao.DB
	manager   *cereal.Manager
}

type DataFeedPollTaskParams struct {
	AssetPageSize   int32
	ReportsPageSize int32
	FeedInterval    time.Duration
	NextFeedStart   time.Time
	NextFeedEnd     time.Time
}

type DataFeedPollTaskResults struct {
	NodeIDs   map[string]NodeIDs
	FeedStart time.Time
	FeedEnd   time.Time
}

// ClientID the node ID assigned by chef client
// ComplianceID the node ID in a compliance report
type NodeIDs struct {
	ClientID     string
	ComplianceID string
}

func NewDataFeedPollTask(dataFeedConfig *config.DataFeedConfig, cfgMgmtConn *grpc.ClientConn, complianceConn *grpc.ClientConn, db *dao.DB, manager *cereal.Manager) *DataFeedPollTask {
	return &DataFeedPollTask{
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
		reporting: reporting.NewReportingServiceClient(complianceConn),
		db:        db,
		manager:   manager,
	}
}

func (d *DataFeedPollTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	log.WithFields(log.Fields{
		"params":    params,
		"FeedStart": params.FeedStart,
		"FeedEnd":   params.FeedEnd,
		"NextStart": params.PollTaskParams.NextFeedStart,
		"NextEnd":   params.PollTaskParams.NextFeedEnd,
	}).Debug("DataFeedPollTask.Run()")

	params.FeedStart, params.FeedEnd = d.getFeedTimes(params, time.Now())
	params.PollTaskParams.NextFeedStart = params.FeedEnd
	params.PollTaskParams.NextFeedEnd = params.FeedEnd.Add(params.PollTaskParams.FeedInterval)
	taskResults := &DataFeedPollTaskResults{
		FeedStart: params.FeedStart,
		FeedEnd:   params.FeedEnd,
	}

	// we must update the workflow params to ensure the nest schedule has the update interval times
	err = d.manager.UpdateWorkflowScheduleByName(ctx, dataFeedScheduleName, dataFeedWorkflowName,
		cereal.UpdateParameters(params),
		cereal.UpdateEnabled(true))
	if err != nil {
		return taskResults, err
	}

	destinations, err := d.db.ListDBDestinations()
	if err != nil {
		return taskResults, errors.Wrap(err, "failed to get destinations from db")
	}

	if len(destinations) == 0 {
		log.Info("DataFeedPollTask.Run no destinations returning")
		return taskResults, nil
	}

	nodeIDs, err := d.GetChangedNodes(ctx, params.PollTaskParams.AssetPageSize, params.FeedStart, params.FeedEnd)
	log.Debugf("DataFeedPollTask found %v nodes", len(nodeIDs))
	if err != nil {
		return taskResults, err
	}
	params.NodeIDs = nodeIDs

	d.listReports(ctx, params.PollTaskParams.AssetPageSize, params.FeedStart, params.FeedEnd, params.NodeIDs)
	taskResults.NodeIDs = params.NodeIDs

	return taskResults, nil
}

// getFeedTimes determines the start and end times for the next interval to poll
// It returns the updated feed start time and feed end time
func (d *DataFeedPollTask) getFeedTimes(params DataFeedWorkflowParams, now time.Time) (time.Time, time.Time) {
	/*
	 * If automate has been down for a significant period of time a due schedule may already be enqueued
	 * and not updated by the manager update call on data-feed-service startup.
	 * In that case we need to determine if the next feed interval is stale and re-initialise it
	 */
	nextFeedStart := params.PollTaskParams.NextFeedStart
	nextFeedEnd := params.PollTaskParams.NextFeedEnd
	lag := now.Sub(params.PollTaskParams.NextFeedEnd).Minutes()
	log.WithFields(log.Fields{
		"lag":      lag,
		"interval": params.PollTaskParams.FeedInterval.Minutes(),
	}).Debug("Feed lag and interval")
	if params.PollTaskParams.NextFeedStart.IsZero() || lag > params.PollTaskParams.FeedInterval.Minutes() {
		nextFeedEnd = d.getFeedEndTime(params.PollTaskParams.FeedInterval, now)
		nextFeedStart = nextFeedEnd.Add(-params.PollTaskParams.FeedInterval)
		log.WithFields(log.Fields{
			"start": nextFeedStart.Format("15:04:05"),
			"end":   nextFeedEnd.Format("15:04:05"),
		}).Debug("Initialise Feed interval")
	} else {
		log.WithFields(log.Fields{
			"start": nextFeedStart.Format("15:04:05"),
			"end":   nextFeedEnd.Format("15:04:05"),
		}).Debug("Current Feed interval")
	}
	return nextFeedStart, nextFeedEnd
}

func (d *DataFeedPollTask) getFeedEndTime(feedInterval time.Duration, now time.Time) time.Time {
	log.Debugf("Time Now: %s", now.Format("15:04:05"))
	/*
	 * We round the current time down based on the interval duration to get the end of the last interval.
	 *
	 * Round will round either up or down to the nearest value of the inteval duration.
	 * e.g 1:20pm rounds to 1pm, 1:40pm rounds to 2pm
	 *
	 * If we have rounded down that will be our feed end time. The end of a clock interval
	 * rather than current time e.g. 1pm
	 */
	feedEndTime := now.Round(feedInterval)
	log.Debugf("FeedInterval/Units: %s", feedInterval)
	/*
	 * If we have rounded up we subtract the interval to effectively round down
	 */
	if feedEndTime.After(now) {
		feedEndTime = feedEndTime.Add(-feedInterval)
		log.Debugf("feedEndTime after: %s", feedEndTime.Format("15:04:05"))
	}
	log.Debugf("feedEndTime: %s", feedEndTime.Format("15:04:05"))
	return feedEndTime
}

// GetChangedNodes calls the nodes API to get any nodes whch have had a client run during the interval
// Returns a map of ipaddress to NodeIDs struct
func (d *DataFeedPollTask) GetChangedNodes(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time) (map[string]NodeIDs, error) {
	log.Debugf("Inventory nodes start %v, %v, %v", pageSize, feedStartTime, feedEndTime)
	feedStartString, err := ptypes.TimestampProto(feedStartTime)
	if err != nil {
		return nil, err
	}
	feedEndString, err := ptypes.TimestampProto(feedEndTime)
	if err != nil {
		return nil, err
	}

	nodesRequest := &cfgmgmtRequest.InventoryNodes{
		PageSize: pageSize,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &cfgmgmtRequest.Sorting{
			Order: cfgmgmtRequest.Order_desc,
		},
	}

	inventoryNodes, err := d.cfgMgmt.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, err
	}

	nodeIDs := make(map[string]NodeIDs, 0)
	log.Debugf("No of inventory nodes %v", len(inventoryNodes.Nodes))
	for len(inventoryNodes.Nodes) > 0 {
		for _, node := range inventoryNodes.Nodes {
			log.Debugf("Inventory node %v", node.Id)
			nodeIDs[node.Ipaddress] = NodeIDs{ClientID: node.Id}
		}
		lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		nodesRequest.CursorId = lastNode.Id
		nodesRequest.CursorDate = lastNode.Checkin

		inventoryNodes, err = d.cfgMgmt.GetInventoryNodes(ctx, nodesRequest)
		log.Debugf("inventory nodes %v, cursor %v", len(inventoryNodes.Nodes), lastNode.Id)
		if err != nil {
			return nil, err
		}
	}
	log.Debugf("found %v nodes", len(nodeIDs))
	return nodeIDs, nil
}

func (d *DataFeedPollTask) listReports(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time, nodeIDs map[string]NodeIDs) {
	feedStartString := strings.SplitAfter(feedStartTime.Format(time.RFC3339), "Z")[0]
	feedEndString := strings.SplitAfter(feedEndTime.Format(time.RFC3339), "Z")[0]
	log.Infof("Building report feed... %v - %v", feedStartString, feedEndString)

	startFilter := &reporting.ListFilter{Type: "start_time", Values: []string{feedStartString}}
	endFilter := &reporting.ListFilter{Type: "end_time", Values: []string{feedEndString}}

	filters := []*reporting.ListFilter{startFilter, endFilter}

	// page is not something we can configure we should start with
	// page at 1 and work out how many calls to make based on page
	// size divide the total by page size and add 1 and we loop
	// over that
	page := int32(1)
	query := &reporting.Query{
		Page:    page,
		PerPage: pageSize,
		Filters: filters,
		Sort:    "latest_report.end_time",
		Order:   reporting.Query_DESC,
	}
	log.Debugf("report query %v", query)

	reports, err := d.reporting.ListReports(ctx, query)
	if err != nil {
		log.Errorf("Error getting reporting/ListReports %v", err)
	}

	pages := (reports.Total / pageSize)
	if (reports.Total % pageSize) != 0 {
		pages++
	}
	log.Debugf("Total reports: %v, reports per page: %v, total pages %v: ",
		reports.Total, pageSize, pages)

	// get reports from the pages
	for page <= pages {
		log.Debugf("report query %v", query)
		for report := range reports.Reports {
			ipaddress := reports.Reports[report].Ipaddress
			log.Debugf("report has ipaddress %v", ipaddress)
			if _, ok := nodeIDs[ipaddress]; ok {
				// We must have a client run in the interval for the node with this ip
				log.Debugf("node data already exists for %v", ipaddress)
				nodeID := nodeIDs[ipaddress]
				// set the NodeId.ComplianceID for this ip to the report ID
				nodeID.ComplianceID = reports.Reports[report].Id
				nodeIDs[ipaddress] = nodeID
			} else {
				// ipaddress not in the mao so we add a new map entry with the report ID as ComplianceID
				nodeIDs[ipaddress] = NodeIDs{ComplianceID: reports.Reports[report].Id}
				log.Debugf("nodeID added %v", nodeIDs[ipaddress])
			}
		}
		page++
		query = &reporting.Query{
			Page:    page,
			PerPage: pageSize,
			Filters: filters}
		reports, err = d.reporting.ListReports(ctx, query)
		if err != nil {
			log.Errorf("Error getting reporting/ListReports %v", err)
		}
	}
}
