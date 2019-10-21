package service

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	secrets "github.com/chef/automate/api/external/secrets"
	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var (
	dataFeedPollTaskName = cereal.NewTaskName("data-feed-poll")
)

type DataFeedPollTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	secrets   secrets.SecretsServiceClient
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
	Destinations []dao.Destination
	NodeIDs      []string
	FeedStart    time.Time
	FeedEnd      time.Time
}

func NewDataFeedPollTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory, db *dao.DB, manager *cereal.Manager) (*DataFeedPollTask, error) {

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	secretsConn, err := connFactory.Dial("secrets-service", dataFeedConfig.SecretsConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to secrets-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedPollTask{
		secrets:   secrets.NewSecretsServiceClient(secretsConn),
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
		db:        db,
		manager:   manager,
	}, nil
}

func (d *DataFeedPollTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	log.Infof("DataFeedPollTask.Run params %v", params)
	now := time.Now()
	params = d.getFeedTimes(params, now)
	feedStartTime := params.PollTaskParams.NextFeedStart
	feedEndTime := params.PollTaskParams.NextFeedEnd
	destinations, err := d.db.ListDBDestinations()
	if err != nil {
		return nil, errors.Wrap(err, "failed to get destinations from db")
	}
	params.ComplianceTaskParams.FeedStart = feedStartTime
	params.ComplianceTaskParams.FeedEnd = feedEndTime
	params.PollTaskParams.NextFeedStart = feedEndTime
	params.PollTaskParams.NextFeedEnd = feedEndTime.Add(params.PollTaskParams.FeedInterval)

	if len(destinations) == 0 {
		log.Info("DataFeedPollTask.Run no destiantions returning")
		return nil, nil
	}

	nodeIDs, err := d.GetChangedNodes(ctx, params.PollTaskParams.AssetPageSize, feedStartTime, feedEndTime)
	log.Infof("DataFeedPollTask nodes %v", nodeIDs)
	if err != nil {
		return nil, err
	}

	params.ClientTaskParams.NodeIDs = nodeIDs
	log.Debugf("Updated Feed interval start, end: %s, %s", params.PollTaskParams.NextFeedStart.Format("15:04:05"), params.PollTaskParams.NextFeedEnd.Format("15:04:05"))
	log.Debugf("Updated Compliance interval start, end: %s, %s", params.ComplianceTaskParams.FeedStart.Format("15:04:05"), params.ComplianceTaskParams.FeedEnd.Format("15:04:05"))

	err = d.manager.UpdateWorkflowScheduleByName(context.Background(), dataFeedScheduleName, dataFeedWorkflowName,
		cereal.UpdateParameters(params),
		cereal.UpdateEnabled(true))
	if err != nil {
		return nil, err
	}

	return &DataFeedPollTaskResults{
		Destinations: destinations,
		NodeIDs:      nodeIDs,
		FeedStart:    feedStartTime,
		FeedEnd:      feedEndTime,
	}, nil
}

func (d *DataFeedPollTask) getFeedTimes(params DataFeedWorkflowParams, now time.Time) DataFeedWorkflowParams {

	if params.PollTaskParams.NextFeedStart.IsZero() {
		params.PollTaskParams.NextFeedEnd = d.getFeedEndTime(params.PollTaskParams.FeedInterval, now)
		params.PollTaskParams.NextFeedStart = params.PollTaskParams.NextFeedEnd.Add(-params.PollTaskParams.FeedInterval)
	}

	log.Debugf("Current Feed interval start, end: %s, %s", params.PollTaskParams.NextFeedStart.Format("15:04:05"), params.PollTaskParams.NextFeedEnd.Format("15:04:05"))
	return params
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

func (d *DataFeedPollTask) GetChangedNodes(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time) ([]string, error) {
	log.Debug("Inventory nodes start")
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

	nodeIDs := make([]string, 0)
	log.Debugf("No of inventory nodes %v", len(inventoryNodes.Nodes))
	for len(inventoryNodes.Nodes) > 0 {
		for _, node := range inventoryNodes.Nodes {
			log.Debugf("Inventory node #%v", node)
			nodeIDs = append(nodeIDs, node.Id)
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
	log.Infof("NODE ID LENGTH: %v", len(nodeIDs))
	return nodeIDs, nil
}
