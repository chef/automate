package service

import (
	"context"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var (
	dataFeedClientTaskName = cereal.NewTaskName("data-feed-client")
)

type DataFeedClientTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
}

type DataFeedClientTaskResults struct {
	DataFeedMessages map[string]datafeedMessage
	NodeIDs          map[string]NodeIDs
}

func NewDataFeedClientTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) (*DataFeedClientTask, error) {

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedClientTask{
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
	}, nil
}

func (d *DataFeedClientTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	nodeIDs := params.NodeIDs
	log.Debugf("DataFeedClientTask.Run %v", nodeIDs)
	datafeedMessages, err := d.buildDatafeed(ctx, nodeIDs)
	if err != nil {
		return nil, errors.Wrap(err, "failed to build chef client data")
	}
	// return the data feed but also return nodeids, report ids not used argh
	return &DataFeedClientTaskResults{DataFeedMessages: datafeedMessages, NodeIDs: nodeIDs}, nil
}

func (d *DataFeedClientTask) buildDatafeed(ctx context.Context, nodeIDs map[string]NodeIDs) (map[string]datafeedMessage, error) {

	log.Info("Building data feed...")

	nodeMessages := make(map[string]datafeedMessage)
	log.Debugf("buildDatafeed nodeIDs %v", nodeIDs)
	for key, nodeID := range nodeIDs {
		if nodeID.ClientID == "" {
			continue
		}

		log.Debugf("buildDataFeed key %s, value %v", key, nodeID)
		filters := []string{"id:" + nodeID.ClientID}

		message, err := getNodeData(ctx, d.cfgMgmt, filters)
		if err != nil {
			log.Errorf("Error getting node data %v", err)
			continue
		}

		ipaddress := message.Automatic["ipaddress"].(string)
		log.Debugf("buildDataFeed key %s, node data ipaddress %v", key, ipaddress)
		filter := &reporting.ListFilter{Type: "ipaddress", Values: []string{ipaddress}}
		reportFilters := []*reporting.ListFilter{filter}

		ipaddressQuery := &reporting.Query{
			Page:    0,
			PerPage: 1,
			Filters: reportFilters,
			Sort:    "latest_report.end_time",
			Order:   reporting.Query_DESC,
		}
		reports, err := d.reporting.ListReports(ctx, ipaddressQuery)
		if err != nil {
			log.Errorf("Error listing reports %v", err)
			continue
		}

		fullReport := new(reporting.Report)
		if len(reports.Reports) != 0 {
			reportID := &reporting.Query{Id: reports.Reports[0].Id}
			fullReport, err = d.reporting.ReadReport(ctx, reportID)
			if err != nil {
				log.Errorf("Error getting report by ip %v", err)
				continue
			}
		}

		message.Report = fullReport
		nodeMessages[ipaddress] = message
		delete(nodeIDs, ipaddress)
	}
	log.Debugf("%v node attribute messages retrieved in interval", len(nodeMessages))
	return nodeMessages, nil
}
