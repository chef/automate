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
	dataFeedComplianceTaskName = cereal.NewTaskName("data-feed-compliance")
)

type DataFeedComplianceTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
}

type DataFeedComplianceTaskResults struct {
	DataFeedMessages map[string]datafeedMessage
}

func NewDataFeedComplianceTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) (*DataFeedComplianceTask, error) {

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedComplianceTask{
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
	}, nil
}

func (d *DataFeedComplianceTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	log.Debugf("DataFeedComplianceTask.Run %v", params)
	dataFeedMessages := d.buildReportFeed(ctx, params.NodeIDs)

	return &DataFeedComplianceTaskResults{DataFeedMessages: dataFeedMessages}, nil
}

func (d *DataFeedComplianceTask) buildReportFeed(ctx context.Context, nodeIDs map[string]NodeIDs) map[string]datafeedMessage {
	dataFeedMessages := make(map[string]datafeedMessage)
	for ipaddress, nodeID := range nodeIDs {
		log.Debugf("nodeIDs has ipaddress %v", ipaddress)

		// the node hasn't had a client run in last window, but has a report so we can add the report
		id := &reporting.Query{Id: nodeID.ComplianceID}
		// get the full report
		fullReport, err := d.reporting.ReadReport(ctx, id)
		if err != nil {
			log.Errorf("Error getting report by if %v", err)
			//TODO
		}
		filters := []string{"ipaddress:" + ipaddress}
		// node the latest node data associated with this report
		// i.e. latest attribute and client data but won't have been in the interval
		message, err := getNodeData(ctx, d.cfgMgmt, filters)
		if err != nil {
			// TODO
			log.Errorf("Error getting node data %v", err)
		}
		message.Report = fullReport
		dataFeedMessages[ipaddress] = message

	}
	return dataFeedMessages
}
