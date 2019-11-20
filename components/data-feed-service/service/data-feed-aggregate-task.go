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
	dataFeedAggregateTaskName = cereal.NewTaskName("data-feed-aggregate")
)

type DataFeedAggregateTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
}

type DataFeedAggregateTaskResults struct {
	DataFeedMessages map[string]map[string]interface{}
	NodeIDs          map[string]NodeIDs
}

func NewDataFeedAggregateTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) (*DataFeedAggregateTask, error) {

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedAggregateTask{
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
	}, nil
}

func (d *DataFeedAggregateTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	nodeIDs := params.NodeIDs
	log.Debugf("DataFeedAggregateTask.Run %v", nodeIDs)
	datafeedMessages, err := d.buildDatafeed(ctx, nodeIDs, params.UpdatedNodesOnly)
	if err != nil {
		return nil, errors.Wrap(err, "failed to build chef client data")
	}

	// return the data feed but also return nodeids, these will be the NodeIDs of reports not yet collected
	// i.e. the reports on nodes which have not had a client run in the interval
	//return &DataFeedAggregateTaskResults{DataFeedMessages: &rawMessages, NodeIDs: nodeIDs}, nil
	return &DataFeedAggregateTaskResults{DataFeedMessages: datafeedMessages, NodeIDs: nodeIDs}, nil
}

func (d *DataFeedAggregateTask) buildDatafeed(ctx context.Context, nodeIDs map[string]NodeIDs, updatedNodesOnly bool) (map[string]map[string]interface{}, error) {

	log.Info("Building data feed...")

	nodeMessages := make(map[string]map[string]interface{})
	log.Debugf("buildDatafeed nodeIDs %v", nodeIDs)
	for ip, nodeID := range nodeIDs {
		if nodeID.ClientID == "" && nodeID.ComplianceID == "" {
			continue
		}

		log.Debugf("buildDataFeed ipaddress %s, value %v", ip, nodeID)

		nodeData, err := d.getNodeClientData(ctx, ip, nodeID, updatedNodesOnly)
		if err != nil {
			log.Errorf("Error getting node data %v", err)
			continue
		}

		if attributes, ok := nodeData["attributes"]; ok {
			ipaddress := attributes.(map[string]interface{})["ipaddress"].(string)

			if ip != ipaddress {
				log.Errorf("Error node ip address %s and attributes ip address %s mismatch", ip, ipaddress)
				continue
			}
		}

		report, err := d.getNodeComplianceData(ctx, ip, nodeID, updatedNodesOnly)
		if err != nil {
			log.Errorf("Error getting compliance data %v", err)
			continue
		}
		// update the message with the full report
		message := nodeData["node_data"].(DataFeedMessage)
		message.Report = report
		nodeData["node_data"] = message

		nodeMessages[ip] = nodeData
	}
	log.Debugf("%v node attribute messages retrieved in interval", len(nodeMessages))
	return nodeMessages, nil
}

func (d *DataFeedAggregateTask) getNodeClientData(ctx context.Context, ipaddress string, nodeID NodeIDs, updatedNodesOnly bool) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	var err error
	if nodeID.ClientID != "" || updatedNodesOnly == false {
		// get full node data
		log.Debugf("get full node data for NodeID  %v", nodeID)
		var filters []string
		if nodeID.ClientID != "" {
			filters = []string{"id:" + nodeID.ClientID}
		} else {
			filters = []string{"ipaddress:" + ipaddress}
		}
		// get the attributes and last client run data of each node
		nodeData, err = getNodeData(ctx, d.cfgMgmt, filters)

	} else if nodeID.ClientID == "" && updatedNodesOnly {
		// get hosts data
		filters := []string{"ipaddress:" + ipaddress}
		macAddress, hostname, err := getNodeHostFields(ctx, d.cfgMgmt, filters)
		if err != nil {
			log.Errorf("Error getting node macaddress and hostname %v", err)
		}
		nodeData["node_data"] = DataFeedMessage{Macaddress: macAddress, Hostname: hostname}
	}
	if err != nil {
		return nodeData, err
	}
	return nodeData, nil
}

func (d *DataFeedAggregateTask) getNodeComplianceData(ctx context.Context, ipaddress string, nodeID NodeIDs, updatedNodesOnly bool) (*reporting.Report, error) {
	var report *reporting.Report
	var err error
	if nodeID.ComplianceID != "" {
		id := &reporting.Query{Id: nodeID.ComplianceID}
		// get the full report
		report, err = d.reporting.ReadReport(ctx, id)
		if err != nil {
			log.Errorf("Error getting report by if %v", err)
		}
	} else if nodeID.ComplianceID == "" && updatedNodesOnly == false {
		filter := &reporting.ListFilter{Type: "ipaddress", Values: []string{ipaddress}}
		reportFilters := []*reporting.ListFilter{filter}

		ipaddressQuery := &reporting.Query{
			Page:    0,
			PerPage: 1,
			Filters: reportFilters,
			Sort:    "latest_report.end_time",
			Order:   reporting.Query_DESC,
		}
		// get the most recent compliance report summary for this node
		reports, err := d.reporting.ListReports(ctx, ipaddressQuery)
		if err == nil {
			if len(reports.Reports) != 0 {
				reportID := &reporting.Query{Id: reports.Reports[0].Id}
				report, err = d.reporting.ReadReport(ctx, reportID)
				if err != nil {
					log.Errorf("Error getting report by ip %v", err)
				}
			}
		} else {
			log.Errorf("Error listing reports %v", err)
		}
	}
	return report, err
}
