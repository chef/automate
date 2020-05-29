package service

import (
	"bytes"
	"context"
	"encoding/json"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	secrets "github.com/chef/automate/api/external/secrets"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	"google.golang.org/grpc"
)

var (
	dataFeedAggregateTaskName = cereal.NewTaskName("data-feed-aggregate")
)

type DataFeedAggregateTaskParams struct {
	NodeIDs          map[string]NodeIDs
	UpdatedNodesOnly bool
	FeedStart        time.Time
	FeedEnd          time.Time
}

type DataFeedAggregateTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
	secrets   secrets.SecretsServiceClient
	db        *dao.DB
}

func NewDataFeedAggregateTask(dataFeedConfig *config.DataFeedConfig, cfgMgmtConn *grpc.ClientConn, complianceConn *grpc.ClientConn, secretsConn *grpc.ClientConn, db *dao.DB) *DataFeedAggregateTask {
	return &DataFeedAggregateTask{
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
		secrets:   secrets.NewSecretsServiceClient(secretsConn),
		db:        db,
	}
}

func (d *DataFeedAggregateTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	params := DataFeedAggregateTaskParams{}
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

	if len(datafeedMessages) == 0 {
		return nil, nil
	}

	var buffer bytes.Buffer
	for _, message := range datafeedMessages {
		data, _ := json.Marshal(message)
		data = bytes.ReplaceAll(data, []byte("\n"), []byte("\f"))
		buffer.Write(data)
		buffer.WriteString("\n")
	}

	destinations, err := d.db.ListDBDestinations()
	if err != nil {
		return nil, err
	}
	for destination := range destinations {
		log.WithFields(log.Fields{
			"name": destinations[destination].Name,
			"url":  destinations[destination].URL,
		}).Debug("Destination")

		username, password, err := GetCredentials(ctx, d.secrets, destinations[destination].Secret)

		if err != nil {
			log.Errorf("Error retrieving credentials, cannot send asset notification: %v", err)
		} else {
			// build and send notification for this rule
			notification := datafeedNotification{username: username, password: password, url: destinations[destination].URL, data: buffer}

			client := NewDataClient()
			err = send(client, notification)
			if err != nil {
				handleSendErr(notification, params.FeedStart, params.FeedEnd, err)
			}
		}
	}

	return nil, nil
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
			log.Warnf("Error getting node data %v", err)
		}

		report, err := d.getNodeComplianceData(ctx, ip, nodeID, updatedNodesOnly)
		if err != nil {
			log.Warnf("Error getting compliance data %v", err)
		}
		// update the message with the full report
		if report != nil {
			nodeData["report"] = report
		}
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
		_, macAddress, hostname, err := getNodeHostFields(ctx, d.cfgMgmt, filters)
		if err != nil {
			log.Warnf("Error getting node macaddress and hostname %v", err)
		}
		nodeDataContent := make(map[string]interface{})
		nodeDataContent["macaddress"] = macAddress
		nodeDataContent["hostname"] = hostname
		nodeDataContent["ipaddress"] = ipaddress
		nodeData["node"] = nodeDataContent
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
