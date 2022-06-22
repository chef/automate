package service

import (
	"bytes"
	"context"
	"encoding/json"
	"net"
	"time"

	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"

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
	cfgMgmt             cfgmgmt.CfgMgmtServiceClient
	reporting           reporting.ReportingServiceClient
	secrets             secrets.SecretsServiceClient
	db                  *dao.DB
	externalFqdn        string
	acceptedStatusCodes []int32
	contentType         string
}

func NewDataFeedAggregateTask(dataFeedConfig *config.DataFeedConfig, cfgMgmtConn *grpc.ClientConn, complianceConn *grpc.ClientConn, secretsConn *grpc.ClientConn, db *dao.DB) *DataFeedAggregateTask {
	return &DataFeedAggregateTask{
		reporting:           reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:             cfgmgmt.NewCfgMgmtServiceClient(cfgMgmtConn),
		secrets:             secrets.NewSecretsServiceClient(secretsConn),
		db:                  db,
		externalFqdn:        dataFeedConfig.ServiceConfig.ExternalFqdn,
		acceptedStatusCodes: dataFeedConfig.ServiceConfig.AcceptedStatusCodes,
		contentType:         dataFeedConfig.ServiceConfig.ContentType,
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
			"name":              destinations[destination].Name,
			"url":               destinations[destination].URL,
			"services":          destinations[destination].Services,
			"integration_types": destinations[destination].IntegrationTypes,
			"MetaData":          destinations[destination].MetaData,
		}).Debug("Destination")
		if destinations[destination].Enable {
			credentials, err := GetCredentials(ctx, d.secrets, destinations[destination].Secret, destinations[destination].Services, destinations[destination].IntegrationTypes, destinations[destination].MetaData)

			if err != nil {
				log.Errorf("Error retrieving credentials, cannot send asset notification: %v", err)
			} else {
				// build and send notification for this rule
				notification := datafeedNotification{credentials: credentials, url: destinations[destination].URL, data: buffer, contentType: d.contentType, services: destinations[destination].Services, integrationTypes: destinations[destination].IntegrationTypes}

				client := NewDataClient(d.acceptedStatusCodes)
				err = send(client, notification)
				if err != nil {
					handleSendErr(notification, params.FeedStart, params.FeedEnd, err)
				}
			}
		}
	}

	return nil, nil
}

func (d *DataFeedAggregateTask) buildDatafeed(ctx context.Context, nodeIDs map[string]NodeIDs, updatedNodesOnly bool) (map[string]map[string]interface{}, error) {

	log.Info("Building data feed...")

	nodeMessages := make(map[string]map[string]interface{})
	log.Debugf("buildDatafeed nodeIDs %v", nodeIDs)
	// might not be ip, might be the inspec node_id if there are no infra nodes e.g. a cloud resource
	for resourceId, nodeID := range nodeIDs {
		if nodeID.ClientID == "" && nodeID.ComplianceID == "" {
			continue
		}

		log.Debugf("buildDataFeed resourceId %s, value %v", resourceId, nodeID)
		// if resourceId is an IP address we get client data returned
		nodeData, err := d.getNodeClientData(ctx, resourceId, nodeID, updatedNodesOnly)
		if err != nil {
			log.Warnf("Error getting node data %v", err)
		}

		report, err := d.getNodeComplianceData(ctx, resourceId, nodeID, updatedNodesOnly)
		if err != nil {
			log.Warnf("Error getting compliance data %v", err)
		}
		// update the message with the full report
		if report != nil {
			nodeData["report"] = report
		}
		nodeMessages[resourceId] = nodeData
	}
	log.Debugf("%v node attribute messages retrieved in interval", len(nodeMessages))
	return nodeMessages, nil
}

func (d *DataFeedAggregateTask) getNodeClientData(ctx context.Context, resourceId string, nodeID NodeIDs, updatedNodesOnly bool) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	var ipaddress string
	if isIPAddress(resourceId) {
		ipaddress = resourceId
	}

	var err error
	if nodeID.ClientID != "" || updatedNodesOnly == false {
		// get full node data
		log.Debugf("get full node data for NodeID  %v", nodeID)
		var filters []string
		if nodeID.ClientID != "" {
			filters = []string{"id:" + nodeID.ClientID}
		} else if isIPAddress(resourceId) {
			filters = []string{"ipaddress:" + ipaddress}
		}
		// get the attributes and last client run data of each node
		nodeData, err = d.safeGetNodeData(ctx, filters)

	} else if nodeID.ClientID == "" && updatedNodesOnly && isIPAddress(resourceId) {
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
		nodeDataContent["automate_fqdn"] = d.externalFqdn
		nodeData["node"] = nodeDataContent
	}
	if err != nil {
		return nodeData, err
	}
	return nodeData, nil
}

func isIPAddress(resourceId string) bool {
	if net.ParseIP(resourceId) != nil {
		return true
	}
	return false
}

func (d *DataFeedAggregateTask) safeGetNodeData(ctx context.Context, filters []string) (map[string]interface{}, error) {
	if len(filters) == 0 {
		return make(map[string]interface{}), nil
	}
	return d.getNodeData(ctx, filters)
}

func (d *DataFeedAggregateTask) getNodeData(ctx context.Context, filters []string) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	client := d.cfgMgmt
	nodeId, lastRunId, err := getNodeFields(ctx, client, filters)
	if err != nil {
		return nodeData, err
	}

	attributesJson, err := getNodeAttributes(ctx, client, nodeId)
	if err != nil {
		return nodeData, err
	}

	nodeData["attributes"] = attributesJson

	lastRun, err := client.GetNodeRun(ctx, &cfgmgmtRequest.NodeRun{NodeId: nodeId, RunId: lastRunId})

	if err != nil {
		log.Errorf("Error getting node run %v", err)
		return nodeData, err
	}
	automaticAttrs := attributesJson["automatic"].(map[string]interface{})
	ipaddress, macAddress, hostname := getHostAttributes(automaticAttrs)
	nodeDataContent := make(map[string]interface{})
	nodeData["client_run"] = lastRun
	nodeDataContent["macaddress"] = macAddress
	nodeDataContent["hostname"] = hostname
	nodeDataContent["ipaddress"] = ipaddress
	nodeDataContent["automate_fqdn"] = d.externalFqdn
	addDataContent(nodeDataContent, automaticAttrs)
	nodeData["node"] = nodeDataContent
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
