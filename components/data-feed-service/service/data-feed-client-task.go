package service

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
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

type DataFeedClientTaskParams struct {
	NodeIDs []string
}

type DataFeedClientTaskResults struct {
	DataFeedMessages map[string]datafeedMessage
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
	log.Infof("DataFeedClientTask.Run %v", params.ClientTaskParams.NodeIDs)
	datafeedMessages, err := d.buildDatafeed(ctx, params.ClientTaskParams.NodeIDs)
	if err != nil {
		return nil, errors.Wrap(err, "failed to build chef client data")
	}

	return &DataFeedClientTaskResults{DataFeedMessages: datafeedMessages}, nil
}

func (d *DataFeedClientTask) buildDatafeed(ctx context.Context, nodeIDs []string) (map[string]datafeedMessage, error) {

	log.Info("Building data feed...")

	nodeMessages := make(map[string]datafeedMessage)
	log.Infof("BUILD FEED NODE IDS %v", nodeIDs)
	for _, nodeID := range nodeIDs {
		filters := []string{"id:" + nodeID}

		message, err := getNodeData(ctx, d.cfgMgmt, filters)
		if err != nil {
			log.Errorf("Error getting node data %v", err)
			//return nil, err
			continue
		}

		ipaddress := message.Automatic["ipaddress"].(string)

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
		// might not be a report!

		//var fullReport *reporting.
		fullReport := new(reporting.Report)
		if len(reports.Reports) != 0 {
			reportID := &reporting.Query{Id: reports.Reports[0].Id}
			fullReport, err = d.reporting.ReadReport(ctx, reportID)
			// TODO err handling
			if err != nil {
				log.Errorf("Error getting report by ip %v", err)
				continue
			}
		}

		message.Report = fullReport
		nodeMessages[ipaddress] = message

	}
	log.Debugf("%v node attribute messages retrieved in interval", len(nodeMessages))
	return nodeMessages, nil
}

func getNodeData(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) (datafeedMessage, error) {

	nodeFilters := &cfgmgmtRequest.Nodes{Filter: filters}
	nodes, err := client.GetNodes(ctx, nodeFilters)
	if err != nil {
		log.Errorf("Error getting cfgmgmt/nodes %v", err)
		return datafeedMessage{}, err
	}

	nodeStruct := nodes.Values[0].GetStructValue()
	id := nodeStruct.Fields["id"].GetStringValue()
	lastRunId := nodeStruct.Fields["latest_run_id"].GetStringValue()
	nodeAttributes, err := client.GetAttributes(ctx, &cfgmgmtRequest.Node{NodeId: id})
	if err != nil {
		log.Errorf("Error getting attributes %v", err)
		return datafeedMessage{}, err
	}
	var automaticJson map[string]interface{}
	err = json.Unmarshal([]byte(nodeAttributes.Automatic), &automaticJson)
	if err != nil {
		log.Errorf("Could not parse automatic attributes from json: %v", err)
		return datafeedMessage{}, err
	}
	attributesJson := buildDynamicJson(automaticJson)

	lastRun, err := client.GetNodeRun(ctx, &cfgmgmtRequest.NodeRun{NodeId: id, RunId: lastRunId})

	if err != nil {
		log.Errorf("Error getting node run %v", err)
		return datafeedMessage{}, err
	} else {
		log.Debugf("Last run\n %v", lastRun)
	}

	return datafeedMessage{Automatic: attributesJson, LastRun: lastRun}, nil
}

func buildDynamicJson(automaticJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised
	var dynamicJson map[string]interface{}
	if automaticJson["lsb"] != nil {
		dynamicJson = buildUbuntuJson(automaticJson)
	} else if automaticJson["os"] == "windows" {
		dynamicJson = buildWindowsJson(automaticJson)
	}

	return dynamicJson
}

func buildUbuntuJson(ubuntuJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised

	lsb := ubuntuJson["lsb"].(map[string]interface{})
	ubuntuJson["description"] = lsb["description"]

	dmi := ubuntuJson["dmi"].(map[string]interface{})
	system := dmi["system"].(map[string]interface{})
	ubuntuJson["serial_number"] = system["serial_number"]

	//delete(ubuntuJson, "lsb")
	//delete(ubuntuJson, "system")

	return ubuntuJson
}

func buildWindowsJson(windowsJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised

	kernel := windowsJson["kernel"].(map[string]interface{})
	windowsJson["description"] = kernel["name"]
	osInfo := kernel["os_info"].(map[string]interface{})
	windowsJson["serial_number"] = osInfo["serial_number"]
	servicePackMajorVersion := fmt.Sprintf("%g", osInfo["service_pack_major_version"].(float64))
	servicePackMinorVersion := fmt.Sprintf("%g", osInfo["service_pack_minor_version"].(float64))
	servicePack := strings.Join([]string{servicePackMajorVersion, servicePackMinorVersion}, ".")
	windowsJson["os_service_pack"] = servicePack

	//delete(windowsJson, "kernel")
	// TODO we could get the chassis from the kernel attributes

	return windowsJson
}
