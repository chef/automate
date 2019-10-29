package service

import (
	"context"
	"strings"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var (
	dataFeedListReportsTaskName = cereal.NewTaskName("data-feed-list-reports")
)

type DataFeedListReportsTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
}

type DataFeedListReportsTaskResults struct {
	NodeIDs map[string]NodeIDs
}

func NewDataFeedListReportsTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) (*DataFeedListReportsTask, error) {

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedListReportsTask{
		reporting: reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:   cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
	}, nil
}

func (d *DataFeedListReportsTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	log.Debugf("DataFeedListReportsTask.Run %v", params)
	d.listReports(ctx, params.PollTaskParams.AssetPageSize, params.FeedStart, params.FeedEnd, params.NodeIDs)

	return &DataFeedListReportsTaskResults{NodeIDs: params.NodeIDs}, nil
}

// listReports update the ipaddress:NodeIDs map with the report ID of any compliance reports in the interval
func (d *DataFeedListReportsTask) listReports(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time, nodeIDs map[string]NodeIDs) {
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
