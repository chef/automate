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
	dataFeedComplianceTaskName = cereal.NewTaskName("data-feed-compliance")
)

type DataFeedComplianceTask struct {
	cfgMgmt   cfgmgmt.CfgMgmtClient
	reporting reporting.ReportingServiceClient
}

type DataFeedComplianceTaskParams struct {
	DataFeedMessages map[string]datafeedMessage
	FeedStart        time.Time
	FeedEnd          time.Time
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
	log.Infof("DataFeedComplianceTask.Run %v", params.ComplianceTaskParams)
	d.buildReportFeed(ctx, params.PollTaskParams.AssetPageSize, params.ComplianceTaskParams.FeedStart, params.ComplianceTaskParams.FeedEnd, params.ComplianceTaskParams.DataFeedMessages)
	if err != nil {
		return nil, errors.Wrap(err, "failed to build chef compliance data")
	}
	return &DataFeedComplianceTaskResults{DataFeedMessages: params.ComplianceTaskParams.DataFeedMessages}, nil
}

func (d *DataFeedComplianceTask) buildReportFeed(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time, datafeedMessages map[string]datafeedMessage) {
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
			if _, ok := datafeedMessages[ipaddress]; ok {
				log.Debugf("node data already exists for %v", ipaddress)
			} else {
				// the node hasn't had a client run in last window, but has a report so we can add the report
				id := &reporting.Query{Id: reports.Reports[report].Id}

				fullReport, err := d.reporting.ReadReport(ctx, id)
				if err != nil {
					log.Debugf("Error getting report by if %v", err)
					//TODO
				}
				filters := []string{"ipaddress:" + ipaddress}
				// node the latest node data associated with this report
				message, err := getNodeData(ctx, d.cfgMgmt, filters)
				message.Report = fullReport
				datafeedMessages[ipaddress] = message
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
