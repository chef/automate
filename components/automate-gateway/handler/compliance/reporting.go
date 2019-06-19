package compliance

import (
	"context"

	"github.com/golang/protobuf/ptypes"

	version "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	jobsService "github.com/chef/automate/components/compliance-service/api/jobs"
	reportingService "github.com/chef/automate/components/compliance-service/api/reporting"
	versionService "github.com/chef/automate/components/compliance-service/api/version"
	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type Reporting struct {
	client  reportingService.ReportingServiceClient
	version versionService.VersionServiceClient
	scanner jobsService.JobsServiceClient
}

func NewReportingHandler(reportingClient reportingService.ReportingServiceClient, versionClient versionService.VersionServiceClient, jobsClient jobsService.JobsServiceClient) *Reporting {
	return &Reporting{
		client:  reportingClient,
		version: versionClient,
		scanner: jobsClient,
	}
}

// should cover /reports
func (a *Reporting) ListReports(ctx context.Context, in *reporting.Query) (*reporting.Reports, error) {
	inDomain := &reportingService.Query{}
	out := &reporting.Reports{}
	f := func() (proto.Message, error) {
		return a.client.ListReports(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /reportIds
func (a *Reporting) ListReportIds(ctx context.Context, in *reporting.Query) (*reporting.ReportIds, error) {
	inDomain := &reportingService.Query{}
	out := &reporting.ReportIds{}
	f := func() (proto.Message, error) {
		return a.client.ListReportIds(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /reports/:reportid
func (a *Reporting) ReadReport(ctx context.Context, in *reporting.Query) (*reporting.Report, error) {
	inDomain := &reportingService.Query{}
	out := &reporting.Report{}
	f := func() (proto.Message, error) {
		return a.client.ReadReport(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /suggestions
func (a *Reporting) ListSuggestions(ctx context.Context, in *reporting.SuggestionRequest) (*reporting.Suggestions, error) {
	inDomain := &reportingService.SuggestionRequest{}
	out := &reporting.Suggestions{}
	f := func() (proto.Message, error) {
		return a.client.ListSuggestions(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /search/profiles
func (a *Reporting) ListProfiles(ctx context.Context, in *reporting.Query) (*reporting.ProfileMins, error) {
	inDomain := &reportingService.Query{}
	out := &reporting.ProfileMins{}
	f := func() (proto.Message, error) {
		return a.client.ListProfiles(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Reporting) Export(*reporting.Query, reporting.ReportingService_ExportServer) error {
	// TODO: needs to be implemented with special care
	return nil
}

func (a *Reporting) ReadNode(ctx context.Context, in *reporting.Id) (*reporting.Node, error) {
	inDomain := &reportingService.Id{}
	out := &reporting.Node{}
	f := func() (proto.Message, error) {
		return a.client.ReadNode(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Reporting) ListNodes(ctx context.Context, in *reporting.Query) (*reporting.Nodes, error) {
	inDomain := &reportingService.Query{}
	out := &reporting.Nodes{}
	f := func() (proto.Message, error) {
		return a.client.ListNodes(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Reporting) GetVersion(ctx context.Context, in *gp.Empty) (*version.VersionInfo, error) {

	ver, err := a.version.Version(ctx, in)
	if err != nil {
		return nil, err
	}
	out := &version.VersionInfo{}
	err = protobuf.Convert(ver, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// LicenseUsageNodes gathers information about scanned nodes (as initiated by scan jobs)
// for owca billing purposes
func (a *Reporting) LicenseUsageNodes(ctx context.Context, in *reporting.TimeQuery) (*reporting.Reports, error) {
	logrus.Infof("making request with start time %s", in.GetStartTime())
	// convert start time to time.Time for later use
	startTime, err := ptypes.Timestamp(in.GetStartTime())
	if err != nil {
		return nil, errors.Wrap(err, "unable to parse time")
	}
	// get all scans with end_time on or after in time OR status running
	jobsList, err := a.scanner.ListInitiatedScans(ctx, &jobs.TimeQuery{StartTime: in.GetStartTime()})
	if err != nil {
		return nil, errors.Wrap(err, "could not list owca scans")
	}
	logrus.Debugf("found jobs %+v", jobsList)

	reports := make([]*reporting.Report, 0)
	// for each job, query compliance reporting with job_id filter, get nodes
	for _, jobId := range jobsList.GetIds() {
		nodesList, err := a.getAllNodes(ctx, jobId)
		if err != nil {
			return nil, errors.Wrap(err, "could not list nodes")
		}
		logrus.Debugf("LicenseUsageNodes found %d nodes ", len(nodesList))

		nodesListNoAPIScans := make([]*reporting.Report, 0)
		// go through the list of nodes found, add to return object if
		// 1) node (environment) != aws-api or azure-api
		// 2) end time is after start time
		for _, node := range nodesList {
			endTime, err := ptypes.Timestamp(node.GetLatestReport().GetEndTime())
			if err != nil {
				return nil, errors.Wrap(err, "unable to parse timestamp")
			}
			if node.Environment != "aws-api" && node.Environment != "azure-api" && endTime.After(startTime) {
				nodesListNoAPIScans = append(nodesListNoAPIScans, &reporting.Report{
					Id:          node.GetLatestReport().GetId(),
					JobId:       jobId,
					NodeId:      node.GetId(),
					NodeName:    node.GetName(),
					Environment: node.GetEnvironment(),
					Platform:    node.GetPlatform(),
					EndTime:     node.GetLatestReport().GetEndTime(),
					Status:      node.GetLatestReport().GetStatus(),
				})
			}
		}
		// append the nodesListNoAPIScans found from this job to the total reports
		reports = append(reports, nodesListNoAPIScans...)
	}
	logrus.Infof("found license usage nodes %+v", reports)
	return &reporting.Reports{Reports: reports, Total: int32(len(reports))}, nil
}

func (a *Reporting) getAllNodes(ctx context.Context, jobId string) ([]*reporting.Node, error) {
	var nodesList []*reporting.Node
	var pageNum int32 = 1
	for {
		nodesPage, err := a.ListNodes(ctx, &reporting.Query{
			PerPage: 1000,
			Page:    pageNum,
			Filters: []*reporting.ListFilter{
				{Type: "job_id", Values: []string{jobId}},
			},
		})
		if err != nil {
			return nil, err
		}
		if cap(nodesList) == 0 {
			nodesList = make([]*reporting.Node, 0, nodesPage.GetTotal())
		}

		nodesList = append(nodesList, nodesPage.GetNodes()...)
		if int32(len(nodesList)) >= nodesPage.GetTotal() {
			break
		}
		pageNum++
	}
	return nodesList, nil
}
