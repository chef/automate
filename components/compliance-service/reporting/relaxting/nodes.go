package relaxting

import (
	"encoding/json"
	"fmt"

	"github.com/golang/protobuf/ptypes"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

type ProfileSource struct {
	Sha256       string                       `json:"sha256"`
	Name         string                       `json:"name"`
	Version      string                       `json:"version"`
	ControlsSums reporting.NodeControlSummary `json:"controls_sums"`
	Status       string                       `json:"status"`
}

type ControlSource struct {
	ID     string  `json:"id"`
	Impact float32 `json:"impact"`
	Status string  `json:"status"`
}

// TODO: header with amount of results
// TODO: need to be able to sort on
//  "latest_report.controls.failed.total":    "controls.failed.total",
//  "latest_report.controls.failed.critical": "controls.failed.critical"
//GetNodes - list all of the nodes or all nodes for a profile-id
func (backend *ES2Backend) GetNodes(from int32, size int32, filters map[string][]string,
	sortField string, sortAsc bool) ([]*reportingapi.Node, int64, error) {
	myName := "GetNodes"

	depth, err := backend.NewDepth(filters, false, true)
	if err != nil {
		return nil, 0, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	logrus.Debugf("%s will retrieve all nodes", myName)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"environment",
		"platform.name",
		"platform.release",
		"report_uuid",
		"end_time")

	if queryInfo.level == ReportLevel {
		fsc.Include(
			"status",
			"controls_sums")
	}

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(queryInfo.filtQuery).
		Sort(sortField, sortAsc).
		From(int(from)).
		Size(int(size))

	// Adding a second sort field when we don't sort by node name
	if sortField != "node_name.lower" {
		searchSource.Sort("node_name.lower", true)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, 0, errors.Wrapf(err, "%s unable to get Source", myName)
	}
	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query searchSource", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(context.Background())

	if err != nil {
		return nil, 0, errors.Wrapf(err, "%s unable to complete search", myName)
	}
	nodes := make([]*reportingapi.Node, 0)
	// extract node information from ESInSpecSummary into Node
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var item ESInSpecSummary
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &item)
				if err == nil {
					node := reportingapi.Node{
						Id:          item.NodeID,
						Name:        item.NodeName,
						Environment: item.Environment,
					}

					var platform reportingapi.Platform
					platform.Name = item.Platform.Name
					platform.Release = item.Platform.Release
					node.Platform = &platform
					reportID := hit.Id
					if item.ReportID != "" {
						reportID = item.ReportID
					}
					var latestReport reportingapi.LatestReportSummary
					latestReport.Id = reportID

					timestamp, err := ptypes.TimestampProto(item.EndTime)
					if err != nil {
						logrus.Errorf("%s time error: %s", myName, err.Error())
					} else {
						latestReport.EndTime = timestamp
					}
					node.LatestReport = &latestReport

					var nodeControlSummary reporting.NodeControlSummary
					var status string

					if queryInfo.level == ReportLevel {
						nodeControlSummary = item.ControlsSums
						status = item.Status
					} else if queryInfo.level == ProfileLevel || queryInfo.level == ControlLevel {
						nodeControlSummary, status, err = getDeepControlsSums(hit, queryInfo)
						if err != nil {
							//todo - handle this
							logrus.Errorf("%s time error: %s", myName, err.Error())
						}
					}

					latestReport.Controls = convertToRSControlSummary(nodeControlSummary)
					latestReport.Status = status

					nodes = append(nodes, &node)
				} else {
					logrus.Errorf("%s unmarshal error: %s", myName, err.Error())
				}
			}

		}
		return nodes, searchResult.TotalHits(), nil
	}

	logrus.Debugf("%s Found no nodes\n", myName)
	return nodes, 0, nil
}

func convertToRSControlSummary(summ reporting.NodeControlSummary) *reportingapi.ControlSummary {
	var controlSummary reportingapi.ControlSummary
	var passedSum reportingapi.Total
	passedSum.Total = int32(summ.Passed.Total)
	var skippedSum reportingapi.Total
	skippedSum.Total = int32(summ.Skipped.Total)
	var failedSums reportingapi.Failed
	failedSums.Total = int32(summ.Failed.Total)
	failedSums.Critical = int32(summ.Failed.Critical)
	failedSums.Minor = int32(summ.Failed.Minor)
	failedSums.Major = int32(summ.Failed.Major)
	controlSummary.Passed = &passedSum
	controlSummary.Skipped = &skippedSum
	controlSummary.Failed = &failedSums
	controlSummary.Total = int32(summ.Total)
	return &controlSummary
}

func (backend *ES2Backend) GetNode(nodeUuid string) (*reportingapi.Node, error) {
	node := new(reportingapi.Node)

	client, err := backend.ES2Client()

	if err != nil {
		return node, errors.Wrap(err, "GetNode, cannot connect to ElasticSearch")
	}

	esIndex := ComplianceDailyRepTwenty
	idQuery := elastic.NewTermQuery("node_uuid", nodeUuid)
	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"environment",
		"platform",
		"profiles.name",
		"profiles.version",
		"profiles.namespace",
		"profiles.sha256",
		"end_time",
		"report_uuid",
		"status",
		"controls_sums")

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(idQuery).
		Sort("end_time", false). // Needed to pick up the most recent report
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "GetNode unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetNode query searchSource")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source").
		Do(context.Background())

	if err != nil {
		return nil, errors.Wrap(err, "GetNode unable to complete search")
	}

	logrus.Debugf("GetNode got %d nodes in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)
	LogQueryPartMin(esIndex, searchResult.Hits, "GetNode - search results hits")

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var item ESInSpecReport
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &item)
				if err == nil {
					// read all profiles
					profiles := make([]*reportingapi.ProfileMeta, 0)
					for _, profileMin := range item.Profiles {
						profiles = append(profiles, &reportingapi.ProfileMeta{
							Name:    profileMin.Name,
							Version: profileMin.Version,
							Id:      profileMin.SHA256,
						})
					}
					node := reportingapi.Node{
						Id:          item.NodeID,
						Name:        item.NodeName,
						Environment: item.Environment,
						Profiles:    profiles,
					}

					var platform reportingapi.Platform
					platform.Name = item.Platform.Name
					platform.Release = item.Platform.Release
					node.Platform = &platform
					var latestReport reportingapi.LatestReportSummary
					latestReport.Id = item.ReportID
					latestReport.Status = item.Status
					latestReport.Controls = convertToRSControlSummary(item.ControlsSums)
					timestamp, err := ptypes.TimestampProto(item.EndTime)
					if err != nil {
						logrus.Errorf("getNode time error: %s", err.Error())
					} else {
						latestReport.EndTime = timestamp
					}
					node.LatestReport = &latestReport

					return &node, nil
				}
				logrus.Errorf("getNode unmarshal error: %s", err.Error())
			}
		}
	}
	return nil, utils.ProcessNotFound(nil, nodeUuid)
}
