package relaxting

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"
	"time"

	"google.golang.org/protobuf/types/known/timestamppb"

	"github.com/golang/protobuf/ptypes"
	elastic "github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/lib/errorutils"
	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/stringutils"
)

const MaxScrollRecordSize = 10000
const layout = "2006-01-02T15:04:05Z"
const impact = "profiles.controls.impact"
const lower = "profiles.controls.title.lower"
const profile = "profiles.title.lower"

func (backend ES2Backend) getDocIdHits(esIndex string,
	searchSource *elastic.SearchSource) ([]*elastic.SearchHit, time.Duration, error) {
	defer util.TimeTrack(time.Now(), "getDocIdHits")
	var startT = time.Now()

	hits := make([]*elastic.SearchHit, 0)

	client, err := backend.ES2Client()
	if err != nil {
		return hits, time.Since(startT), errors.Wrap(err, "getDocIdsHits cannot connect to elasticsearch")
	}

	source, err := searchSource.Source()
	if err != nil {
		return hits, time.Since(startT), errors.Wrap(err, "getDocIdHits unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getDocIdHits query searchSource")

	scroll := client.Scroll().
		Index(esIndex).
		SearchSource(searchSource)

	for {
		results, err := scroll.Do(context.Background())
		//LogQueryPartMin(results, "getDocIdHits query results")
		if err == io.EOF {
			return hits, time.Since(startT), nil // all results retrieved
		}
		if err != nil {
			return hits, time.Since(startT), errors.Wrap(err, "getDocIdHits unable to get results")
		}
		if results.TotalHits() > 0 && len(results.Hits.Hits) > 0 {
			for _, hit := range results.Hits.Hits {
				hits = append(hits, hit)
			}
		}
	}
}

func (backend ES2Backend) getNodeReportIdsFromTimeseries(esIndex string,
	filters map[string][]string,
	latestOnly bool) (*reportingapi.ReportIds, error) {
	myName := "getNodeReportIdsFromTimeseries"
	var repIds reportingapi.ReportIds

	//nodeReport := make(map[string]reportingapi.ReportData, 0)
	boolQuery := backend.getFiltersQuery(filters, latestOnly)

	fsc := elastic.NewFetchSourceContext(true).Include("end_time")

	aggs := elastic.NewTermsAggregation().Field("node_uuid").Size(reporting.ESize).
		SubAggregation("distinct", elastic.NewTopHitsAggregation().Size(1).
			FetchSourceContext(fsc).
			Sort("end_time", false))

	client, err := backend.ES2Client()
	if err != nil {
		return &repIds, errors.Wrap(err, "getNodeReportIdsFromTimeseries cannot connect to elasticsearch")
	}

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("nodes", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return &repIds, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getNodeReportIdsFromTimeseries query searchSource")

	searchResult, err := client.Search().
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.nodes.buckets.key",
			"aggregations.nodes.buckets.distinct.hits.hits._id",
			"aggregations.nodes.buckets.distinct.hits.hits._source",
		).
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		logrus.Errorf("unable to getNodeReportIdsFromTimeseries %v", err)
		return &repIds, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to complete search")
	}

	if searchResult.TotalHits() == 0 {
		logrus.Debugf("getNodeReportIdsFromTimeseries: No report ids for the given filters: %+v\n", filters)
		// no matching report IDs is not an error, just return an empty array
		return &repIds, nil
	}
	LogQueryPartMin(esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	outermostAgg, _ := searchResult.Aggregations.Terms("nodes")
	if outermostAgg != nil {
		for _, nodeBucket := range outermostAgg.Buckets {
			topHits, _ := nodeBucket.Aggregations.TopHits("distinct")
			//nodeID := fmt.Sprintf("%s", nodeBucket.Key)
			for _, hit := range topHits.Hits.Hits {
				var et EndTimeSource
				if hit.Source != nil {
					err = json.Unmarshal(hit.Source, &et)
					if err != nil {
						logrus.Errorf("could not get the end_time for runid: %s %v", hit.Id, err)
						continue
					}
				}
				endTimeTimestamp, err := ptypes.TimestampProto(et.EndTime)
				if err != nil {
					logrus.Errorf("error converting end_time for runid: %s %v", hit.Id, err)
				}

				repData := reportingapi.ReportData{}
				repData.Id = hit.Id
				repData.EndTime = endTimeTimestamp

				repIds.Ids = append(repIds.Ids, repData.Id)
				repIds.ReportData = append(repIds.ReportData, &repData)
				//nodeReport[nodeID] = repData
			}
		}
	}
	/*reportIds := make([]string, len(nodeReport))
	reportData := make([]*reportingapi.ReportData, len(nodeReport))
	i := 0
	for _, v := range nodeReport {
		a := v
		reportData[i] = &a
		reportIds[i] = a.Id
		i++
	}
	repIds.Ids = reportIds
	repIds.ReportData = reportData*/

	logrus.Debugf("getNodeReportIdsFromTimeseries returning %d report ids in %d milliseconds\n",
		len(repIds.Ids), searchResult.TookInMillis)

	return &repIds, nil
}

func (backend ES2Backend) GetReportIds(esIndex string, filters map[string][]string) (*reportingapi.ReportIds, error) {
	reportIds, err := backend.getNodeReportIdsFromTimeseries(esIndex, filters, true)
	if err != nil {
		return nil, errors.Wrap(err, "GetReportIds unable to get node report ids")
	}

	return reportIds, nil
}

func (backend ES2Backend) filterIdsByControl(esIndex string, ids, controls []string) ([]string, error) {
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(ids...)
	termsQuery := elastic.NewTermsQuery("profiles.controls.id", stringArrayToInterfaceArray(controls)...)
	reportIdsAndControlIdQuery := elastic.NewBoolQuery()

	reportIdsNestedQuery := elastic.NewNestedQuery("profiles.controls", reportIdsAndControlIdQuery.Must(idsQuery,
		termsQuery))

	fsc := elastic.NewFetchSourceContext(false).
		Include("took", "hits.total", "hits.hits._id")

	//we use scrolling to accomplish sizes greater than 10000 we set it to 10k here which gets us 10k/scroll.
	searchSource := elastic.NewSearchSource().
		Query(reportIdsNestedQuery).
		FetchSourceContext(fsc).
		Size(MaxScrollRecordSize)

	source, err := searchSource.Source()
	if err != nil {
		return ids, errors.Wrap(err, "filterIdsByControl unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "filterIdsByControl Control filter query searchSource")

	logrus.Debugf("esIndex: %s", esIndex)
	hits, _, err := backend.getDocIdHits(esIndex, searchSource)
	if err != nil {
		return ids, errors.Wrap(err, "filterIdsByControl unable to get doc ids")
	}

	ids = make([]string, 0)

	// building up the new ids array from the controls filter query
	if len(hits) > 0 {
		for _, hit := range hits {
			ids = append(ids, hit.Id)
		}
	}
	return ids, nil
}

// GetReports returns all reports in a given timeframe
// TODO: support timeframe and pagination
func (backend *ES2Backend) GetReports(from int32, size int32, filters map[string][]string,
	sortField string, sortAsc bool) ([]*reportingapi.ReportSummaryLevelOne, int64, error) {
	myName := "GetReports"

	depth, err := backend.NewDepth(filters, false)
	if err != nil {
		return nil, 0, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	logrus.Debugf("%s will retrieve all reports in a given timeframe with filters %+v", myName, filters)

	client, err := backend.ES2Client()

	if err != nil {
		return nil, 0, errors.Wrapf(err, "%s, cannot connect to Elasticsearch", myName)
	}

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"environment",
		"end_time",
		"ipaddress")

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

	source, err := searchSource.Source()
	if err != nil {
		return nil, 0, errors.Wrapf(err, "%s unable to get Source", myName)
	}
	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query searchSource", myName))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(context.Background())

	if err != nil {
		return nil, 0, errors.Wrapf(err, "%s unable to complete search", myName)
	}
	logrus.Debugf("GetReports got %d reports in %d milliseconds\n", searchResult.TotalHits(),
		searchResult.TookInMillis)
	reports := make([]*reportingapi.ReportSummaryLevelOne, 0)
	if searchResult.TotalHits() > 0 {
		for _, hit := range searchResult.Hits.Hits {
			item := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(hit.Source, &item)
				if err == nil {
					t := item.EndTime.Round(1 * time.Second)
					timestamp, _ := ptypes.TimestampProto(t)
					report := reportingapi.ReportSummaryLevelOne{
						Id:       hit.Id,
						NodeId:   item.NodeID,
						NodeName: item.NodeName,
						EndTime:  timestamp,
					}

					if item.IPAddress != nil {
						report.Ipaddress = *item.IPAddress
					}

					var controlSummary reporting.NodeControlSummary
					var status string

					if queryInfo.level == ReportLevel {
						controlSummary = item.ControlsSums
						status = item.Status
					} else if queryInfo.level == ProfileLevel || queryInfo.level == ControlLevel {
						controlSummary, status, err = getDeepControlsSums(hit, queryInfo)
						if err != nil {
							//todo - handle this
							logrus.Errorf("%s time error: %s", myName, err.Error())
						}
					}
					report.Controls = convertToRSControlSummary(controlSummary)
					report.Status = status

					reports = append(reports, &report)
				} else {
					logrus.Errorf("GetReports unmarshal error: %s", err.Error())
				}
			}
		}
		return reports, searchResult.TotalHits(), nil
	}

	logrus.Debug("Found no reports\n")
	return reports, 0, nil
}

// GetReport returns the information about a single report
func (backend *ES2Backend) GetReport(reportId string, filters map[string][]string) (*reportingapi.Report, error) {
	var report *reportingapi.Report
	var method = "GetReport"
	searchResult, queryInfo, err := backend.getSearchResult(reportId, filters, method)
	if err != nil {
		return report, err
	}
	// we should only receive one value
	if searchResult.TotalHits() > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(hit.Source, &esInSpecReport)
				if err == nil {
					//TODO: FIX Unmarshal error(json: cannot unmarshal array into Go struct field
					// ESInSpecReportControl.results) and move the read all profiles section here
				} else {
					logrus.Errorf("GetReport unmarshal error: %s", err.Error())
				}

				var esInspecProfiles []ESInSpecReportProfile //`json:"profiles"`
				var status string

				if queryInfo.level == ReportLevel {
					esInspecProfiles = esInSpecReport.Profiles
					status = esInSpecReport.Status
				} else if queryInfo.level == ProfileLevel || queryInfo.level == ControlLevel {
					esInspecProfiles, status, err = getDeepInspecProfiles(hit, queryInfo)
					if err != nil {
						//todo - handle this
						logrus.Errorf("GetReport time error: %s", err.Error())
					}
				}
				esInSpecReport.Status = status

				// read all profiles
				profiles := make([]*reportingapi.Profile, 0)
				for _, esInSpecReportProfileMin := range esInspecProfiles {
					logrus.Debugf("Determine profile: %s", esInSpecReportProfileMin.Name)
					esInSpecProfile, err := backend.GetProfile(esInSpecReportProfileMin.SHA256)
					if err != nil {
						logrus.Errorf("GetReport - Could not get profile '%s' error: %s", esInSpecReportProfileMin.SHA256, err.Error())
						logrus.Debug("GetReport - Making the most from the profile information in esInSpecReportProfileMin")
						esInSpecProfile.Name = esInSpecReportProfileMin.Name
						esInSpecProfile.Title = esInSpecReportProfileMin.Title
						esInSpecProfile.Version = esInSpecReportProfileMin.Version
						esInSpecProfile.Sha256 = esInSpecReportProfileMin.SHA256
					}

					reportProfile := inspec.Profile{}
					reportProfile.Name = esInSpecProfile.Name
					reportProfile.CopyrightEmail = esInSpecProfile.CopyrightEmail
					reportProfile.Copyright = esInSpecProfile.Copyright
					reportProfile.Title = esInSpecProfile.Title
					reportProfile.Summary = esInSpecProfile.Summary
					reportProfile.Version = esInSpecProfile.Version
					reportProfile.Sha256 = esInSpecProfile.Sha256
					reportProfile.Status = esInSpecProfile.Status
					reportProfile.Status = esInSpecReportProfileMin.Status
					reportProfile.Full = esInSpecReportProfileMin.Full

					if esInSpecReportProfileMin.StatusMessage != "" {
						reportProfile.StatusMessage = esInSpecReportProfileMin.StatusMessage
					} else {
						// Legacy message only available for the skipped status
						reportProfile.StatusMessage = esInSpecReportProfileMin.SkipMessage
					}

					dependsHash := make(map[string]*ESInSpecReportDepends, len(esInSpecReportProfileMin.Depends))
					for _, esInSpecProfileDependency := range esInSpecReportProfileMin.Depends {

						depCopy := esInSpecProfileDependency // to prevent https://github.com/golang/go/issues/20725
						dependsHash[esInSpecProfileDependency.Name] = &depCopy
					}
					// Picking the report specific dependency info(status, skip_message) and adding it to the static
					// info retrieved from comp-profiles
					for _, esInSpecProfileDep := range esInSpecProfile.Depends {
						if hash, ok := dependsHash[esInSpecProfileDep.Name]; ok {
							esInSpecProfileDep.Status = hash.Status
							esInSpecProfileDep.SkipMessage = hash.SkipMessage
						}
					}
					reportProfile.Dependencies = esInSpecProfile.Depends

					reportProfile.Controls = make([]inspec.Control, len(esInSpecReportProfileMin.Controls))
					// Creating a map of report control ids to avoid a n^2 complexity later on when we look for the
					// matching profile control id
					profileControlsMap := make(map[string]*reportingapi.Control, len(esInSpecProfile.Controls))
					for _, control := range esInSpecProfile.Controls {
						profileControlsMap[control.Id] = control
					}

					convertedControls := make([]*reportingapi.Control, 0)
					// Enrich min report controls with profile metadata
					for _, reportControlMin := range esInSpecReportProfileMin.Controls {
						if controlFromMap, ok := profileControlsMap[reportControlMin.ID]; ok {
							reportControlMin.Tags = controlFromMap.Tags
							// store controls to returned report
							convertedControl := convertControl(profileControlsMap, reportControlMin, filters)
							if convertedControl != nil {
								convertedControls = append(convertedControls, convertedControl)
							}
						} else {
							logrus.Warnf("GetReport: %s was not found in the profile control map",
								reportControlMin.ID)
						}
					}

					if len(convertedControls) > 1 {
						// Sort convertedControls by Id
						sort.Slice(convertedControls, func(i, j int) bool {
							return convertedControls[i].Id < convertedControls[j].Id
						})
					}

					// TODO: fix this (vj)
					// Name: control.Attribute
					convertedAttributes := []*reportingapi.Attribute{}

					convertedProfile := reportingapi.Profile{
						Name:           reportProfile.Name,
						Full:           reportProfile.Full,
						Title:          reportProfile.Title,
						Maintainer:     reportProfile.Maintainer,
						Copyright:      reportProfile.Copyright,
						CopyrightEmail: reportProfile.CopyrightEmail,
						License:        reportProfile.License,
						Summary:        reportProfile.Summary,
						Version:        reportProfile.Version,
						Supports:       convertMapSupportsToRSSupports(reportProfile.Supports),
						Depends:        reportProfile.Dependencies,
						Sha256:         reportProfile.Sha256,
						Groups:         reportProfile.Groups,
						Controls:       convertedControls,
						Attributes:     convertedAttributes,
						Status:         reportProfile.Status,
						StatusMessage:  reportProfile.StatusMessage,
					}
					profiles = append(profiles, &convertedProfile)
				}
				ipAddress := ""
				if esInSpecReport.IPAddress != nil {
					ipAddress = *esInSpecReport.IPAddress
				}
				timestamp, _ := ptypes.TimestampProto(esInSpecReport.EndTime)
				report = &reportingapi.Report{
					Id:               hit.Id,
					NodeId:           esInSpecReport.NodeID,
					NodeName:         esInSpecReport.NodeName,
					Environment:      esInSpecReport.Environment,
					Status:           esInSpecReport.Status,
					StatusMessage:    esInSpecReport.StatusMessage,
					JobId:            esInSpecReport.JobID,
					EndTime:          timestamp,
					Version:          esInSpecReport.InSpecVersion,
					Profiles:         profiles,
					Ipaddress:        ipAddress,
					Fqdn:             esInSpecReport.FQDN,
					ChefServer:       esInSpecReport.SourceFQDN,
					ChefOrganization: esInSpecReport.OrganizationName,
					ChefTags:         esInSpecReport.ChefTags,
					Roles:            esInSpecReport.Roles,
					Projects:         esInSpecReport.Projects,
				}
				report.Statistics = &reportingapi.Statistics{
					Duration: esInSpecReport.Statistics.Duration,
				}
				report.Platform = &reportingapi.Platform{
					Name:    esInSpecReport.Platform.Name,
					Release: esInSpecReport.Platform.Release,
					Full:    esInSpecReport.Platform.Full,
				}
			}
		}
		return report, nil
	}

	return report, errorutils.ProcessNotFound(nil, reportId)
}

// GetNodeInfoFromReportID returns report header information of a single report
func (backend *ES2Backend) GetNodeInfoFromReportID(reportId string, filters map[string][]string) (*reportingapi.NodeHeaderInfo, error) {
	var report *reportingapi.NodeHeaderInfo
	depth, err := backend.NewDepth(filters, false)
	if err != nil {
		return report, errors.Wrap(err, "GetNodeInfoFromReportID unable to get depth level for report")
	}
	queryInfo := depth.getQueryInfo()

	//normally, we compute the boolQuery when we create a new Depth obj.. here we, instead call a variation of the full
	// query builder. we need to do this because this variation of filter query provides this func with deeper
	// information about the report being retrieved.
	queryInfo.filtQuery = getFiltersQueryForDeepReport(reportId, filters)

	logrus.Debugf("GetNodeInfoFromReportID will retrieve report %s based on filters %+v", reportId, filters)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"environment",
		"roles",
		"platform",
		"profiles",
		"end_time",
		"status",
		"status_message",
		"version")

	if queryInfo.level != ReportLevel {
		fsc.Exclude("profiles")
	}
	logrus.Debugf("GetNodeInfoFromReportID for reportid=%s, filters=%+v", reportId, filters)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(queryInfo.filtQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return report, errors.Wrap(err, "GetNodeInfoFromReportID unable to get Source")
	}
	LogQueryPartMin(queryInfo.esIndex, source, "GetNodeInfoFromReportID query searchSource")

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(context.Background())

	if err != nil {
		return report, errors.Wrap(err, "GetNodeInfoFromReportID unable to complete search")
	}

	logrus.Debugf("GetNodeInfoFromReportID got %d reports in %d milliseconds\n", searchResult.TotalHits(),
		searchResult.TookInMillis)

	// we should only receive one value
	report, err = populateNodeReport(searchResult, backend, queryInfo)
	if err != nil {
		return report, errorutils.ProcessNotFound(nil, reportId)
	}
	return report, nil
}

// populateNodeReport generates the report from the search result of report id
func populateNodeReport(searchResult *elastic.SearchResult, backend *ES2Backend, queryInfo *QueryInfo) (*reportingapi.NodeHeaderInfo, error) {
	var report *reportingapi.NodeHeaderInfo
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits.Value > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			if hit.Source == nil {
				logrus.Debugf("no source found for search hit %s", hit.Id)
				continue
			}
			err := json.Unmarshal(hit.Source, &esInSpecReport)
			if err != nil {
				logrus.Errorf("GetNodeInfoFromReportID unmarshal error: %s", err.Error())
				return report, errors.New("cannot unmarshal the search hits")
			}
			var esInspecProfiles []ESInSpecReportProfile //`json:"profiles"`
			var status string

			switch queryInfo.level {
			case ReportLevel:
				esInspecProfiles = esInSpecReport.Profiles
				status = esInSpecReport.Status
			case ProfileLevel, ControlLevel:
				esInspecProfiles, status, err = getDeepInspecProfiles(hit, queryInfo)
				if err != nil {
					logrus.Errorf("GetNodeInfoFromReportID time error: %s", err.Error())
				}
			}
			esInSpecReport.Status = status
			profiles := make([]*reportingapi.NodeHeaderProfileInfo, 0)
			for _, esInSpecReportProfileMin := range esInspecProfiles {
				logrus.Debugf("Determine profile: %s", esInSpecReportProfileMin.Name)
				esInSpecProfile, err := backend.GetProfile(esInSpecReportProfileMin.SHA256)
				if err != nil {
					logrus.Errorf("GetNodeInfoFromReportID - Could not get profile '%s' error: %s", esInSpecReportProfileMin.SHA256, err.Error())
					logrus.Debug("GetNodeInfoFromReportID - Making the most from the profile information in esInSpecReportProfileMin")
					esInSpecProfile.Name = esInSpecReportProfileMin.Name
				}

				reportProfile := inspec.Profile{}
				reportProfile.Name = esInSpecProfile.Name
				reportProfile.Status = esInSpecReportProfileMin.Status

				if esInSpecReportProfileMin.StatusMessage != "" {
					reportProfile.StatusMessage = esInSpecReportProfileMin.StatusMessage
				} else {
					// Legacy message only available for the skipped status
					reportProfile.StatusMessage = esInSpecReportProfileMin.SkipMessage
				}

				convertedProfile := reportingapi.NodeHeaderProfileInfo{
					Name:          reportProfile.Name,
					Status:        reportProfile.Status,
					StatusMessage: reportProfile.StatusMessage,
				}
				profiles = append(profiles, &convertedProfile)
			}
			timestamp, _ := ptypes.TimestampProto(esInSpecReport.EndTime)

			report = &reportingapi.NodeHeaderInfo{
				NodeId:        esInSpecReport.NodeID,
				NodeName:      esInSpecReport.NodeName,
				Environment:   esInSpecReport.Environment,
				Status:        esInSpecReport.Status,
				StatusMessage: esInSpecReport.StatusMessage,
				EndTime:       timestamp,
				Version:       esInSpecReport.InSpecVersion,
				Profiles:      profiles,
				Roles:         esInSpecReport.Roles,
			}

			report.Platform = &reportingapi.Platform{
				Name:    esInSpecReport.Platform.Name,
				Release: esInSpecReport.Platform.Release,
				Full:    esInSpecReport.Platform.Full,
			}
			report.EndTime = timestamp
		}
		return report, nil
	}
	return report, errors.New("No process found")
}

func convertControl(profileControlsMap map[string]*reportingapi.Control, reportControlMin ESInSpecReportControl, filters map[string][]string) *reportingapi.Control {
	profileControl := profileControlsMap[reportControlMin.ID]

	if profileControl == nil {
		return nil
	}

	if len(filters["control"]) > 0 {
		if !stringutils.SliceContains(filters["control"], profileControl.Id) {
			return nil
		}
	}

	profileControl.Results = make([]*reportingapi.Result, 0)
	if reportControlMin.ID != profileControl.Id {
		return nil
	}

	minResults := make([]*reportingapi.Result, len(reportControlMin.Results))
	for i, result := range reportControlMin.Results {
		minResults[i] = &reportingapi.Result{
			Status:      result.Status,
			CodeDesc:    result.CodeDesc,
			RunTime:     result.RunTime,
			Message:     result.Message,
			SkipMessage: result.SkipMessage,
		}
	}

	convertedControl := reportingapi.Control{
		Id:             profileControl.Id,
		Code:           profileControl.Code,
		Desc:           profileControl.Desc,
		Impact:         profileControl.Impact,
		Title:          profileControl.Title,
		SourceLocation: profileControl.SourceLocation,
		Tags:           profileControl.Tags,
		Results:        minResults,
		WaivedStr:      reportControlMin.WaivedStr,
	}
	if reportControlMin.RemovedResultsCounts != nil {
		convertedControl.RemovedResultsCounts = &reportingapi.RemovedResultsCounts{
			Failed:  int32(reportControlMin.RemovedResultsCounts.Failed),
			Skipped: int32(reportControlMin.RemovedResultsCounts.Skipped),
			Passed:  int32(reportControlMin.RemovedResultsCounts.Passed),
		}
	}
	if reportControlMin.WaiverData != nil {
		convertedControl.WaiverData = &reportingapi.OrigWaiverData{
			ExpirationDate:     reportControlMin.WaiverData.ExpirationDate,
			Justification:      reportControlMin.WaiverData.Justification,
			Run:                reportControlMin.WaiverData.Run,
			SkippedDueToWaiver: reportControlMin.WaiverData.SkippedDueToWaiver,
			Message:            reportControlMin.WaiverData.Message,
		}
	}

	jsonTags := make(map[string]*reportingapi.TagValues, 0)
	for _, tag := range reportControlMin.StringTags {
		if len(tag.Values) == 0 {
			jsonTags[tag.Key] = &reportingapi.TagValues{Values: []string{"null"}}
		} else {
			vals := make([]string, 0)
			for _, val := range tag.Values {
				vals = append(vals, val)
				jsonTags[tag.Key] = &reportingapi.TagValues{Values: vals}
			}
		}
	}

	if !doesControlTagMatchFilter(filters, jsonTags) {
		return nil
	}

	convertedControl.StringTags = jsonTags

	convertedControl.Refs = make([]*reportingapi.Ref, len(reportControlMin.Refs))
	for i, ref := range reportControlMin.Refs {
		convertedControl.Refs[i] = &reportingapi.Ref{
			Ref: ref.Ref,
			Url: ref.Url,
		}
	}
	convertedControl.Tags = reportControlMin.Tags
	return &convertedControl
}

func doesControlTagMatchFilter(filters map[string][]string,
	jsonTags map[string]*reportingapi.TagValues) bool {
	matchingTagRequired := false
	for filterKey, filterVals := range filters {
		if strings.HasPrefix(filterKey, "control_tag") {
			matchingTagRequired = true
			trimmed := strings.TrimPrefix(filterKey, "control_tag:")
			if tagVal, ok := jsonTags[trimmed]; ok {
				for _, val := range filterVals {
					if contains(tagVal.Values, val) || nullArrMatchEmptyString(tagVal.Values, val) {
						return true
					}
				}
			}

		}
	}
	if matchingTagRequired {
		return false
	}
	return true
}

func nullArrMatchEmptyString(vals []string, val string) bool {
	if len(vals) == 1 && vals[0] == "null" {
		if val == "" {
			return true
		}
	}
	return false
}

func contains(a []string, x string) bool {
	for _, n := range a {
		if strings.HasSuffix(x, n) {
			return true
		}
	}
	return false
}

func (backend *ES2Backend) GetControlListItems(ctx context.Context, filters map[string][]string,
	size int32, pageNumber int32) (*reportingapi.ControlItems, error) {
	myName := "GetControlListItems"

	contListItems := make([]*reportingapi.ControlItem, 0)

	controlSummaryTotals := &reportingapi.ControlSummary{
		Passed:  &reportingapi.Total{},
		Skipped: &reportingapi.Total{},
		Failed:  &reportingapi.Failed{},
		Waived:  &reportingapi.Total{},
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return nil, err
	}

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	esIndex, err := GetEsIndex(filters, false)
	if err != nil {
		return nil, errors.Wrap(err, myName)
	}

	//here, we set latestOnly to true.  We may need to set it to false if we want to search non lastest reports
	//for now, we don't search non-latest reports so don't do it.. it's slower for obvious reasons.
	latestOnly := FetchLatestDataOrNot(filters)

	filtQuery := backend.getFiltersQuery(filters, latestOnly)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		FetchSource(false).
		Size(1)

	controlTermsAgg := elastic.NewTermsAggregation().Field("profiles.controls.id").
		Size(int(size*pageNumber)).
		Order("_key", true)

	controlTermsAgg.SubAggregation("impact",
		elastic.NewTermsAggregation().Field(impact).Size(1))

	controlTermsAgg.SubAggregation("title",
		elastic.NewTermsAggregation().Field("profiles.controls.title").Size(1))

	waivedQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes", "yes_run")
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "passed")).
		MustNot(waivedQuery))

	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "failed")).
		MustNot(waivedQuery))

	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "skipped")).
		MustNot(waivedQuery))

	waivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	controlTermsAgg.SubAggregation("skipped", skippedFilter)
	controlTermsAgg.SubAggregation("failed", failedFilter)
	controlTermsAgg.SubAggregation("passed", passedFilter)
	controlTermsAgg.SubAggregation("waived", waivedFilter)

	waivedStrAgg := elastic.NewTermsAggregation().Field("profiles.controls.waived_str").Size(4) //there are 4 different waived_str states
	waivedButNotRunQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes")
	waivedAndRunQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes_run")
	waiverDataPassedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "passed")).
		Must(waivedAndRunQuery))

	waiverDataFailedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "failed")).
		Must(waivedAndRunQuery))

	waiverDataSkippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Should(elastic.NewTermQuery("profiles.controls.status", "skipped")).
		Should(waivedButNotRunQuery))

	waiverDataWaivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	waiverDataJustificationAgg := elastic.NewTermsAggregation().Field("profiles.controls.waiver_data.justification").Size(reporting.ESize)
	waiverDataJustificationAgg.SubAggregation("skipped", waiverDataSkippedFilter)
	waiverDataJustificationAgg.SubAggregation("failed", waiverDataFailedFilter)
	waiverDataJustificationAgg.SubAggregation("passed", waiverDataPassedFilter)
	waiverDataJustificationAgg.SubAggregation("waived", waiverDataWaivedFilter)

	waiverDataExpirationDateAgg := elastic.NewTermsAggregation().Field("profiles.controls.waiver_data.expiration_date").Size(reporting.ESize)
	waiverDataExpirationDateAgg.SubAggregation("justification", waiverDataJustificationAgg)
	waivedStrAgg.SubAggregation("expiration_date", waiverDataExpirationDateAgg)

	waivedStrFilter := elastic.NewFilterAggregation().Filter(waivedQuery)
	waivedStrFilter.SubAggregation("waived_str", waivedStrAgg)

	controlTermsAgg.SubAggregation("filtered_waived_str", waivedStrFilter)

	controlTermsAgg.SubAggregation("end_time", elastic.NewReverseNestedAggregation().SubAggregation("most_recent_report",
		elastic.NewTermsAggregation().Field("end_time").Size(1)))

	profileInfoAgg := elastic.NewReverseNestedAggregation().Path("profiles").SubAggregation("version",
		elastic.NewTermsAggregation().Field("profiles.name"))

	profileInfoAgg.SubAggregation("sha",
		elastic.NewTermsAggregation().Field("profiles.sha256"))

	profileInfoAgg.SubAggregation("title",
		elastic.NewTermsAggregation().Field("profiles.title"))

	profileInfoAgg.SubAggregation("version",
		elastic.NewTermsAggregation().Field("profiles.version"))

	controlTermsAgg.SubAggregation("profile", profileInfoAgg)

	controlsQuery := &elastic.BoolQuery{}
	// Going through all filters to find the ones prefixed with 'control_tag', e.g. 'control_tag:nist'
	for filterType := range filters {
		if strings.HasPrefix(filterType, "control_tag:") {
			_, tagKey := leftSplit(filterType, ":")
			termQuery := newNestedTermQueryFromControlTagsFilter(tagKey, filters[filterType])
			controlsQuery = controlsQuery.Must(termQuery)
		}
	}
	if len(filters["control_status"]) > 0 {
		controlStatusQuery := newTermQueryFromFilter("profiles.controls.status", filters["control_status"])
		controlsQuery = controlsQuery.Must(controlStatusQuery)
		logrus.Infof("controls status query %v", controlStatusQuery)
	}
	if len(filters["control_name"]) > 0 {
		controlTitlesQuery := newTermQueryFromFilter(lower, filters["control_name"])
		controlsQuery = controlsQuery.Should(controlTitlesQuery)
		logrus.Infof("controls name query %v", controlTitlesQuery)
	}
	if len(filters["control"]) > 0 {
		controlIdsQuery := newTermQueryFromFilter("profiles.controls.id", filters["control"])
		controlsQuery = controlsQuery.Should(controlIdsQuery)
		logrus.Infof("controls ids query %v", controlIdsQuery)
	}

	filteredControls := elastic.NewFilterAggregation().Filter(controlsQuery)
	filteredControls.SubAggregation("control", controlTermsAgg)
	filteredControls.SubAggregation("skipped_total", skippedFilter)
	filteredControls.SubAggregation("failed_total", failedFilter)
	filteredControls.SubAggregation("passed_total", passedFilter)
	filteredControls.SubAggregation("waived_total", waivedFilter)
	controlsAgg := elastic.NewNestedAggregation().Path("profiles.controls")
	controlsAgg.SubAggregation("filtered_controls", filteredControls)

	profilesQuery := &elastic.BoolQuery{}
	if len(filters["profile_name"]) > 0 {
		profileTitlesQuery := newTermQueryFromFilter(profile, filters["profile_name"])
		profilesQuery = profilesQuery.Should(profileTitlesQuery)
		logrus.Infof("profiles name query %v", profileTitlesQuery)
	}

	if len(filters["profile_id"]) > 0 {
		profilesShaQuery := newTermQueryFromFilter("profiles.sha256", filters["profile_id"])
		profilesQuery = profilesQuery.Should(profilesShaQuery)
	}

	filteredProfiles := elastic.NewFilterAggregation().Filter(profilesQuery)
	filteredProfiles.SubAggregation("controls", controlsAgg)
	outterMostProfilesAgg := elastic.NewNestedAggregation().Path("profiles")
	outterMostProfilesAgg.SubAggregation("filtered_profiles", filteredProfiles)

	searchSource.Aggregation("profiles", outterMostProfilesAgg)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrapf(err, "%s unable to get Source ", myName)
	}
	LogQueryPartMin(esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(ctx)

	if err != nil {
		logrus.Errorf("%s search failed", myName)
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)

	if outerProfilesAggResult, found := searchResult.Aggregations.Nested("profiles"); found {
		if outerFilteredProfiles, found := outerProfilesAggResult.Aggregations.Filter("filtered_profiles"); found {
			if outerControlsAggResult, found := outerFilteredProfiles.Aggregations.Nested("controls"); found {
				if filteredControls, found := outerControlsAggResult.Aggregations.Filter("filtered_controls"); found {
					controlSummaryTotals.Total = int32(filteredControls.DocCount)
					if totalPassedControls, found := filteredControls.Aggregations.Filter("passed_total"); found {
						controlSummaryTotals.Passed.Total = int32(totalPassedControls.DocCount)
					}
					if totalFailedControls, found := filteredControls.Aggregations.Filter("failed_total"); found {
						controlSummaryTotals.Failed.Total = int32(totalFailedControls.DocCount)
					}
					if totalSkippedControls, found := filteredControls.Aggregations.Filter("skipped_total"); found {
						controlSummaryTotals.Skipped.Total = int32(totalSkippedControls.DocCount)
					}
					if totalWaivedControls, found := filteredControls.Aggregations.Filter("waived_total"); found {
						controlSummaryTotals.Waived.Total = int32(totalWaivedControls.DocCount)
					}
					if controlBuckets, found := filteredControls.Aggregations.Terms("control"); found && len(controlBuckets.Buckets) > 0 {
						start, end := paginate(int(pageNumber), int(size), len(controlBuckets.Buckets))
						for _, controlBucket := range controlBuckets.Buckets[start:end] {
							contListItem, err := backend.getControlItem(controlBucket)
							if err != nil {
								return nil, err
							}
							contListItems = append(contListItems, &contListItem)
						}
					}
				}
			}
		}
	}

	contListItemList := &reportingapi.ControlItems{ControlItems: contListItems, ControlSummaryTotals: controlSummaryTotals}
	return contListItemList, nil
}

// setControlSummaryForControlItems applying control summary to control list from the control summary map
func setControlSummaryForControlItems(contListItems []*reportingapi.ControlItem, controlSummaryMap map[string]*reportingapi.ControlSummary) {
	for _, controlItem := range contListItems {
		controlItem.ControlSummary = controlSummaryMap[controlItem.Id]
	}
}

// GetNodeControlListItems returns the paginated control list response.
func (backend *ES2Backend) GetNodeControlListItems(ctx context.Context, filters map[string][]string, reportID string) (*reportingapi.ControlElements, error) {
	controlNodeList := &reportingapi.ControlElements{}

	depth, err := backend.NewDepth(filters, false)
	if err != nil {
		return controlNodeList, errors.Wrap(err, "GetNodeControlListItems unable to get depth level for report")
	}

	queryInfo := depth.getQueryInfo()

	// create query for fetching paginated response with the passed filters
	queryInfo.filtQuery, err = backend.getFilterQueryWithPagination(reportID, filters)
	if err != nil {
		return controlNodeList, nil
	}

	logrus.Debugf("GetNodeControlListItems will retrieve control items %s based on filters %+v", reportID, filters)

	fsc := elastic.NewFetchSourceContext(true)

	logrus.Debugf("GetNodeControlListItems for reportid=%s, filters=%+v", reportID, filters)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(queryInfo.filtQuery).
		Size(1)

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(ctx)
	if err != nil {
		return controlNodeList, err
	}

	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits.Value > 0 {
		numProfiles := make(map[int]struct {
			Name   string
			Sha256 string
		}, 0)
		// Extract the profile name for adding the root profile name to the return response.
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			err = json.Unmarshal(hit.Source, &esInSpecReport)
			if err != nil {
				logrus.Errorf("error unmarshalling the search response: %+v", err)
				return nil, err
			}
			for index, profile := range esInSpecReport.Profiles {
				numProfiles[index] = struct {
					Name   string
					Sha256 string
				}{
					Name:   profile.Name,
					Sha256: profile.SHA256,
				}
			}
		}

		var listControls = make([]*reportingapi.ControlElement, 0)
		for _, hit := range searchResult.Hits.Hits {
			// this code is executed if filter contains profile or controls.
			if profileHit, ok := hit.InnerHits["profiles"]; ok {
				for _, innerhit := range profileHit.Hits.Hits {
					listControl, err := populateControlElements(innerhit, numProfiles)
					if err != nil {
						logrus.Errorf("error unmarshalling the search control response: %+v", err)
						return nil, err
					}
					listControls = append(listControls, listControl...)
				}
			}
			// this code is executed if there are no profile/control filters.
			if _, ok := hit.InnerHits["profiles.controls"]; ok {
				listControls, err = populateControlElements(hit, numProfiles)
				if err != nil {
					logrus.Errorf("error unmarshalling the search control response: %+v", err)
					return nil, err
				}
			}
		}
		contNodeList := &reportingapi.ControlElements{ControlElements: listControls}
		return contNodeList, nil
	}
	return nil, errorutils.ProcessNotFound(nil, reportID)
}

// getControlItem returns one control from the control list results
func (backend *ES2Backend) getControlItem(controlBucket *elastic.AggregationBucketKeyItem) (reportingapi.ControlItem, error) {
	contListItem := reportingapi.ControlItem{}
	id, ok := controlBucket.Key.(string)
	if !ok {
		logrus.Errorf("could not convert the value of controlBucket: %v, to a string!", controlBucket)
	}
	contListItem.Id = id
	controlSummary := &reportingapi.ControlSummary{
		Passed:  &reportingapi.Total{},
		Skipped: &reportingapi.Total{},
		Failed:  &reportingapi.Failed{},
		Waived:  &reportingapi.Total{},
	}
	if aggResult, found := controlBucket.Aggregations.Terms("title"); found {
		//there can only be one
		bucket := aggResult.Buckets[0]
		title, ok := bucket.Key.(string)
		if !ok {
			logrus.Errorf("could not convert the value of title: %v, to a string!", bucket)
		}
		contListItem.Title = title
	}
	if impactAggResult, found := controlBucket.Aggregations.Terms("impact"); found {
		//there can only be one
		impactBucket := impactAggResult.Buckets[0]
		impactAsNumber, ok := impactBucket.Key.(float64)
		if !ok {
			logrus.Errorf("could not convert the value of impact: %v, to a float!", impactBucket)
		}
		contListItem.Impact = float32(impactAsNumber)
		controlSummary.Total = int32(impactBucket.DocCount)
	}
	profileMin := &reportingapi.ProfileMin{}
	if profileResult, found := controlBucket.Aggregations.ReverseNested("profile"); found {
		if result, found := profileResult.Terms("sha"); found && len(result.Buckets) > 0 {
			sha := result.Buckets[0]
			profileMin.Id = sha.Key.(string)
		}
		if result, found := profileResult.Terms("title"); found && len(result.Buckets) > 0 {
			title := result.Buckets[0]
			profileMin.Title = title.Key.(string)
		}
		if result, found := profileResult.Terms("version"); found && len(result.Buckets) > 0 {
			version := result.Buckets[0]
			profileMin.Version = version.Key.(string)
		}
	}
	contListItem.Profile = profileMin

	if endTimeResult, found := controlBucket.Aggregations.ReverseNested("end_time"); found {
		if result, found := endTimeResult.Terms("most_recent_report"); found && len(result.Buckets) > 0 {
			endTime := result.Buckets[0]
			name := endTime.KeyAsString

			endTimeAsTime, err := time.Parse(time.RFC3339, *name)
			if err != nil {
				return reportingapi.ControlItem{}, errors.Wrapf(err, "%s time error : ", *name)
			}

			timestamp, err := ptypes.TimestampProto(endTimeAsTime)
			if err != nil {
				return reportingapi.ControlItem{}, errors.Wrapf(err, "%s time error : ", *name)
			} else {
				contListItem.EndTime = timestamp
			}
		}
	}
	if waived, found := controlBucket.Aggregations.Filter("waived"); found {
		controlSummary.Waived.Total = int32(waived.DocCount)
	}
	if passed, found := controlBucket.Aggregations.Filter("passed"); found {
		controlSummary.Passed.Total = int32(passed.DocCount)
	}
	if skipped, found := controlBucket.Aggregations.Filter("skipped"); found {
		controlSummary.Skipped.Total = int32(skipped.DocCount)
	}
	if failed, found := controlBucket.Aggregations.Filter("failed"); found {
		total := int32(failed.DocCount)
		controlSummary.Failed.Total = total
		if contListItem.Impact < 0.4 {
			controlSummary.Failed.Minor = total
		} else if contListItem.Impact < 0.7 {
			controlSummary.Failed.Major = total
		} else {
			controlSummary.Failed.Critical = total
		}
	}

	if filteredWaivedStr, found := controlBucket.Aggregations.Filter("filtered_waived_str"); found {
		if waivedStrBuckets, found := filteredWaivedStr.Aggregations.Terms("waived_str"); found && len(waivedStrBuckets.Buckets) > 0 {
			contListItem.Waivers, err = backend.getWaiverData(waivedStrBuckets)
			if err != nil {
				return contListItem, err
			}
		}
	}

	contListItem.ControlSummary = controlSummary
	return contListItem, nil

}

//getProfileMinForControlItem is parsing the  control title form the result of control list from comp-7-r-*
func getTitleForControlItem(controlBucket *elastic.AggregationBucketKeyItem) string {
	var title string
	var ok bool
	if aggResult, found := controlBucket.Aggregations.Terms("title"); found {
		//there can only be one
		bucket := aggResult.Buckets[0]
		title, ok = bucket.Key.(string)
		if !ok {
			logrus.Errorf("could not convert the value of title: %v, to a string!", bucket)
		}
	}

	return title

}

//getEndTimeForControlItem is parsing the end time form the result of control list from comp-7-r-*
func getEndTimeForControlItem(controlBucket *elastic.AggregationBucketKeyItem) (*timestamppb.Timestamp, error) {
	var timestamp *timestamppb.Timestamp

	if endTimeResult, found := controlBucket.Aggregations.ReverseNested("end_time"); found {
		if result, found := endTimeResult.Terms("most_recent_report"); found && len(result.Buckets) > 0 {
			endTime := result.Buckets[0]
			name := endTime.KeyAsString
			endTimeAsTime, err := time.Parse(time.RFC3339, *name)
			if err != nil {
				return nil, errors.Wrapf(err, "%s time error: ", *name)
			}
			timestamp, err = ptypes.TimestampProto(endTimeAsTime)
			if err != nil {
				return nil, errors.Wrapf(err, "%s time error: ", *name)
			}
		}
	}
	return timestamp, nil
}

//getProfileMinForControlItem is parsing the profile id,title and version form the result of control list from comp-7-r-*
func getProfileMinForControlItem(controlBucket *elastic.AggregationBucketKeyItem, profileMin *reportingapi.ProfileMin) {
	if profileResult, found := controlBucket.Aggregations.ReverseNested("profile"); found {
		if result, found := profileResult.Terms("sha"); found && len(result.Buckets) > 0 {
			sha := result.Buckets[0]
			profileMin.Id = sha.Key.(string)
		}
		if result, found := profileResult.Terms("title"); found && len(result.Buckets) > 0 {
			title := result.Buckets[0]
			profileMin.Title = title.Key.(string)
		}
		if result, found := profileResult.Terms("version"); found && len(result.Buckets) > 0 {
			version := result.Buckets[0]
			profileMin.Version = version.Key.(string)
		}
	}

}

func (backend *ES2Backend) getWaiverData(waiverDataBuckets *elastic.AggregationBucketKeyItems) ([]*reportingapi.WaiverData, error) {
	waiverDataCollection := make([]*reportingapi.WaiverData, 0)

	for _, waiverDataBucket := range waiverDataBuckets.Buckets {
		if aggResult, found := waiverDataBucket.Aggregations.Terms("expiration_date"); found && len(aggResult.Buckets) > 0 {
			for _, expirationBucket := range aggResult.Buckets {
				expDate, ok := expirationBucket.Key.(string)
				if !ok {
					logrus.Errorf("could not convert the value of expiration date: %v, to a string! Will set expiration date to empty string", expirationBucket)
				}
				if aggResult, found := expirationBucket.Aggregations.Terms("justification"); found && len(aggResult.Buckets) > 0 {
					for _, justificationBucket := range aggResult.Buckets {
						justification, ok := justificationBucket.Key.(string)
						if !ok {
							logrus.Errorf("could not convert the value of justification: %v, to a string! Will set justification to an empty string", justificationBucket)
						}

						controlSummary := &reportingapi.ControlSummary{
							Passed:  &reportingapi.Total{},
							Skipped: &reportingapi.Total{},
							Failed:  &reportingapi.Failed{},
							Waived:  &reportingapi.Total{},
						}

						if waived, found := justificationBucket.Aggregations.Filter("waived"); found {
							controlSummary.Waived.Total = int32(waived.DocCount)
						}
						if passed, found := justificationBucket.Aggregations.Filter("passed"); found {
							controlSummary.Passed.Total = int32(passed.DocCount)
						}
						if skipped, found := justificationBucket.Aggregations.Filter("skipped"); found {
							controlSummary.Skipped.Total = int32(skipped.DocCount)
						}
						if failed, found := justificationBucket.Aggregations.Filter("failed"); found {
							total := int32(failed.DocCount)
							controlSummary.Failed.Total = total
						}

						waiverData := &reportingapi.WaiverData{
							WaivedStr:      waiverDataBucket.Key.(string),
							ExpirationDate: expDate,
							Justification:  justification,
							WaiverSummary:  controlSummary,
						}
						waiverDataCollection = append(waiverDataCollection, waiverData)
					}
				}
			}
		}
	}

	return waiverDataCollection, nil
}

//getFiltersQuery - builds up an elasticsearch query filter based on the filters map that is passed in
//  arguments: filters - is a map of filters that serve as the source for generated es query filters
//             latestOnly - specifies whether or not we are only interested in retrieving only the latest report
//  return *elastic.BoolQuery
func (backend ES2Backend) getFiltersQuery(filters map[string][]string, latestOnly bool) *elastic.BoolQuery {
	utils.DeDupFilters(filters)
	logrus.Debugf("????? Called getFiltersQuery with filters=%+v, latestOnly=%t", filters, latestOnly)
	boolQuery := elastic.NewBoolQuery()

	// These are filter types where we use ElasticSearch Term Queries
	filterTypes := []string{"environment", "organization", "chef_server", "chef_tags",
		"policy_group", "policy_name", "status", "node_name", "platform", "platform_with_version",
		"role", "recipe", "inspec_version", "ipaddress"}

	for _, filterType := range filterTypes {
		if len(filters[filterType]) > 0 {
			ESFieldName := backend.getESFieldName(filterType)
			termQuery := newTermQueryFromFilter(ESFieldName, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	if len(filters["control_name"]) > 0 {
		termQuery := newNestedTermQueryFromFilter(lower, "profiles.controls",
			filters["control_name"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_name"]) > 0 {
		termQuery := newNestedTermQueryFromFilter(profile, "profiles", filters["profile_name"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_with_version"]) > 0 {
		termQuery := newNestedTermQueryFromFilter("profiles.full.lower", "profiles", filters["profile_with_version"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["node_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("node_uuid", stringArrayToInterfaceArray(filters["node_id"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	numberOfProfiles := len(filters["profile_id"])
	numberOfControls := len(filters["control"])
	if numberOfProfiles > 0 || numberOfControls > 0 {
		profileBaseFscIncludes := []string{"profiles.name", "profiles.sha256", "profiles.version"}
		profileLevelFscIncludes := []string{"profiles.controls_sums", "profiles.status"}
		controlLevelFscIncludes := []string{"profiles.controls.id", "profiles.controls.status",
			impact, "profiles.controls.waived_str"}

		profileAndControlQuery := getProfileAndControlQuery(filters, profileBaseFscIncludes,
			profileLevelFscIncludes, controlLevelFscIncludes)
		boolQuery = boolQuery.Must(profileAndControlQuery)
	}

	if len(filters["start_time"]) > 0 || len(filters["end_time"]) > 0 {
		endTime := firstOrEmpty(filters["end_time"])
		startTime := firstOrEmpty(filters["start_time"])
		timeRangeQuery := elastic.NewRangeQuery("end_time")
		if len(startTime) > 0 {
			timeRangeQuery.Gte(startTime)
		}
		if len(endTime) > 0 {
			timeRangeQuery.Lte(endTime)
		}

		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	if len(filters["projects"]) > 0 {
		projectsQuery := elastic.NewBoolQuery()

		if stringutils.SliceContains(filters["projects"], authzConstants.UnassignedProjectID) {
			emptyProjectQuery := elastic.NewBoolQuery()
			emptyProjectQuery.MustNot(elastic.NewExistsQuery("projects"))
			projectsQuery.Should(emptyProjectQuery)
		}

		assignedProjectIds := stringutils.SliceFilter(filters["projects"], func(projectId string) bool {
			return projectId != authzConstants.UnassignedProjectID
		})

		if len(assignedProjectIds) > 0 {
			projectMatchQuery := elastic.NewTermsQuery("projects", stringArrayToInterfaceArray(assignedProjectIds)...)
			projectsQuery.Should(projectMatchQuery)
		}

		boolQuery = boolQuery.Filter(projectsQuery)
	}

	if len(filters["job_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("job_uuid", stringArrayToInterfaceArray(filters["job_id"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if latestOnly {
		// only if there is no job_id filter set, do we want the daily latest
		setFlags, err := filterQueryChange(firstOrEmpty(filters["end_time"]), firstOrEmpty(filters["start_time"]))
		if err != nil {
			errors.Errorf("cannot parse the time %v", err)
			return nil
		}
		for _, flag := range setFlags {
			termQuery := elastic.NewTermsQuery(flag, true)
			boolQuery = boolQuery.Must(termQuery)
		}

	}

	if len(filters["end_time"]) == 0 {
		// If we don't have an end_time filter, we limit to last 24 hours timeframe
		timeRangeQuery := elastic.NewRangeQuery("end_time")
		timeRangeQuery.Gt(time.Now().Add(-24 * time.Hour).UTC().Format(time.RFC3339))
		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	// Going through all filters to find the ones prefixed with 'control_tag', e.g. 'control_tag:nist'
	for filterType := range filters {
		if strings.HasPrefix(filterType, "control_tag:") {
			_, tagKey := leftSplit(filterType, ":")
			termQuery := newNestedTermQueryFromControlTagsFilter(tagKey, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	return boolQuery
}

func (backend ES2Backend) getESFieldName(filterType string) string {
	ESFieldName := filterType
	switch filterType {
	case "chef_server":
		ESFieldName = "source_fqdn.lower"
	case "inspec_version":
		ESFieldName = "version.lower"
	case "organization":
		ESFieldName = "organization_name.lower"
	case "platform":
		ESFieldName = "platform.name.lower"
	case "platform_with_version":
		ESFieldName = "platform.full.lower"
	case "recipe":
		ESFieldName = "recipes.lower"
	case "role":
		ESFieldName = "roles.lower"
	case "environment":
		ESFieldName = "environment.lower"
	case "chef_tags":
		ESFieldName = "chef_tags.lower"
	case "policy_group":
		ESFieldName = "policy_group.lower"
	case "policy_name":
		ESFieldName = "policy_name.lower"
	case "node_name":
		ESFieldName = "node_name.lower"
	}

	return ESFieldName
}

func newTermQueryFromFilter(ESField string,
	filters []string) *elastic.BoolQuery {
	refinedValues := make([]string, 0, 0)
	filterQuery := elastic.NewBoolQuery()

	for _, value := range filters {
		if containsWildcardChar(value) {
			wildQuery := elastic.NewWildcardQuery(ESField, value)
			filterQuery = filterQuery.Should(wildQuery)
		} else {
			refinedValues = append(refinedValues, value)
		}
	}
	if len(refinedValues) > 0 {
		termQuery := elastic.NewTermsQuery(ESField, stringArrayToInterfaceArray(refinedValues)...)
		filterQuery = filterQuery.Should(termQuery)
	}
	return filterQuery
}

func newNestedTermQueryFromFilter(ESField string, ESFieldPath string,
	filterValues []string) *elastic.BoolQuery {
	refinedValues := make([]string, 0, 0)
	filterQuery := elastic.NewBoolQuery()

	for _, filterValue := range filterValues {
		if containsWildcardChar(filterValue) {
			wildQuery := elastic.NewWildcardQuery(ESField, filterValue)
			nestedQuery := elastic.NewNestedQuery(ESFieldPath, wildQuery)
			filterQuery = filterQuery.Should(nestedQuery)
		} else {
			refinedValues = append(refinedValues, filterValue)
		}
	}
	if len(refinedValues) > 0 {
		termQuery := elastic.NewTermsQuery(ESField, stringArrayToInterfaceArray(refinedValues)...)
		nestedQuery := elastic.NewNestedQuery(ESFieldPath, termQuery)
		filterQuery = filterQuery.Should(nestedQuery)
	}

	return filterQuery
}

// Returns an ElasticSearch nested query to filter reports by control tags
func newNestedTermQueryFromControlTagsFilter(tagKey string, tagValues []string) *elastic.NestedQuery {
	refinedValues := make([]string, 0, 0)
	ESFieldPath := "profiles.controls.string_tags"
	ESFieldTagKey := "profiles.controls.string_tags.key.lower"
	ESFieldTagValues := "profiles.controls.string_tags.values.lower"

	// Add ElasticSearch query filter for the control tag key
	tagKey = strings.ToLower(tagKey)
	boolQuery := elastic.NewBoolQuery()
	if containsWildcardChar(tagKey) {
		boolQuery.Must(elastic.NewWildcardQuery(ESFieldTagKey, tagKey))
	} else {
		boolQuery.Must(elastic.NewTermsQuery(ESFieldTagKey, tagKey))
	}

	// Add ElasticSearch query filters for the control tag value(s)
	insideBoolQuery := elastic.NewBoolQuery()
	insideBool := false
	emptyValuesRequested := false
	for _, tagValue := range tagValues {
		tagValue = strings.ToLower(tagValue)
		if containsWildcardChar(tagValue) {
			insideBoolQuery.Should(elastic.NewWildcardQuery(ESFieldTagValues, tagValue))
			insideBool = true
		} else if tagValue == "" {
			emptyValuesRequested = true
		} else {
			refinedValues = append(refinedValues, tagValue)
		}
	}
	// Here we handle control tag value(s) without wildcard characters
	if len(refinedValues) > 0 {
		termQuery := elastic.NewTermsQuery(ESFieldTagValues, stringArrayToInterfaceArray(refinedValues)...)
		insideBoolQuery.Should(termQuery)
		insideBool = true
	}
	// Here we handle the case where we want a tag key with NO values
	if emptyValuesRequested {
		existsQuery := elastic.NewExistsQuery(ESFieldTagValues)
		insideBoolQuery.Should(elastic.NewBoolQuery().MustNot(existsQuery))
		insideBool = true
	}
	if insideBool {
		boolQuery.Must(insideBoolQuery)
	}

	nestedQuery := elastic.NewNestedQuery(ESFieldPath, boolQuery)
	return nestedQuery
}

func containsWildcardChar(value string) bool {
	wildcardValues := []string{"*", "?"}

	for _, wildcard := range wildcardValues {
		if strings.Contains(value, wildcard) {
			return true
		}
	}

	return false
}

//  getFiltersQueryForDeepReport - builds up an elasticsearch query filter based on the reportId filters map passed in.
//   This func ignores all but the profile_id and control filter.  If a single profile_id or a single profile_id and a
//   child control from the single profile_id is passed in, then we are in deep filtering mode and the
//   fetchSourceContexts will be adjusted accordingly
//    arguments:
//			reportId - the id of the report we are querying for.
//			filters - is a map of filters that serve as the source for generated es query filters
//    return *elastic.BoolQuery
func getFiltersQueryForDeepReport(reportId string,
	filters map[string][]string) *elastic.BoolQuery {

	utils.DeDupFilters(filters)

	boolQuery := elastic.NewBoolQuery()

	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportId)
	boolQuery = boolQuery.Must(idsQuery)

	if len(filters["projects"]) > 0 {
		projectsQuery := elastic.NewBoolQuery()

		if stringutils.SliceContains(filters["projects"], authzConstants.UnassignedProjectID) {
			emptyProjectQuery := elastic.NewBoolQuery()
			emptyProjectQuery.MustNot(elastic.NewExistsQuery("projects"))
			projectsQuery.Should(emptyProjectQuery)
		}

		assignedProjectIds := stringutils.SliceFilter(filters["projects"], func(projectId string) bool {
			return projectId != authzConstants.UnassignedProjectID
		})

		if len(assignedProjectIds) > 0 {
			projectMatchQuery := elastic.NewTermsQuery("projects", stringArrayToInterfaceArray(assignedProjectIds)...)
			projectsQuery.Should(projectMatchQuery)
		}

		boolQuery = boolQuery.Filter(projectsQuery)
	}

	numberOfProfiles := len(filters["profile_id"])
	numberOfControls := len(filters["control"])
	if numberOfProfiles > 0 || numberOfControls > 0 {
		profileBaseFscIncludes := []string{
			"profiles.depends",
			"profiles.name",
			"profiles.sha256",
			"profiles.status",
			"profiles.skip_message",
			"profiles.version"}

		profileLevelFscIncludes := []string{"profiles"}
		controlLevelFscIncludes := []string{"profiles.controls"}

		profileAndControlQuery := getProfileAndControlQuery(
			filters,
			profileBaseFscIncludes,
			profileLevelFscIncludes,
			controlLevelFscIncludes,
		)
		boolQuery = boolQuery.Must(profileAndControlQuery)
	}

	return boolQuery
}

// getFilterQueryWithPagination creates a boolquery with filters and pagination support.
func (backend ES2Backend) getFilterQueryWithPagination(reportId string,
	filters map[string][]string) (*elastic.BoolQuery, error) {
	utils.DeDupFilters(filters)
	boolQuery := elastic.NewBoolQuery()

	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportId)
	boolQuery = boolQuery.Must(idsQuery)

	profileAndControlQuery, err := getProfileAndControlQueryWithPagination(filters)
	if err != nil {
		return nil, err
	}
	boolQuery = boolQuery.Must(profileAndControlQuery)
	return boolQuery, nil
}

func getProfileAndControlQuery(filters map[string][]string, profileBaseFscIncludes, profileLevelFscIncludes,
	controlLevelFscIncludes []string) *elastic.NestedQuery {
	var profileIds string
	var profileLevelFsc *elastic.FetchSourceContext
	numberOfProfiles := len(filters["profile_id"])
	numberOfControls := len(filters["control"])
	if numberOfProfiles == 0 {
		profileIds = ".*"
	} else {
		profileIds = strings.Join(filters["profile_id"], "|")
		if numberOfProfiles == 1 && numberOfControls <= 1 {
			//we are deep and when that's the case, we know we want at least this stuff.
			// if we don't have controls, we'll add even more to the profile level context (below)
			profileLevelFsc = elastic.NewFetchSourceContext(true).
				Include(profileBaseFscIncludes...)
		}
	}
	profileBoolQuery := elastic.NewBoolQuery()
	stringQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.sha256:/(%s)/", profileIds))
	profileBoolQuery.Must(stringQuery)

	var nestedQuery *elastic.NestedQuery
	nestedQuery = elastic.NewNestedQuery("profiles", profileBoolQuery)

	//add in the control query if it exists
	if numberOfControls > 0 {
		var nestedControlQuery *elastic.NestedQuery

		controlQuery := elastic.NewQueryStringQuery(fmt.Sprintf("(%s)", getMultiControlString(filters["control"])))
		controlQuery = controlQuery.Field("profiles.controls.id")
		nestedControlQuery = elastic.NewNestedQuery("profiles.controls", controlQuery)

		if numberOfProfiles == 1 && numberOfControls == 1 {
			//we are control deep and when that's the case, we know we want this stuff in fsc.
			//we also know that we want to nest it so that we may use it for reports or node info
			fsc := elastic.NewFetchSourceContext(true).
				Include(controlLevelFscIncludes...)
			nestedControlQuery.InnerHit(elastic.NewInnerHit().FetchSourceContext(fsc))
		}

		profileBoolQuery = profileBoolQuery.Must(nestedControlQuery)
	} else if numberOfProfiles == 1 && profileLevelFsc != nil {
		//we are profile deep and when that's the case, we know we want this stuff in fsc.
		//for deep filtering, we only need to include profile level controls_sums and status at profile level,
		// if we are filtering at profile level.. to include it when we are at control level would be inefficient
		profileLevelFsc.Include(profileLevelFscIncludes...)
	}

	//we are deep so lets add the fetch source context
	if numberOfProfiles == 1 && numberOfControls <= 1 {
		nestedQuery.InnerHit(elastic.NewInnerHit().FetchSourceContext(profileLevelFsc))
	}
	return nestedQuery
}

func paginate(pageNum int, size int, length int) (int, int) {
	start := (pageNum - 1) * size

	if start > length {
		start = length
	}

	end := start + size
	if end > length {
		end = length
	}

	return start, end
}

// getProfileAndControlQueryWithPagination creates a profile and control filter query with pagination
func getProfileAndControlQueryWithPagination(filters map[string][]string) (*elastic.NestedQuery, error) {
	var profileIds, controlIds string
	numberOfProfiles := len(filters["profile_id"])
	numberOfControls := len(filters["control"])

	var status string
	if temp, ok := filters["status"]; ok {
		status = strings.Join(temp, "|")
	}

	if numberOfProfiles == 0 {
		profileIds = ".*"
	} else {
		profileIds = strings.Join(filters["profile_id"], "|")
	}

	if numberOfControls > 0 {
		controlIds = strings.Join(filters["control"], "|")
	}

	profileControlQuery := elastic.NewBoolQuery()

	if numberOfProfiles > 0 && numberOfControls > 0 {
		profileQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.sha256:/(%s)/", profileIds))
		controlQuery := controlQuery(controlIds, status)
		nestedControlQuery, err := createPaginatedControl(controlQuery, filters)
		if err != nil {
			return nil, err
		}
		profileControlQuery = profileControlQuery.Must(profileQuery)
		profileControlQuery = profileControlQuery.Must(nestedControlQuery)
	} else if numberOfControls > 0 {
		controlQuery := controlQuery(controlIds, status)
		nestedControlQuery, err := createPaginatedControl(controlQuery, filters)
		if err != nil {
			return nil, err
		}
		profileControlQuery = profileControlQuery.Must(nestedControlQuery)
	} else if numberOfProfiles > 0 {
		profileQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.sha256:/(%s)/", profileIds))
		var controlQuery elastic.Query
		if status != "" {
			controlQuery = elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.status:/(%s)/", status))
		} else {
			controlQuery = elastic.NewMatchAllQuery()
		}
		nestedControlQuery, err := createPaginatedControl(controlQuery, filters)
		if err != nil {
			return nil, err
		}
		profileControlQuery = profileControlQuery.Must(profileQuery)
		profileControlQuery = profileControlQuery.Must(nestedControlQuery)
	} else {
		var controlQuery elastic.Query
		if status != "" {
			controlQuery = elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.status:/(%s)/", status))
		} else {
			controlQuery = elastic.NewMatchAllQuery()
		}
		nestedControlQuery, err := createPaginatedControl(controlQuery, filters)
		if err != nil {
			return nil, err
		}
		return nestedControlQuery, nil
	}
	nestedQuery := elastic.NewNestedQuery("profiles", profileControlQuery)
	nestedQuery = nestedQuery.InnerHit(elastic.NewInnerHit())

	return nestedQuery, nil
}

// controlQuery creates a query based on controlIds and status
func controlQuery(controlIds, status string) elastic.Query {
	var controlQuery elastic.Query
	if status != "" {
		idQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.id:/(%s)/", controlIds))
		statusQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.status:/(%s)/", status))
		boolQuery := elastic.NewBoolQuery()
		controlQuery = boolQuery.Must(idQuery, statusQuery)
	} else {
		controlQuery = elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.id:/(%s)/", controlIds))
	}
	return controlQuery
}

// populateControlElements creates the control lists for each search hit.
func populateControlElements(searchHits *elastic.SearchHit, profiles map[int]struct {
	Name   string
	Sha256 string
}) ([]*reportingapi.ControlElement, error) {
	var listControls = make([]*reportingapi.ControlElement, 0)
	var tempControl struct {
		ID        string
		Impact    float32
		Results   []*reportingapi.Result
		Title     string
		ProfileID string
		Status    string
	}
	for _, innerhit := range searchHits.InnerHits["profiles.controls"].Hits.Hits {
		control := &reportingapi.ControlElement{}
		err = json.Unmarshal(innerhit.Source, &tempControl)
		if err != nil {
			logrus.Errorf("error unmarshalling the search control response: %+v", err)
			return listControls, err
		}

		// store the control result to temporary control element
		profileOffset := innerhit.Nested.Offset
		profile, ok := profiles[profileOffset]
		if !ok {
			err := fmt.Errorf("error in fetching profile information for given control")
			logrus.Error(err)
			return listControls, err
		}

		control.Id = tempControl.ID
		control.Impact = tempControl.Impact
		control.Results = int32(len(tempControl.Results))
		control.Title = tempControl.Title
		control.Profile = profile.Name
		control.ProfileId = profile.Sha256
		control.Status = tempControl.Status
		listControls = append(listControls, control)
	}
	return listControls, nil
}

// createPaginatedControl creates a nested query with pagination and control filter
func createPaginatedControl(query elastic.Query, filters map[string][]string) (*elastic.NestedQuery, error) {
	from, size, err := paginatedParams(filters)
	if err != nil {
		return nil, err
	}
	nestedQuery := elastic.NewNestedQuery("profiles.controls", query)
	nestedQuery = nestedQuery.InnerHit(elastic.NewInnerHit().From(from).Size(size))
	return nestedQuery, nil
}

func paginatedParams(filters map[string][]string) (int, int, error) {
	var from, size = 0, 10
	var err error
	if len(filters["from"]) > 0 {
		from, err = strconv.Atoi(filters["from"][0])
	}
	if len(filters["size"]) > 0 {
		size, err = strconv.Atoi(filters["size"][0])
	}
	return from, size, err
}

// GetReportManagerRequest takes report id and filters to populate the report manager request
func (backend *ES2Backend) GetReportManagerRequest(reportId string, filters map[string][]string) (*reportingapi.ReportResponse, error) {
	mgrRequest := &reportingapi.ReportResponse{}
	var method = "GetReportManagerRequest"
	searchResult, queryInfo, err := backend.getSearchResult(reportId, filters, method)
	if err != nil {
		return mgrRequest, err
	}
	// we should only receive one searchResult value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits.Value > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(hit.Source, &esInSpecReport)
				if err != nil {
					logrus.Errorf("error unmarshalling the search response: %+v", err)
					return mgrRequest, err
				}

				var esInspecProfiles []ESInSpecReportProfile //`json:"profiles"`

				if queryInfo.level == ReportLevel {
					esInspecProfiles = esInSpecReport.Profiles
				} else if queryInfo.level == ProfileLevel || queryInfo.level == ControlLevel {
					esInspecProfiles, _, err = getDeepInspecProfiles(hit, queryInfo)
					if err != nil {
						//todo - handle this
						logrus.Errorf("GetReportManagerRequest time error: %s", err.Error())
					}
				}
				// read all profiles
				profiles := make([]*reportingapi.Profile, 0)
				for _, esInSpecReportProfileMin := range esInspecProfiles {
					logrus.Debugf("Determine profile: %s", esInSpecReportProfileMin.Name)
					esInSpecProfile, err := backend.GetProfile(esInSpecReportProfileMin.SHA256)
					if err != nil {
						logrus.Errorf("GetReportManagerRequest - Could not get profile '%s' error: %s", esInSpecReportProfileMin.SHA256, err.Error())
						logrus.Debug("GetReportManagerRequest - Making the most from the profile information in esInSpecReportProfileMin")
						esInSpecProfile.Sha256 = esInSpecReportProfileMin.SHA256
					}

					reportProfile := inspec.Profile{}
					reportProfile.Sha256 = esInSpecProfile.Sha256

					reportProfile.Controls = make([]inspec.Control, len(esInSpecReportProfileMin.Controls))

					// Creating a map of report control ids to avoid a n^2 complexity later on when we look for the
					// matching profile control id
					profileControlsMap := make(map[string]*reportingapi.Control, len(esInSpecProfile.Controls))
					for _, control := range esInSpecProfile.Controls {
						profileControlsMap[control.Id] = control
					}

					convertedControls := make([]*reportingapi.Control, 0)
					// Enrich min report controls with profile metadata
					for _, reportControlMin := range esInSpecReportProfileMin.Controls {
						if controlFromMap, ok := profileControlsMap[reportControlMin.ID]; ok {
							reportControlMin.Tags = controlFromMap.Tags
							// store controls to returned request
							convertedControl := convertControl(profileControlsMap, reportControlMin, filters)
							if convertedControl != nil {
								convertedControls = append(convertedControls, convertedControl)
							}

						} else {
							logrus.Warnf("GetReportManagerRequest: %s was not found in the profile control map",
								reportControlMin.ID)
						}
					}
					if len(convertedControls) > 0 {
						convertedProfile := reportingapi.Profile{
							Sha256:   reportProfile.Sha256,
							Controls: convertedControls,
						}
						profiles = append(profiles, &convertedProfile)
					}
				}
				mgrRequest.ReportId = hit.Id
				for _, profile := range profiles {
					tempProfile := &reportingapi.ProfileResponse{}
					tempProfile.ProfileId = profile.Sha256
					for _, control := range profile.Controls {
						tempProfile.Controls = append(tempProfile.Controls, control.Id)
						sort.Strings(tempProfile.Controls)
					}
					mgrRequest.Profiles = append(mgrRequest.Profiles, tempProfile)
				}
			}
		}
		return mgrRequest, nil
	}
	return mgrRequest, errorutils.ProcessNotFound(nil, mgrRequest.ReportId)
}

func (backend *ES2Backend) getSearchResult(reportId string, filters map[string][]string, method string) (*elastic.SearchResult, *QueryInfo, error) {
	depth, err := backend.NewDepth(filters, false)
	if err != nil {
		return nil, nil, errors.Errorf("%s unable to get depth level for report, %s", method, err.Error())
	}
	queryInfo := depth.getQueryInfo()

	//normally, we compute the boolQuery when we create a new Depth obj.. here we, instead call a variation of the full
	// query builder. we need to do this because this variation of filter query provides this func with deeper
	// information about the report being retrieved.
	queryInfo.filtQuery = getFiltersQueryForDeepReport(reportId, filters)
	logrus.Debugf("%s will retrieve report %s based on filters %+v", method, reportId, filters)

	fsc := elastic.NewFetchSourceContext(true)

	if queryInfo.level != ReportLevel {
		fsc.Exclude("profiles")
	}
	logrus.Debugf("%s for reportid=%s, filters=%+v", method, reportId, filters)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(queryInfo.filtQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, nil, errors.Errorf("%s unable to get Source, %s", method, err.Error())
	}
	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query searchSource", method))

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
		return nil, nil, errors.Errorf("%s unable to complete search, %s", method, err.Error())
	}

	logrus.Debugf("%s got %d reports in %d milliseconds\n", method, searchResult.TotalHits(),
		searchResult.TookInMillis)

	return searchResult, queryInfo, nil
}

// getControlSummaryFromControlIndex is constructing query for getting control summary for various filters and returning the result in a map
func (backend *ES2Backend) getControlSummaryFromControlIndex(ctx context.Context, controlId []string, filters map[string][]string, esIndex string, size int32) (map[string]*reportingapi.ControlSummary, error) {
	nodeStatus := "nodes.status"
	controlSummaryMap := make(map[string]*reportingapi.ControlSummary)
	client, err := backend.ES2Client()
	if err != nil {
		return nil, err
	}
	boolQuery := getControlSummaryFilters(controlId, filters)

	filterQuery := elastic.NewBoolQuery()

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		FetchSource(false).Size(1)

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery(nodeStatus, "passed")))

	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery(nodeStatus, "failed")))

	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery(nodeStatus, "skipped")))

	setFlags, err := filterQueryChange(firstOrEmpty(filters["end_time"]), firstOrEmpty(filters["start_time"]))
	if err != nil {
		logrus.Errorf("Unable to fetch details for flags %v", err)
	}
	for _, flag := range setFlags {
		termQuery := elastic.NewTermsQuery("nodes."+flag, true)
		filterQuery = filterQuery.Must(termQuery)
	}

	filterAggregationForLatestRecord := elastic.NewFilterAggregation().Filter(filterQuery).
		SubAggregation("failed", failedFilter).
		SubAggregation("passed", passedFilter).
		SubAggregation("skipped", skippedFilter)

	subAggregationNodes := elastic.NewNestedAggregation().Path("nodes").SubAggregation("status", filterAggregationForLatestRecord)

	//waivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	controlTermsAggregation := elastic.NewTermsAggregation().Field("control_id").SubAggregation("nodes", subAggregationNodes).Size(int(size))

	searchSource.Aggregation("controls", controlTermsAggregation)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrapf(err, "%s unable to get Source", "ControlSummary")
	}
	LogQueryPartMin(esIndex, source, fmt.Sprintf("%s query", "ControlSummary"))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(ctx)

	if err != nil {
		return nil, err
	}

	if controlBuckets, found := searchResult.Aggregations.Terms("controls"); found && len(controlBuckets.Buckets) > 0 {
		for _, bucket := range controlBuckets.Buckets {
			controlSummary := &reportingapi.ControlSummary{
				Passed:  &reportingapi.Total{},
				Skipped: &reportingapi.Total{},
				Failed:  &reportingapi.Failed{},
				Waived:  &reportingapi.Total{},
			}
			if nodesBuckets, found := bucket.Aggregations.Nested("nodes"); found {
				controlSummary = getControlSummaryResult(nodesBuckets)
			}

			id, ok := bucket.Key.(string)
			if !ok {
				logrus.Errorf("Unable to find id for the control in new index structure %v", err)
				continue
			}
			controlSummaryMap[id] = controlSummary
		}
	}
	return controlSummaryMap, nil

}

// getControlSummaryResult gets the result from the summary query from index comp-1-control-*
func getControlSummaryResult(nodesBucket *elastic.AggregationSingleBucket) (controlSummary *reportingapi.ControlSummary) {
	controlSummary = &reportingapi.ControlSummary{
		Passed:  &reportingapi.Total{},
		Skipped: &reportingapi.Total{},
		Failed:  &reportingapi.Failed{},
		Waived:  &reportingapi.Total{},
	}

	if status, found := nodesBucket.Aggregations.Filter("status"); found {
		if failed, found := status.Aggregations.Filter("failed"); found {
			controlSummary.Failed.Total = int32(failed.DocCount)
		}

		if passed, found := status.Aggregations.Filter("passed"); found {
			controlSummary.Passed.Total = int32(passed.DocCount)
		}

		if skipped, found := status.Aggregations.Filter("skipped"); found {
			controlSummary.Skipped.Total = int32(skipped.DocCount)
		}
	}

	return controlSummary

}

// getControlSummaryFilters is applying filters to the query for index comp-1-control-*
func getControlSummaryFilters(controlId []string, filters map[string][]string) *elastic.BoolQuery {

	boolQuery := elastic.NewBoolQuery()

	controlTermsQuery := elastic.NewTermsQueryFromStrings("control_id", controlId...)

	if len(filters["start_time"]) > 0 || len(filters["end_time"]) > 0 {
		endTime := firstOrEmpty(filters["end_time"])
		startTime := firstOrEmpty(filters["start_time"])
		timeRangeQuery := elastic.NewRangeQuery("end_time")
		if len(startTime) > 0 {
			timeRangeQuery.Gte(startTime)
		}
		if len(endTime) > 0 {
			timeRangeQuery.Lte(endTime)
		}

		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	controlQuery := boolQuery.Must(controlTermsQuery)

	return controlQuery

}

//getMultiControlString takes the list of strings and returns the query string used in search.
func getMultiControlString(list []string) string {
	controlList := []string{}
	for _, control := range list {
		controlList = append(controlList, handleSpecialChar(control))
	}
	return strings.Join(controlList, " ")
}

//handleSpecialChar takes the string, and add additional char in front of the reserved chars.
func handleSpecialChar(term string) string {
	//maintain `\` always first in the list to stop encoding the characters we are appending.
	reservedChar := []string{`\`, `/`, `+`, `-`, `=`, `&&`, `||`, `>`, `<`, `!`, `(`, `)`, `{`, `}`, `[`, `]`, `^`, `"`, `~`, `*`, `?`, `:`}
	for _, rChar := range reservedChar {
		term = strings.ReplaceAll(term, rChar, fmt.Sprintf("\\%s", rChar))
	}
	return fmt.Sprintf("(%s)", term)
}

func filterQueryChange(endTime string, startTime string) ([]string, error) {
	if len(endTime) == 0 && len(startTime) == 0 {
		return []string{"day_latest", "daily_latest"}, nil
	}
	if len(startTime) == 0 {
		return []string{"daily_latest"}, nil
	}
	eTime, err := time.Parse(layout, endTime)
	sTime, err := time.Parse(layout, startTime)
	diff := int(eTime.Sub(sTime).Hours() / 24)
	if err != nil {
		return nil, errors.Errorf("cannot parse the time")
	}
	if diff == 0 {
		return []string{"daily_latest"}, nil
	}
	return []string{"day_latest", "daily_latest"}, nil

}

func validateFiltersTimeRange(endTime string, startTime string) error {
	if len(endTime) == 0 || len(startTime) == 0 {
		return nil
	}
	eTime, err := time.Parse(layout, endTime)
	sTime, err := time.Parse(layout, startTime)
	diff := int(eTime.Sub(sTime).Hours() / 24)
	if err != nil {
		return errors.Errorf("cannot parse the time")
	}
	if diff > 90 {
		return errors.Errorf("Range of start time and end time should not be greater than 90 days")
	} else if diff < 0 {
		return errors.Errorf("Start time should not be greater than end time")
	}
	return nil
}

func isDateRange(endTime string, startTime string) (bool, error) {
	if len(endTime) == 0 || len(startTime) == 0 {
		return false, nil
	}
	eTime, err := time.Parse(layout, endTime)
	if err != nil {
		return false, errors.Errorf("cannot parse the time")
	}
	sTime, err := time.Parse(layout, startTime)
	if err != nil {
		return false, errors.Errorf("cannot parse the time")
	}
	diff := int(eTime.Sub(sTime).Hours() / 24)

	if diff > 1 {
		return true, nil
	} else if diff < 0 {
		return false, errors.Errorf("Start time should not be greater than end time")
	}
	return false, nil
}

func getStartDateFromEndDate(endTime string, startTime string) ([]string, error) {
	if len(endTime) == 0 {
		return nil, nil
	}

	parsedEndTime, err := time.Parse(time.RFC3339, endTime)
	if err != nil {
		return []string{}, err
	}

	if checkTodayIsEndTime(parsedEndTime) {
		if startTime == "" {
			return []string{}, nil
		}
		return []string{startTime}, nil
	}
	newStartTime := time.Date(parsedEndTime.Year(), parsedEndTime.Month(), parsedEndTime.Day(), 0, 0, 0, 0, time.Local)

	return []string{newStartTime.Format(time.RFC3339)}, nil

}

func checkTodayIsEndTime(endTime time.Time) bool {
	currentDay := time.Now()

	if currentDay.Year() == endTime.Year() && currentDay.Month() == endTime.Month() && currentDay.Day() == endTime.Day() {
		return true
	}
	return false
}

func (backend *ES2Backend) GetControlListItemsRange(ctx context.Context, filters map[string][]string,
	size int32, pageNumber int32) (*reportingapi.ControlItems, error) {
	myName := "GetControlListItemsRange"

	contListItems := make([]*reportingapi.ControlItem, 0)

	controlListIds := make([]string, 0)

	controlSummaryTotals := &reportingapi.ControlSummary{
		Passed:  &reportingapi.Total{},
		Skipped: &reportingapi.Total{},
		Failed:  &reportingapi.Failed{},
		Waived:  &reportingapi.Total{},
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return nil, err
	}

	filters["start_time"], err = getStartDateFromEndDate(firstOrEmpty(filters["end_time"]), firstOrEmpty(filters["start_time"]))
	esIndex, err := GetEsIndex(filters, false)
	if err != nil {
		return nil, errors.Wrap(err, myName)
	}

	controlIndex, err := getControlIndex(filters)
	if err != nil {
		return nil, errors.Wrap(err, myName)
	}
	//here, we set latestOnly to true.  We may need to set it to false if we want to search non lastest reports
	//for now, we don't search non-latest reports so don't do it.. it's slower for obvious reasons.
	latestOnly := FetchLatestDataOrNot(filters)

	filtQuery := backend.getFiltersQuery(filters, latestOnly)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		FetchSource(false).
		Size(1)

	controlTermsAgg := elastic.NewTermsAggregation().Field("profiles.controls.id").
		Size(int(size*pageNumber)).
		Order("_key", true)

	controlTermsAgg.SubAggregation("impact",
		elastic.NewTermsAggregation().Field(impact).Size(1))

	controlTermsAgg.SubAggregation("title",
		elastic.NewTermsAggregation().Field("profiles.controls.title").Size(1))

	waivedQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes", "yes_run")
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "passed")).
		MustNot(waivedQuery))

	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "failed")).
		MustNot(waivedQuery))

	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "skipped")).
		MustNot(waivedQuery))

	waivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	controlTermsAgg.SubAggregation("skipped", skippedFilter)
	controlTermsAgg.SubAggregation("failed", failedFilter)
	controlTermsAgg.SubAggregation("passed", passedFilter)
	controlTermsAgg.SubAggregation("waived", waivedFilter)

	waivedStrAgg := elastic.NewTermsAggregation().Field("profiles.controls.waived_str").Size(4) //there are 4 different waived_str states
	waivedButNotRunQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes")
	waivedAndRunQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes_run")
	waiverDataPassedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "passed")).
		Must(waivedAndRunQuery))

	waiverDataFailedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermQuery("profiles.controls.status", "failed")).
		Must(waivedAndRunQuery))

	waiverDataSkippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Should(elastic.NewTermQuery("profiles.controls.status", "skipped")).
		Should(waivedButNotRunQuery))

	waiverDataWaivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	waiverDataJustificationAgg := elastic.NewTermsAggregation().Field("profiles.controls.waiver_data.justification").Size(reporting.ESize)
	waiverDataJustificationAgg.SubAggregation("skipped", waiverDataSkippedFilter)
	waiverDataJustificationAgg.SubAggregation("failed", waiverDataFailedFilter)
	waiverDataJustificationAgg.SubAggregation("passed", waiverDataPassedFilter)
	waiverDataJustificationAgg.SubAggregation("waived", waiverDataWaivedFilter)

	waiverDataExpirationDateAgg := elastic.NewTermsAggregation().Field("profiles.controls.waiver_data.expiration_date").Size(reporting.ESize)
	waiverDataExpirationDateAgg.SubAggregation("justification", waiverDataJustificationAgg)
	waivedStrAgg.SubAggregation("expiration_date", waiverDataExpirationDateAgg)

	waivedStrFilter := elastic.NewFilterAggregation().Filter(waivedQuery)
	waivedStrFilter.SubAggregation("waived_str", waivedStrAgg)

	controlTermsAgg.SubAggregation("filtered_waived_str", waivedStrFilter)

	controlTermsAgg.SubAggregation("end_time", elastic.NewReverseNestedAggregation().SubAggregation("most_recent_report",
		elastic.NewTermsAggregation().Field("end_time").Size(1)))

	profileInfoAgg := elastic.NewReverseNestedAggregation().Path("profiles").SubAggregation("version",
		elastic.NewTermsAggregation().Field("profiles.name"))

	profileInfoAgg.SubAggregation("sha",
		elastic.NewTermsAggregation().Field("profiles.sha256"))

	profileInfoAgg.SubAggregation("title",
		elastic.NewTermsAggregation().Field("profiles.title"))

	profileInfoAgg.SubAggregation("version",
		elastic.NewTermsAggregation().Field("profiles.version"))

	controlTermsAgg.SubAggregation("profile", profileInfoAgg)

	controlsQuery := &elastic.BoolQuery{}
	// Going through all filters to find the ones prefixed with 'control_tag', e.g. 'control_tag:nist'
	for filterType := range filters {
		if strings.HasPrefix(filterType, "control_tag:") {
			_, tagKey := leftSplit(filterType, ":")
			termQuery := newNestedTermQueryFromControlTagsFilter(tagKey, filters[filterType])
			controlsQuery = controlsQuery.Must(termQuery)
		}
	}
	if len(filters["control_status"]) > 0 {
		controlStatusQuery := newTermQueryFromFilter("profiles.controls.status", filters["control_status"])
		controlsQuery = controlsQuery.Must(controlStatusQuery)
		logrus.Infof("controls status query %v", controlStatusQuery)
	}
	if len(filters["control_name"]) > 0 {
		controlTitlesQuery := newTermQueryFromFilter(lower, filters["control_name"])
		controlsQuery = controlsQuery.Should(controlTitlesQuery)
		logrus.Infof("controls name query %v", controlTitlesQuery)
	}
	if len(filters["control"]) > 0 {
		controlIdsQuery := newTermQueryFromFilter("profiles.controls.id", filters["control"])
		controlsQuery = controlsQuery.Should(controlIdsQuery)
		logrus.Infof("controls ids query %v", controlIdsQuery)
	}

	filteredControls := elastic.NewFilterAggregation().Filter(controlsQuery)
	filteredControls.SubAggregation("control", controlTermsAgg)
	filteredControls.SubAggregation("skipped_total", skippedFilter)
	filteredControls.SubAggregation("failed_total", failedFilter)
	filteredControls.SubAggregation("passed_total", passedFilter)
	filteredControls.SubAggregation("waived_total", waivedFilter)
	controlsAgg := elastic.NewNestedAggregation().Path("profiles.controls")
	controlsAgg.SubAggregation("filtered_controls", filteredControls)

	profilesQuery := &elastic.BoolQuery{}
	if len(filters["profile_name"]) > 0 {
		profileTitlesQuery := newTermQueryFromFilter(profile, filters["profile_name"])
		profilesQuery = profilesQuery.Should(profileTitlesQuery)
		logrus.Infof("profiles name query %v", profileTitlesQuery)
	}

	if len(filters["profile_id"]) > 0 {
		profilesShaQuery := newTermQueryFromFilter("profiles.sha256", filters["profile_id"])
		profilesQuery = profilesQuery.Should(profilesShaQuery)
	}

	filteredProfiles := elastic.NewFilterAggregation().Filter(profilesQuery)
	filteredProfiles.SubAggregation("controls", controlsAgg)
	outterMostProfilesAgg := elastic.NewNestedAggregation().Path("profiles")
	outterMostProfilesAgg.SubAggregation("filtered_profiles", filteredProfiles)

	searchSource.Aggregation("profiles", outterMostProfilesAgg)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrapf(err, "%s Unable to get control items", myName)
	}
	LogQueryPartMin(esIndex, source, fmt.Sprintf("%s query ", myName))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(ctx)

	if err != nil {
		logrus.Errorf("%s search failed", myName)
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, fmt.Sprintf("%s Search Result aggs", myName))

	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)

	if outerProfilesAggResult, found := searchResult.Aggregations.Nested("profiles"); found {
		if outerFilteredProfiles, found := outerProfilesAggResult.Aggregations.Filter("filtered_profiles"); found {
			if outerControlsAggResult, found := outerFilteredProfiles.Aggregations.Nested("controls"); found {
				if filteredControls, found := outerControlsAggResult.Aggregations.Filter("filtered_controls"); found {
					controlSummaryTotals.Total = int32(filteredControls.DocCount)
					if totalPassedControls, found := filteredControls.Aggregations.Filter("passed_total"); found {
						controlSummaryTotals.Passed.Total = int32(totalPassedControls.DocCount)
					}
					if totalFailedControls, found := filteredControls.Aggregations.Filter("failed_total"); found {
						controlSummaryTotals.Failed.Total = int32(totalFailedControls.DocCount)
					}
					if totalSkippedControls, found := filteredControls.Aggregations.Filter("skipped_total"); found {
						controlSummaryTotals.Skipped.Total = int32(totalSkippedControls.DocCount)
					}
					if totalWaivedControls, found := filteredControls.Aggregations.Filter("waived_total"); found {
						controlSummaryTotals.Waived.Total = int32(totalWaivedControls.DocCount)
					}

					if controlBuckets, found := filteredControls.Aggregations.Terms("control"); found && len(controlBuckets.Buckets) > 0 {
						start, end := paginate(int(pageNumber), int(size), len(controlBuckets.Buckets))
						for _, controlBucket := range controlBuckets.Buckets[start:end] {
							id, ok := controlBucket.Key.(string)
							if !ok {
								logrus.Errorf("could not convert the value of controlBucket: %v, to a string!", controlBucket)
							}
							contListItem, err := backend.getControlItemRange(controlBucket, id)
							if err != nil {
								return nil, err
							}
							controlListIds = append(controlListIds, id)
							contListItems = append(contListItems, &contListItem)
						}
					}
				}
			}
		}
	}
	controlSummaryMap, err := backend.getControlSummaryFromControlIndex(ctx, controlListIds, filters, controlIndex, size)
	if err != nil {
		logrus.Errorf("Unable to fetch the nodes status for controls with error %v", err)
	}
	setControlSummaryForControlItems(contListItems, controlSummaryMap)
	contListItemList := &reportingapi.ControlItems{ControlItems: contListItems, ControlSummaryTotals: controlSummaryTotals}
	return contListItemList, nil

}

func (backend *ES2Backend) getControlItemRange(controlBucket *elastic.AggregationBucketKeyItem, id string) (reportingapi.ControlItem, error) {
	contListItem := reportingapi.ControlItem{}

	contListItem.Id = id

	contListItem.Title = getTitleForControlItem(controlBucket)
	if impactAggResult, found := controlBucket.Aggregations.Terms("impact"); found {
		//there can only be one
		impactBucket := impactAggResult.Buckets[0]
		impactAsNumber, ok := impactBucket.Key.(float64)
		if !ok {
			logrus.Errorf("could not convert the value of impact: %v, to a float!", impactBucket)
		}
		contListItem.Impact = float32(impactAsNumber)
	}
	profileMin := &reportingapi.ProfileMin{}
	getProfileMinForControlItem(controlBucket, profileMin)
	contListItem.Profile = profileMin

	endTime, err := getEndTimeForControlItem(controlBucket)
	if err != nil {
		return reportingapi.ControlItem{}, errors.Wrapf(err, " time error:")
	}
	contListItem.EndTime = endTime

	if filteredWaivedStr, found := controlBucket.Aggregations.Filter("filtered_waived_str"); found {
		if waivedStrBuckets, found := filteredWaivedStr.Aggregations.Terms("waived_str"); found && len(waivedStrBuckets.Buckets) > 0 {
			contListItem.Waivers, err = backend.getWaiverData(waivedStrBuckets)
			if err != nil {
				return contListItem, err
			}
		}
	}

	return contListItem, nil

}
