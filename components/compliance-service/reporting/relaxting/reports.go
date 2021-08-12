package relaxting

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/external/lib/errorutils"
	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/stringutils"
)

const MaxScrollRecordSize = 10000

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

	nodeReport := make(map[string]reportingapi.ReportData, 0)
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

	if searchResult.TotalHits() == 0 || searchResult.Hits.TotalHits == 0 {
		logrus.Debugf("getNodeReportIdsFromTimeseries: No report ids for the given filters: %+v\n", filters)
		// no matching report IDs is not an error, just return an empty array
		return &repIds, nil
	}
	LogQueryPartMin(esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	outermostAgg, _ := searchResult.Aggregations.Terms("nodes")
	if outermostAgg != nil {
		for _, nodeBucket := range outermostAgg.Buckets {
			topHits, _ := nodeBucket.Aggregations.TopHits("distinct")
			nodeID := fmt.Sprintf("%s", nodeBucket.Key)
			for _, hit := range topHits.Hits.Hits {
				var et EndTimeSource
				if hit.Source != nil {
					err = json.Unmarshal(*hit.Source, &et)
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

				nodeReport[nodeID] = repData
			}
		}
	}
	reportIds := make([]string, len(nodeReport))
	reportData := make([]*reportingapi.ReportData, len(nodeReport))
	i := 0
	for _, v := range nodeReport {
		a := v
		reportData[i] = &a
		reportIds[i] = a.Id
		i++
	}
	repIds.Ids = reportIds
	repIds.ReportData = reportData

	logrus.Debugf("getNodeReportIdsFromTimeseries returning %d report ids in %d milliseconds\n",
		len(reportIds), searchResult.TookInMillis)

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
	idsQuery := elastic.NewIdsQuery(mappings.DocType)
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
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			item := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &item)
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

	depth, err := backend.NewDepth(filters, false)
	if err != nil {
		return report, errors.Wrap(err, "GetReport unable to get depth level for report")
	}

	queryInfo := depth.getQueryInfo()

	//normally, we compute the boolQuery when we create a new Depth obj.. here we, instead call a variation of the full
	// query builder. we need to do this because this variation of filter query provides this func with deeper
	// information about the report being retrieved.
	queryInfo.filtQuery = backend.getFiltersQueryForDeepReport(reportId, filters)

	logrus.Debugf("GetReport will retrieve report %s based on filters %+v", reportId, filters)

	fsc := elastic.NewFetchSourceContext(true)

	if queryInfo.level != ReportLevel {
		fsc.Exclude("profiles")
	}
	logrus.Debugf("GetReport for reportid=%s, filters=%+v", reportId, filters)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(queryInfo.filtQuery).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return report, errors.Wrap(err, "GetReport unable to get Source")
	}
	LogQueryPartMin(queryInfo.esIndex, source, "GetReport query searchSource")

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
		return report, errors.Wrap(err, "GetReport unable to complete search")
	}

	logrus.Debugf("GetReport got %d reports in %d milliseconds\n", searchResult.TotalHits(),
		searchResult.TookInMillis)

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esInSpecReport)
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
	size int32) (*reportingapi.ControlItems, error) {
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
	filtQuery := backend.getFiltersQuery(filters, true)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		FetchSource(false).
		Size(1)

	controlTermsAgg := elastic.NewTermsAggregation().Field("profiles.controls.id").
		Size(int(size)).
		Order("_key", true)

	controlTermsAgg.SubAggregation("impact",
		elastic.NewTermsAggregation().Field("profiles.controls.impact").Size(1))

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
		controlTitlesQuery := newTermQueryFromFilter("profiles.controls.title.lower", filters["control_name"])
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
		profileTitlesQuery := newTermQueryFromFilter("profiles.title.lower", filters["profile_name"])
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
		return nil, errors.Wrapf(err, "%s unable to get Source", myName)
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
						for _, controlBucket := range controlBuckets.Buckets {
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
				return reportingapi.ControlItem{}, errors.Wrapf(err, "%s time error: ", *name)
			}

			timestamp, err := ptypes.TimestampProto(endTimeAsTime)
			if err != nil {
				return reportingapi.ControlItem{}, errors.Wrapf(err, "%s time error: ", *name)
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

	typeQuery := elastic.NewTypeQuery(mappings.DocType)

	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(typeQuery)

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
		termQuery := newNestedTermQueryFromFilter("profiles.controls.title.lower", "profiles.controls",
			filters["control_name"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_name"]) > 0 {
		termQuery := newNestedTermQueryFromFilter("profiles.title.lower", "profiles", filters["profile_name"])
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
			"profiles.controls.impact", "profiles.controls.waived_str"}

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
		if len(filters["end_time"]) > 0 {
			// If we have an end_time filter, we use the daily_latest filter dedicated to the UTC timeseries indices
			termQuery := elastic.NewTermsQuery("daily_latest", true)
			boolQuery = boolQuery.Must(termQuery)
		} else {
			// If we don't have an end_time filter, we use the day_latest filter
			termQuery := elastic.NewTermsQuery("day_latest", true)
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
func (backend ES2Backend) getFiltersQueryForDeepReport(reportId string,
	filters map[string][]string) *elastic.BoolQuery {

	utils.DeDupFilters(filters)
	typeQuery := elastic.NewTypeQuery(mappings.DocType)

	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(typeQuery)

	idsQuery := elastic.NewIdsQuery(mappings.DocType)
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
		controlIds := strings.Join(filters["control"], "|")
		var nestedControlQuery *elastic.NestedQuery

		controlQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.controls.id:/(%s)/", controlIds))
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
