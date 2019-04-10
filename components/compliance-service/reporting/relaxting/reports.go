package relaxting

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/golang/protobuf/ptypes"

	"io"

	"sort"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/components/compliance-service/utils"
	elastic "github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

const MaxScrollRecordSize = 10000

type Filter struct {
	Type   string
	Values []string
}

func (backend ES2Backend) GetNodeReportIds(esIndex string, filters map[string][]string) (map[string]string, error) {
	var nodeReport map[string]string
	var err error

	//switch esIndex {
	//case CompSumLatestIndexAccumulated:
	//	nodeReport, err = backend.getNodeReportIdsFromLatest(esIndex, filters)
	//default:
	//	nodeReport, err = backend.getNodeReportIdsFromTimeseries(esIndex, filters)
	//}
	//
	//if err != nil {
	//	return nodeReport, errors.Wrap(err, "GetNodeReportIds unable to retrieve report ids")
	//}

	nodeReport, err = backend.getNodeReportIdsFromTimeseries(esIndex, filters, true)
	if err != nil {
		return nodeReport, errors.Wrap(err, "GetNodeReportIds unable to retrieve report ids")
	}
	return nodeReport, nil
}

func (backend ES2Backend) getDocIdHits(esIndex string,
	searchSource *elastic.SearchSource) ([]*elastic.SearchHit, time.Duration, error) {
	defer util.TimeTrack(time.Now(), "getDocIdHits")
	var startT time.Time = time.Now()

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

func (backend ES2Backend) getNodeReportIdsFromTimeseries(esIndex string, filters map[string][]string, latestOnly bool) (map[string]string, error) {
	nodeReport := make(map[string]string, 0)
	useSummaryIndex := strings.Contains(esIndex, "-s-")
	boolQuery := backend.getFiltersQuery(filters, useSummaryIndex, latestOnly)

	// aggs
	aggs := elastic.NewTermsAggregation().Field("node_uuid").Size(reporting.ESize).
		SubAggregation("distinct", elastic.NewTopHitsAggregation().Size(1).
			FetchSource(false).
			Sort("end_time", false))

	client, err := backend.ES2Client()
	if err != nil {
		return nodeReport, errors.Wrap(err, "getNodeReportIdsFromTimeseries cannot connect to elasticsearch")
	}

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("nodes", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nodeReport, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getNodeReportIdsFromTimeseries query searchSource")

	searchResult, err := client.Search().
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.nodes.buckets.key",
			"aggregations.nodes.buckets.distinct.hits.hits._id").
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		logrus.Errorf("unable to getNodeReportIdsFromTimeseries %v", err)
		return nodeReport, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to complete search")
	}

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		outermostAgg, _ := searchResult.Aggregations.Terms("nodes")
		if outermostAgg != nil {
			for _, nodeBucket := range outermostAgg.Buckets {
				topHits, _ := nodeBucket.Aggregations.TopHits("distinct")
				nodeID := fmt.Sprintf("%s", nodeBucket.Key)
				for _, hit := range topHits.Hits.Hits {
					nodeReport[nodeID] = hit.Id
				}
			}
		}

		// When filtering by controls, we are reducing the array of report ids based on
		// another query on inspec_report documents where we have control ids
		if len(filters["control"]) > 0 {
			esIndex, err = GetEsIndex(filters, false, false)
			if err != nil {
				return nil, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to GetEsIndex")
			}
			filteredReportIds, err := backend.filterIdsByControl(esIndex, MapValues(nodeReport), filters["control"])
			if err != nil {
				return nodeReport, errors.Wrap(err, "getNodeReportIdsFromTimeseries unable to filter ids by control")
			}
			logrus.Debugf("getNodeReportIdsFromTimeseries control filtering, len(nodeReport)=%d, len(filteredNodeIds)=%d\n", len(nodeReport), len(filteredReportIds))

			// flipping map[nodeid][reportid] to map[reportid][nodeid] for quicker lookups, to avoid an expensive O(n^2) Contains(filteredReportIds, reportId) call
			reportNode := make(map[string]string, len(nodeReport))
			for nodeID, reportID := range nodeReport {
				reportNode[reportID] = nodeID
			}

			// filtering out the nodes for which the report id is no longer in filteredReportIds
			filteredNodeReport := make(map[string]string, len(filteredReportIds))
			for _, reportID := range filteredReportIds {
				filteredNodeReport[reportNode[reportID]] = reportID
			}
			nodeReport = filteredNodeReport
		}

		logrus.Debugf("getNodeReportIdsFromTimeseries returning %d report ids in %d milliseconds\n", len(nodeReport), searchResult.TookInMillis)

		return nodeReport, nil
	}

	logrus.Debugf("getNodeReportIdsFromTimeseries: No report ids for the given filters: %+v\n", filters)
	// no matching report IDs is not an error, just return an empty array
	return nodeReport, nil
}

func (backend ES2Backend) GetReportIds(esIndex string, filters map[string][]string) ([]string, error) {
	nodeReport, err := backend.GetNodeReportIds(esIndex, filters)
	reportIds := make([]string, len(nodeReport))
	if err != nil {
		return reportIds, errors.Wrap(err, "GetReportIds unable to get node report ids")
	}
	i := 0
	for _, reportID := range nodeReport {
		reportIds[i] = reportID
		i++
	}
	return reportIds, nil
}

//GetMinScanDate gets the date of the oldest scan in ES
func (backend ES2Backend) GetMinScanDate() (*time.Time, error) {
	complianceBirthday := time.Date(2017, time.January, 1, 0, 0, 0, 0, time.UTC)
	aggs := elastic.NewMinAggregation().Field("end_time").Format("yyyy-MM-dd")
	searchSource := elastic.NewSearchSource().
		Aggregation("min_date", aggs).
		Size(0)
	esIndex := ComplianceDailySumTwenty
	source, err := searchSource.Source()
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetMinScanDate query searchSource")

	client, err := backend.ES2Client()
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate cannot connect to ElasticSearch")
	}

	searchResult, err := client.Search().SearchSource(searchSource).
		Index(ComplianceDailySumTwenty).
		FilterPath("aggregations.min_date").
		Do(context.Background())

	if err != nil {
		logrus.Errorf("could not get early scan date %s", err)
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate unable to complete search")
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetMinScanDate - search results aggs")

	minDate, found := searchResult.Aggregations.Min("min_date")
	if !found {
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate unable to read min_date")
	}

	var dateAsString string
	err = json.Unmarshal(*minDate.Aggregations["value_as_string"], &dateAsString)
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate unable to Unmarshal min_date")
	}

	logrus.Debugf("GetMinScanDate: earliest scan date = %s", dateAsString)

	formatOfDate := "2006-01-02"
	earliestScanDate, err := time.Parse(formatOfDate, dateAsString)
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMinScanDate unable to Parse min_date")
	}
	return &earliestScanDate, nil
}

//GetMaxScanDate gets the date of the newest scan in ES
func (backend ES2Backend) GetMaxScanDate() (*time.Time, error) {
	complianceBirthday := time.Date(2017, time.January, 1, 0, 0, 0, 0, time.UTC)
	aggs := elastic.NewMaxAggregation().Field("end_time").Format("yyyy-MM-dd")
	searchSource := elastic.NewSearchSource().
		Aggregation("max_date", aggs).
		Size(0)

	esIndex := ComplianceDailySumTwenty
	source, err := searchSource.Source()
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetMaxScanDate query searchSource")

	client, err := backend.ES2Client()
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate cannot connect to ElasticSearch")
	}

	searchResult, err := client.Search().SearchSource(searchSource).
		Index(ComplianceDailySumTwenty).
		FilterPath("aggregations.max_date").
		Do(context.Background())

	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate unable to complete search")
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetMaxScanDate - search results aggs")

	maxDate, found := searchResult.Aggregations.Min("max_date")
	if !found {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate unable to read max_date")
	}

	var dateAsString string
	err = json.Unmarshal(*maxDate.Aggregations["value_as_string"], &dateAsString)
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate unable to Unmarshal max_date")
	}

	logrus.Debugf("newest scan date = %s", dateAsString)

	newestScanDate, err := time.Parse("2006-01-02", dateAsString)
	if err != nil {
		return &complianceBirthday, errors.Wrap(err, "GetMaxScanDate unable to Parse max_date")
	}
	return &newestScanDate, nil
}

func (backend ES2Backend) filterIdsByControl(esIndex string, ids, controls []string) ([]string, error) {
	idsQuery := elastic.NewIdsQuery(mappings.DocType)
	idsQuery.Ids(ids...)
	termsQuery := elastic.NewTermsQuery("profiles.controls.id", stringArrayToInterfaceArray(controls)...)
	reportIdsAndControlIdQuery := elastic.NewBoolQuery()

	reportIdsNestedQuery := elastic.NewNestedQuery("profiles.controls", reportIdsAndControlIdQuery.Must(idsQuery, termsQuery))

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

// GetAllReports returns all reports in a given timeframe
// TODO: support timeframe and pagination
func (backend *ES2Backend) GetAllReports(from int32, size int32, filters map[string][]string, sort_field string, sort_asc bool) ([]*reportingapi.Report, int64, error) {

	client, err := backend.ES2Client()

	if err != nil {
		return nil, 0, errors.Wrap(err, "GetAllReports cannot connect to ElasticSearch")
	}

	esIndex, err := GetEsIndex(filters, true, true)
	if err != nil {
		return nil, 0, errors.Wrap(err, "GetAllReports unable to get index dates")
	}

	//this one if the list report history for a node so we want all for this day
	//not just the lastest
	boolQuery := backend.getFiltersQuery(filters, true, false)

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"end_time",
		"status",
		"controls_sums")
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Sort(sort_field, sort_asc).
		Query(boolQuery).
		From(int(from)).
		Size(int(size))

	source, err := searchSource.Source()
	if err != nil {
		return nil, 0, errors.Wrap(err, "GetAllReports unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetAllReports query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source").
		Do(context.Background())

	if err != nil {
		return nil, 0, errors.Wrap(err, "GetAllReports unable to complete search")
	}

	logrus.Debugf("GetAllReports got %d reports in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)
	reports := make([]*reportingapi.Report, 0)
	// Here's how you iterate through results with full control over each step.
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			item := ESInSpecSummary{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &item)
				if err == nil {
					t := item.EndTime.Round(1 * time.Second)
					timestamp, _ := ptypes.TimestampProto(t)
					report := reportingapi.Report{
						Id:       hit.Id,
						NodeId:   item.NodeID,
						NodeName: item.NodeName,
						EndTime:  timestamp,
						Status:   item.Status,
					}
					report.Controls = convertToRSControlSummary(item.ControlsSums)
					reports = append(reports, &report)
				} else {
					logrus.Errorf("GetAllReports unmarshal error: %s", err.Error())
				}
			}
		}
		return reports, searchResult.TotalHits(), nil
	}

	logrus.Debug("Found no reports\n")
	return reports, 0, nil
}

// GetReport returns the information about a single report
func (backend *ES2Backend) GetReport(esIndex string, reportid string, filters map[string][]string) (reportingapi.Report, error) {
	var report reportingapi.Report

	client, err := backend.ES2Client()

	if err != nil {
		return report, errors.Wrap(err, "GetReport cannot connect to ElasticSearch")
	}

	var profileNameFilter, profileIDFilter, controlIDFilter string
	if len(filters["profile_name"]) == 1 {
		profileNameFilter = filters["profile_name"][0]
	} else if len(filters["profile_id"]) == 1 {
		profileIDFilter = filters["profile_id"][0]
	}
	// we only allow filtering by one control
	if len(filters["control"]) == 1 {
		controlIDFilter = filters["control"][0]
	}
	logrus.Debugf("GetReport for reportid=%s, filters=%+v", reportid, filters)

	var query elastic.Query

	if len(filters["projects"]) > 0 {
		boolQuery := elastic.NewBoolQuery()

		typeQuery := elastic.NewTypeQuery(mappings.DocType)
		boolQuery = boolQuery.Must(typeQuery)

		idsQuery := elastic.NewIdsQuery(mappings.DocType)
		idsQuery.Ids(reportid)
		boolQuery = boolQuery.Filter(idsQuery)

		termQuery := elastic.NewTermsQuery("projects", stringArrayToInterfaceArray(filters["projects"])...)
		boolQuery = boolQuery.Filter(termQuery)

		query = boolQuery
	} else {
		idsQuery := elastic.NewIdsQuery(mappings.DocType)
		idsQuery.Ids(reportid)
		query = idsQuery
	}

	searchSource := elastic.NewSearchSource().
		Query(query).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return report, errors.Wrap(err, "GetReport unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetReport query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._index",
			"hits.hits._id",
			"hits.hits._source",
		).
		Do(context.Background())

	if err != nil {
		return report, errors.Wrap(err, "GetReport unable to complete search")
	}

	logrus.Debugf("GetReport got %d reports in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	// we should only receive one value
	if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
		for _, hit := range searchResult.Hits.Hits {
			esInSpecReport := ESInSpecReport{}
			if hit.Source != nil {
				err := json.Unmarshal(*hit.Source, &esInSpecReport)
				if err == nil {
					// TODO: FIX Unmarshal error(json: cannot unmarshal array into Go struct field ESInSpecReportControl.results) and move
					// the read all profiles section here
				} else {
					logrus.Errorf("GetReport unmarshal error: %s", err.Error())
				}

				// read all profiles
				profiles := make([]*reportingapi.Profile, 0)
				for _, esInSpecReportProfileMin := range esInSpecReport.Profiles {

					if profileNameFilter != "" {
						if esInSpecReportProfileMin.Name != profileNameFilter {
							continue
						}
					}
					if profileIDFilter != "" {
						if esInSpecReportProfileMin.SHA256 != profileIDFilter {
							continue
						}
					}
					// we need to enrich the profile information Here
					logrus.Debugf("Determine profile: %s", esInSpecReportProfileMin.Name)
					esInSpecProfile, err := backend.GetProfile(esInSpecReportProfileMin.SHA256)
					if err != nil {
						logrus.Errorf("Could not get profile: %s", err.Error())
					}

					// TODO: extract mapping from here
					reportProfile := inspec.Profile{}
					reportProfile.Name = esInSpecProfile.Name
					reportProfile.CopyrightEmail = esInSpecProfile.CopyrightEmail
					reportProfile.Copyright = esInSpecProfile.Copyright
					reportProfile.Title = esInSpecProfile.Title
					reportProfile.Summary = esInSpecProfile.Summary
					reportProfile.Version = esInSpecProfile.Version
					reportProfile.Sha256 = esInSpecProfile.Sha256
					reportProfile.Status = esInSpecProfile.Status
					reportProfile.SkipMessage = esInSpecProfile.SkipMessage
					reportProfile.Status = esInSpecReportProfileMin.Status
					reportProfile.SkipMessage = esInSpecReportProfileMin.SkipMessage

					dependsHash := make(map[string]*ESInSpecReportDepends, len(esInSpecReportProfileMin.Depends))
					for _, esInSpecProfileDependency := range esInSpecReportProfileMin.Depends {
						depCopy := esInSpecProfileDependency // to prevent https://github.com/golang/go/issues/20725
						dependsHash[esInSpecProfileDependency.Name] = &depCopy
					}
					// Picking the report specific dependency info(status, skip_message) and adding it to the static info retrieved from comp-profiles
					for _, esInSpecProfileDep := range esInSpecProfile.Depends {
						if hash, ok := dependsHash[esInSpecProfileDep.Name]; ok {
							esInSpecProfileDep.Status = hash.Status
							esInSpecProfileDep.SkipMessage = hash.SkipMessage
						}
					}
					reportProfile.Dependencies = esInSpecProfile.Depends

					reportProfile.Controls = make([]inspec.Control, len(esInSpecReportProfileMin.Controls))
					// Creating a map of report control ids to avoid a n^2 complexity later on when we look for the matching profile control id
					profileControlsMap := make(map[string]*reportingapi.Control, len(esInSpecProfile.Controls))
					for _, control := range esInSpecProfile.Controls {
						profileControlsMap[control.Id] = control
					}

					convertedControls := make([]*reportingapi.Control, 0)
					// Enrich min report controls with profile metadata
					for _, reportControlMin := range esInSpecReportProfileMin.Controls {
						if controlIDFilter != "" {
							if reportControlMin.ID != controlIDFilter {
								logrus.Debugf("control filter %s requested. skipping control %s", controlIDFilter, reportControlMin.ID)
								continue
							}
						}
						// reportControlMin contains the controls with their report specific information(results, execution time, etc)
						// profileControl contains the static metadata of the control
						profileControl := profileControlsMap[reportControlMin.ID]

						if profileControl != nil {
							profileControl.Results = make([]*reportingapi.Result, 0)
							if reportControlMin.ID == profileControl.Id {
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
									Results:        minResults,
								}
								var jsonTags map[string]string
								tags, _ := json.Marshal(profileControl.Tags)
								json.Unmarshal(tags, jsonTags) // nolint: errcheck
								convertedControl.Tags = jsonTags
								var jsonRefs []*reportingapi.Ref
								refs, _ := json.Marshal(profileControl.Refs)
								json.Unmarshal(refs, jsonRefs) // nolint: errcheck
								convertedControl.Refs = jsonRefs
								// store controls to returned report
								convertedControls = append(convertedControls, &convertedControl)
							}
						}
					}

					// Sort convertedControls by Id
					sort.Slice(convertedControls, func(i, j int) bool {
						return convertedControls[i].Id < convertedControls[j].Id
					})

					// TODO: fix this (vj)
					// Name: control.Attribute
					convertedAttributes := []*reportingapi.Attribute{}

					convertedProfile := reportingapi.Profile{
						Name:           reportProfile.Name,
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
						SkipMessage:    reportProfile.SkipMessage,
					}
					profiles = append(profiles, &convertedProfile)
				}

				ipAddress := ""
				if esInSpecReport.IPAddress != nil {
					ipAddress = *esInSpecReport.IPAddress
				}
				timestamp, _ := ptypes.TimestampProto(esInSpecReport.EndTime)
				report = reportingapi.Report{
					Id:          hit.Id,
					NodeId:      esInSpecReport.NodeID,
					NodeName:    esInSpecReport.NodeName,
					Ipaddress:   ipAddress,
					Fqdn:        esInSpecReport.FQDN,
					Environment: esInSpecReport.Environment,
					Status:      esInSpecReport.Status,
					EndTime:     timestamp,
					Version:     esInSpecReport.InSpecVersion,
					Profiles:    profiles,
				}
				report.Statistics = &reportingapi.Statistics{
					Duration: esInSpecReport.Statistics.Duration,
				}
				report.Platform = &reportingapi.Platform{
					Name:    esInSpecReport.Platform.Name,
					Release: esInSpecReport.Platform.Release,
				}
			}
		}
		return report, nil
	}

	return report, utils.ProcessNotFound(nil, reportid)
}

/*
  getFiltersQuery - builds up an elasticsearch query filter based on the filters map that is passed in
  arguments: filters - is a map of filters that serve as the source for generated es query filters
             useSummaryIndex - specifies whether or not we are building this filter for use against the summary
                               or detail timeseries indices.  This is important since the nested mapping of the
                               profile field is named differently if we are referencing summary vs detail
             latestOnly - specifies whether or not we are only interested in retrieving only the latest report

*/
func (backend ES2Backend) getFiltersQuery(filters map[string][]string, useSummaryIndex bool, latestOnly bool) *elastic.BoolQuery {
	//todo - we can get rid of useSummaryIndex once we migrate the indices and have sha256 on all profiles

	var (
		endTime   string
		startTime string
	)

	typeQuery := elastic.NewTypeQuery(mappings.DocType)

	endTime = firstOrEmpty(filters["end_time"])
	startTime = firstOrEmpty(filters["start_time"])

	timeRangeQuery := elastic.NewRangeQuery("end_time")
	if len(startTime) > 0 {
		timeRangeQuery.Gte(startTime)
	}
	if len(endTime) > 0 {
		timeRangeQuery.Lte(endTime)
	}

	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(typeQuery)

	if len(startTime) > 0 || len(endTime) > 0 {
		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	if len(filters["environment"]) > 0 {
		termQuery := elastic.NewTermsQuery("environment", stringArrayToInterfaceArray(filters["environment"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["status"]) > 0 {
		termQuery := elastic.NewTermsQuery("status", stringArrayToInterfaceArray(filters["status"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["node_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("node_uuid", stringArrayToInterfaceArray(filters["node_id"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["node_name"]) > 0 {
		termQuery := elastic.NewTermsQuery("node_name", stringArrayToInterfaceArray(filters["node_name"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["platform"]) > 0 {
		termQuery := elastic.NewTermsQuery("platform.name", stringArrayToInterfaceArray(filters["platform"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_id"]) > 0 {
		ids := strings.Join(filters["profile_id"], "|")
		var nestedQuery *elastic.NestedQuery
		//todo -rdm now that we have harmonized sum and det, we can remove this if and just use the sha256.. nice!
		if useSummaryIndex {
			stringQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.profile:/.*\\|(%s)/", ids))
			nestedQuery = elastic.NewNestedQuery("profiles", stringQuery)
		} else {
			stringQuery := elastic.NewQueryStringQuery(fmt.Sprintf("profiles.sha256:(%s)", ids))
			nestedQuery = elastic.NewNestedQuery("profiles", stringQuery)
		}
		boolQuery = boolQuery.Must(nestedQuery)
	}

	if len(filters["role"]) > 0 {
		termQuery := elastic.NewTermsQuery("roles", stringArrayToInterfaceArray(filters["role"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["recipe"]) > 0 {
		termQuery := elastic.NewTermsQuery("recipes", stringArrayToInterfaceArray(filters["recipe"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["job_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("job_uuid", stringArrayToInterfaceArray(filters["job_id"])...)
		boolQuery = boolQuery.Must(termQuery)
	} else if latestOnly {
		//only if there is no job_id filter set, do we want the daily latest
		termQuery := elastic.NewTermsQuery("daily_latest", true)
		boolQuery = boolQuery.Must(termQuery)
	}

	return boolQuery
}
