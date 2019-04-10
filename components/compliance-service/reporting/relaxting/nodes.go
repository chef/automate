package relaxting

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/golang/protobuf/ptypes"

	"strings"

	"sort"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

// TODO: header with amount of results
func (backend *ES2Backend) getAllNodes(from int32, size int32, filters map[string][]string,
	sortField string, sortAsc bool) ([]*reportingapi.Node, int64, error) {

	logrus.Debug("getAllNodes will retrieve all nodes")

	client, err := backend.ES2Client()

	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodes, cannot connect to Elasticsearch")
	}

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"node_name",
		"environment",
		"platform.name",
		"platform.release",
		"status",
		"report_uuid",
		"end_time",
		"controls_sums")

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodes unable to get index")
	}

	filterQuery := backend.getFiltersQuery(filters, true, true)

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(filterQuery).
		Sort(sortField, sortAsc).
		From(int(from)).
		Size(int(size))

	// Adding a second sort field when we don't sort by node name
	if sortField != "node_name.lower" {
		searchSource.Sort("node_name.lower", true)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodes unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getAllNodes query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source").
		Do(context.Background())

	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodes unable to complete search")
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
					latestReport.Status = item.Status
					timestamp, err := ptypes.TimestampProto(item.EndTime)
					if err != nil {
						logrus.Errorf("getAllNodes time error: %s", err.Error())
					} else {
						latestReport.EndTime = timestamp
					}
					node.LatestReport = &latestReport
					latestReport.Controls = convertToRSControlSummary(item.ControlsSums)
					nodes = append(nodes, &node)
				} else {
					logrus.Errorf("getAllNodes unmarshal error: %s", err.Error())
				}
			}
		}
		return nodes, searchResult.TotalHits(), nil
	}

	logrus.Debugf("Found no nodes\n")
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

// given a profile_id (in filters), return stats for all nodes that use said profile and just for that profile
// TODO: header with amount of results
// todo: harmonize this with getAllNodes and make it so that they both have one common goal... deep filtering
func (backend *ES2Backend) getAllNodesForProfile(from int32, size int32, filters map[string][]string,
	sortField string, sortAsc bool) ([]*reportingapi.Node, int64, error) {

	logrus.Debugf("getAllNodesForProfile will retrieve Node info for profile(s): %+v", filters["profile_id"])

	client, err := backend.ES2Client()

	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodesForProfile, cannot connect to ElasticSearch")
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodes")
	}

	filterQuery := backend.getFiltersQuery(filters, true, true)

	passedFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.passed.total")
	failedFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.failed.total")
	skippedFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.skipped.total")
	minorFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.failed.minor")
	majorFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.failed.major")
	criticalFilterAgg := elastic.NewSumAggregation().Field("profiles.controls_sums.failed.critical")

	totals := elastic.NewTermsAggregation().Field("profiles.profile")
	totals.SubAggregation("passed", passedFilterAgg)
	totals.SubAggregation("failed", failedFilterAgg)
	totals.SubAggregation("skipped", skippedFilterAgg)
	totals.SubAggregation("minor", minorFilterAgg)
	totals.SubAggregation("major", majorFilterAgg)
	totals.SubAggregation("critical", criticalFilterAgg)

	ids := strings.Join(filters["profile_id"], "|")
	profilesFilter := elastic.NewFilterAggregation().Filter(
		elastic.NewQueryStringQuery(
			fmt.Sprintf("profiles.profile:/.*\\|(%s)/", ids)))

	profilesFilter.SubAggregation("totals", totals)

	profilesAgg := elastic.NewNestedAggregation().Path("profiles")
	profilesAgg.SubAggregation("profiles_filter", profilesFilter)

	statusAgg := elastic.NewTermsAggregation().Field("status")
	statusAgg.SubAggregation("profiles", profilesAgg)

	platformNameAgg := elastic.NewTermsAggregation().Field("platform.name")
	platformNameAgg.SubAggregation("status", statusAgg)

	environmentAgg := elastic.NewTermsAggregation().Field("environment")
	environmentAgg.SubAggregation("platform_name", platformNameAgg)

	nodeNameAgg := elastic.NewTermsAggregation().Field("node_name")
	nodeNameAgg.SubAggregation("environment", environmentAgg)

	endTimeAgg := elastic.NewTermsAggregation().Field("end_time")
	endTimeAgg.SubAggregation("node_name", nodeNameAgg)

	bucketField := "report_uuid"

	uidAgg := elastic.NewTermsAggregation().Field(bucketField)
	uidAgg.SubAggregation("end_time", endTimeAgg)

	nodeUUIDAgg := elastic.NewTermsAggregation().Field("node_uuid")
	nodeUUIDAgg.SubAggregation("uid", uidAgg).Size(int(size))
	// ^ We can't paginate the aggregated output, so only size number of nodes will be returned. from is not used

	searchSource := elastic.NewSearchSource().
		//FetchSourceContext(fsc).
		Query(filterQuery).
		Aggregation("node_uuid", nodeUUIDAgg).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodesForProfile unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getAllNodesForProfile query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"aggregations").
		Do(context.Background())

	if err != nil {
		return nil, 0, errors.Wrap(err, "getAllNodesForProfile could not complete search")
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "getAllNodesForProfile searchresult aggs")

	nodes := make([]*reportingapi.Node, 0)

	nodeUUIDAggResult, _ := searchResult.Aggregations.Terms("node_uuid")
	if nodeUUIDAggResult != nil {
		for _, nodeUUIDBucket := range nodeUUIDAggResult.Buckets {
			node := reportingapi.Node{
				Id: string(nodeUUIDBucket.KeyNumber),
			}

			uidAggRes, _ := nodeUUIDBucket.Aggregations.Terms("uid")
			uidBucket := uidAggRes.Buckets[0]
			uid := string(uidBucket.KeyNumber)

			var latestReport reportingapi.LatestReportSummary
			latestReport.Id = uid

			endTimeAggRes, _ := uidBucket.Aggregations.Terms("end_time")
			endTimeBucket := endTimeAggRes.Buckets[0]

			etime, err := time.Parse(time.RFC3339, *endTimeBucket.KeyAsString)
			if err != nil {
				logrus.Errorf("getAllNodesForProfile unable to parse time: %s", err.Error())
			}
			timestamp, err := ptypes.TimestampProto(etime)
			if err != nil {
				logrus.Errorf("getAllNodesForProfile time error: %s", err.Error())
			} else {
				latestReport.EndTime = timestamp
			}
			latestReport.EndTime = timestamp

			nodeNameAggRes, _ := endTimeBucket.Aggregations.Terms("node_name")
			nodeNameBucket := nodeNameAggRes.Buckets[0]
			node.Name = string(nodeNameBucket.KeyNumber)

			environmentAggRes, _ := nodeNameBucket.Aggregations.Terms("environment")
			environmentBucket := environmentAggRes.Buckets[0]
			node.Environment = string(environmentBucket.KeyNumber)

			platformOSNameAggRes, _ := environmentBucket.Aggregations.Terms("platform_name")
			if len(platformOSNameAggRes.Buckets) == 0 {
				// prevents this bug https://github.com/chef/inspec/issues/2100
				// from panic-ing this code when the platform_name made it to ElasticSearch as null
				continue
			}
			platformOSNameBucket := platformOSNameAggRes.Buckets[0]
			var platform reportingapi.Platform
			platform.Name = string(platformOSNameBucket.KeyNumber)
			node.Platform = &platform

			statusAggRes, _ := platformOSNameBucket.Aggregations.Terms("status")
			statusBucket := statusAggRes.Buckets[0]

			latestReport.Status = string(statusBucket.KeyNumber)

			profilesNestedAggRes, _ := statusBucket.Aggregations.Nested("profiles")
			profilesFilterAggRes, _ := profilesNestedAggRes.Aggregations.Filter("profiles_filter")

			//now's when we get the stats
			totalsAgg, _ := profilesFilterAggRes.Terms("totals")
			if len(totalsAgg.Buckets) == 0 {
				continue
			}
			totalsBucket := totalsAgg.Buckets[0]
			failedValue, _ := totalsBucket.Sum("failed")
			passedValue, _ := totalsBucket.Sum("passed")
			skippedValue, _ := totalsBucket.Sum("skipped")
			minorValue, _ := totalsBucket.Sum("minor")
			majorValue, _ := totalsBucket.Sum("major")
			criticalValue, _ := totalsBucket.Sum("critical")

			passed := int32(*passedValue.Value)
			failed := int32(*failedValue.Value)
			skipped := int32(*skippedValue.Value)
			minor := int32(*minorValue.Value)
			major := int32(*majorValue.Value)
			critical := int32(*criticalValue.Value)

			// uh yeah....temporary hold over til everything gets converted
			// i had to do this b/c pointers were screaming at me
			var controlSummary reportingapi.ControlSummary
			var passedSum reportingapi.Total
			passedSum.Total = passed
			var skippedSum reportingapi.Total
			skippedSum.Total = skipped
			var failedSums reportingapi.Failed
			failedSums.Total = failed
			failedSums.Critical = critical
			failedSums.Minor = minor
			failedSums.Major = major
			controlSummary.Passed = &passedSum
			controlSummary.Skipped = &skippedSum
			controlSummary.Failed = &failedSums
			latestReport.Controls = &controlSummary
			latestReport.Status = computeStatus(failed, passed, skipped)

			latestReport.Controls.Total = skipped + passed + failed

			node.LatestReport = &latestReport

			logrus.Debugf("node %+v", node)
			nodes = append(nodes, &node)
		}
		// can't sort the nodes based on aggregation values so we are doing it in go
		sortNodes(nodes, sortField, sortAsc)

		return nodes, searchResult.TotalHits(), nil
	}

	logrus.Debugf("Found no nodes\n")
	return nodes, 0, nil
}

func sortNodes(nodes []*reportingapi.Node, sortField string, sortAsc bool) {
	switch sortField {
	case "node_name":
		sort.Slice(nodes, func(i, j int) bool {
			if sortAsc {
				return nodes[i].Name < nodes[j].Name
			}
			return nodes[i].Name > nodes[j].Name
		})
	case "environment":
		sort.Slice(nodes, func(i, j int) bool {
			if sortAsc {
				return nodes[i].Environment < nodes[j].Environment
			}
			return nodes[i].Environment > nodes[j].Environment
		})
	case "platform.name":
		sort.Slice(nodes, func(i, j int) bool {
			if sortAsc {
				if strings.Compare(nodes[i].Platform.Name, nodes[j].Platform.Name) == 0 {
					return nodes[i].Platform.Release < nodes[j].Platform.Release
				}
				return nodes[i].Platform.Name < nodes[j].Platform.Name
			}
			if strings.Compare(nodes[i].Platform.Name, nodes[j].Platform.Name) == 0 {
				return nodes[i].Platform.Release > nodes[j].Platform.Release
			}
			return nodes[i].Platform.Name > nodes[j].Platform.Name
		})
	case "controls_sums.failed.total":
		sort.Slice(nodes, func(i, j int) bool {
			if sortAsc {
				return nodes[i].LatestReport.Controls.Failed.Total < nodes[j].LatestReport.Controls.Failed.Total
			}
			return nodes[i].LatestReport.Controls.Failed.Total > nodes[j].LatestReport.Controls.Failed.Total
		})
	default:
		logrus.Errorf("sortNodes, unsupported sort field %s\n", sortField)
	}
}

//GetNodes - list all of the nodes or all nodes for a profile-id
// TODO: header with amount of results
func (backend *ES2Backend) GetNodes(from int32, size int32, filters map[string][]string,
	sortField string, sortAsc bool) ([]*reportingapi.Node, int64, error) {

	if len(filters["profile_id"]) == 0 {
		return backend.getAllNodes(from, size, filters, sortField, sortAsc)
	}
	return backend.getAllNodesForProfile(from, size, filters, sortField, sortAsc)
}

func (backend *ES2Backend) GetNode(node_uuid string) (*reportingapi.Node, error) {
	node := new(reportingapi.Node)

	client, err := backend.ES2Client()

	if err != nil {
		return node, errors.Wrap(err, "GetNode, cannot connect to ElasticSearch")
	}

	//todo - important for migrations - use ComplianceDailyRepTwenty where daily_latest == true
	//esIndex := CompRepLatestIndexAccumulated  //was this before migrations
	esIndex := ComplianceDailyRepTwenty
	idQuery := elastic.NewTermQuery("node_uuid", node_uuid)
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

	//todo - important can we get rid of CompRepLatestIndexAccumulated and CompSumLatestIndexAccumulated?
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
	return nil, utils.ProcessNotFound(nil, node_uuid)
}
