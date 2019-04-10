package relaxting

import (
	"fmt"

	"github.com/sirupsen/logrus"

	"time"

	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"golang.org/x/net/context"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/stringutils"
)

//GetStatsSummary - Report #16
func (backend ES2Backend) GetStatsSummary(filters map[string][]string) (*stats.ReportSummary, error) {
	client, err := backend.ES2Client()
	var reportMeta stats.ReportSummary
	var stats stats.Stats
	reportMeta.Stats = &stats

	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return &reportMeta, err
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return &reportMeta, errors.Wrap(err, "GetStatsSummary")
	}

	filtQuery := backend.getFiltersQuery(filters, true, true)

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "skipped"))

	//we have nodeUUIDTermsQSize set to 1 because we don't need to return the actual values.
	//this works for node_uuid because it's unique to the report_id. we will use when we compute reportMeta.Stats.Nodes (below)
	nodeUUIDTermsQSize := 1
	nodeUUIDTerms := elastic.NewTermsAggregation().Field("node_uuid").Size(nodeUUIDTermsQSize)
	platformTerms := elastic.NewTermsAggregation().Field("platform.name").Size(reporting.ESize)
	environmentTerms := elastic.NewTermsAggregation().Field("environment").Size(reporting.ESize)

	profilesTerms := elastic.NewTermsAggregation().Field("profiles.profile").Size(reporting.ESize)
	profilesNested := elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("profiles_shas", profilesTerms)

	searchSource := elastic.NewSearchSource().
		Aggregation("passed", passedFilter).
		Aggregation("failed", failedFilter).
		Aggregation("skipped", skippedFilter).
		Aggregation("nodes", nodeUUIDTerms).
		Aggregation("platforms", platformTerms).
		Aggregation("environment", environmentTerms).
		Aggregation("profiles", profilesNested).
		Query(filtQuery).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return &reportMeta, errors.Wrap(err, "GetStatsSummary unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetStatsSummary query")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Size(1).
		Do(context.Background())

	if err != nil {
		return nil, err
	}
	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetStatsSummary searchresult aggs")

	if environments, found := searchResult.Aggregations.Terms("environment"); found {
		stats.Environments = int32(len(environments.Buckets))
	}
	if nodes, found := searchResult.Aggregations.Terms("nodes"); found {
		//we need to do this because the number of nodes can get huge and we only need the node count
		//by setting the terms query for node_id to 1, we won't return any values but then we can just
		//use nodes.SumOfOtherDocCount which works well because node_uuid is unique per node and therefore
		stats.Nodes = int64(len(nodes.Buckets)) + nodes.SumOfOtherDocCount

	}
	if platforms, found := searchResult.Aggregations.Terms("platforms"); found {
		stats.Platforms = int32(len(platforms.Buckets))
	}

	/* the logic for summary status is:
	if any fail => overall fail
	if all skip => overall skip
	otherwise pass
	if there are no reports, then the status is simply unknown
	*/
	if failedResult, found := searchResult.Aggregations.Filter("failed"); found && (failedResult.DocCount > 0) {
		reportMeta.Status = "failed"
	} else if passedResult, found := searchResult.Aggregations.Filter("passed"); found && (passedResult.DocCount > 0) {
		reportMeta.Status = "passed"
	} else if skippedResult, found := searchResult.Aggregations.Filter("skipped"); found && (skippedResult.DocCount > 0) {
		reportMeta.Status = "skipped"
	} else {
		reportMeta.Status = "unknown"
	}

	if profilesAggResult, found := searchResult.Aggregations.Nested("profiles"); found {
		if profiles, found := profilesAggResult.Aggregations.Terms("profiles_shas"); found {
			if len(filters["profile_id"]) == 0 {
				stats.Profiles = int32(len(profiles.Buckets))
			} else {
				numberOfProfiles := 0
				for _, profileBucket := range profiles.Buckets {
					_, profile_id := rightSplit(string(profileBucket.KeyNumber), "|")
					if stringutils.SliceContains(filters["profile_id"], profile_id) {
						numberOfProfiles++
					}
				}
				stats.Profiles = int32(numberOfProfiles)
			}
		}
	}
	reportMeta.Stats = &stats
	return &reportMeta, nil
}

func (backend ES2Backend) GetStatsFailures(reportTypes []string,
	size int,
	filters map[string][]string) (stats.Failures, error) {
	var statsTopFailures stats.Failures

	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()

	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return statsTopFailures, err
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return statsTopFailures, errors.Wrap(err, "GetStatsFailures")
	}

	filtQuery := backend.getFiltersQuery(filters, true, true)

	//TODO - we can and should parallelize these!
	for _, reportType := range reportTypes {
		switch reportType {
		case "profile":
			_, err = backend.getStatsProfilesFailures(esIndex, client, &statsTopFailures, size, filters, filtQuery)
		case "platform":
			_, err = backend.getStatsPlatformsFailures(esIndex, client, &statsTopFailures, size, filters, filtQuery)
		case "control":
			esIndex, err = GetEsIndex(filters, false, false)
			if err != nil {
				return statsTopFailures, errors.Wrap(err, "GetStatsFailures")
			}
			filtQuery := backend.getFiltersQuery(filters, false, true)
			_, err = backend.getStatsControlsFailures(esIndex, client, &statsTopFailures, size, filters, filtQuery)
		case "environment":
			_, err = backend.getStatsEnvironmentsFailures(esIndex, client, &statsTopFailures, size, filters, filtQuery)
		default:
			return statsTopFailures, &utils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}

		if err != nil {
			return statsTopFailures, errors.Wrap(err, "GetStatsFailures")
		}
	}

	return statsTopFailures, nil
}

func (backend ES2Backend) GetStatsSummaryNodes(filters map[string][]string) (*stats.NodeSummary, error) {
	var summary stats.NodeSummary

	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()

	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return &summary, err
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		logrus.Errorf("GetEsIndex, error(%s) calling IndexDates, using index: %s", err.Error(), CompDailySumIndexPrefix)
		return &summary, err
	}

	filtQuery := backend.getFiltersQuery(filters, true, true)

	// aggs
	agg_compliant := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.total", 0)).
			Should(elastic.NewTermQuery("controls_sums.skipped.total", 0),
				elastic.NewRangeQuery("controls_sums.passed.total").Gt(0)))

	agg_skipped := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.total", 0)).
			Must(elastic.NewTermQuery("controls_sums.passed.total", 0)).
			Must(elastic.NewRangeQuery("controls_sums.skipped.total").Gt(0)))

	agg_uncompliant := elastic.NewFilterAggregation().
		Filter(elastic.NewRangeQuery("controls_sums.failed.total").Gt(0))

	agg_high_risk := elastic.NewFilterAggregation().
		Filter(elastic.NewRangeQuery("controls_sums.failed.critical").Gt(0))

	agg_medium_risk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.critical", 0)).
			Must(elastic.NewRangeQuery("controls_sums.failed.major").Gt(0)))

	agg_low_risk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.critical", 0)).
			Must(elastic.NewTermQuery("controls_sums.failed.major", 0)).
			Must(elastic.NewRangeQuery("controls_sums.failed.minor").Gt(0)))

	searchSource := elastic.NewSearchSource().
		Aggregation("compliant", agg_compliant).
		Aggregation("skipped", agg_skipped).
		Aggregation("noncompliant", agg_uncompliant).
		Aggregation("high_risk", agg_high_risk).
		Aggregation("medium_risk", agg_medium_risk).
		Aggregation("low_risk", agg_low_risk).
		Query(filtQuery).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return &summary, errors.Wrap(err, "GetStatsSummaryNodes unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetStatsSummaryNodes query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"aggregations").
		Do(context.Background())

	if err != nil {
		return &summary, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetStatsSummaryNodes - search results aggs")
	logrus.Debugf("GetStatsSummaryNodes got results in %d milliseconds\n", searchResult.TookInMillis)

	singleBucket, found := searchResult.Aggregations.Filter("compliant")
	if found {
		summary.Compliant = int32(singleBucket.DocCount)
	}

	singleBucket, found = searchResult.Aggregations.Filter("skipped")
	if found {
		summary.Skipped = int32(singleBucket.DocCount)
	}

	singleBucket, found = searchResult.Aggregations.Filter("noncompliant")
	if found {
		summary.Noncompliant = int32(singleBucket.DocCount)
	}

	singleBucket, found = searchResult.Aggregations.Filter("high_risk")
	if found {
		summary.HighRisk = int32(singleBucket.DocCount)
	}

	singleBucket, found = searchResult.Aggregations.Filter("medium_risk")
	if found {
		summary.MediumRisk = int32(singleBucket.DocCount)
	}

	singleBucket, found = searchResult.Aggregations.Filter("low_risk")
	if found {
		summary.LowRisk = int32(singleBucket.DocCount)
	}

	return &summary, nil
}

//GetStatsSummaryControls - Report #9
func (backend ES2Backend) GetStatsSummaryControls(
	filters map[string][]string, size int) (*stats.ControlsSummary, error) {

	logrus.Debugf("GetStatsSummaryControls started\n")

	aggregatedComplianceSummary := new(stats.ControlsSummary)

	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return aggregatedComplianceSummary, err
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return nil, errors.Wrap(err, "GetStatsSummaryControls")
	}

	filtQuery := backend.getFiltersQuery(filters, true, true)

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("failed", elastic.NewSumAggregation().Field("controls_sums.failed.total")).
		Aggregation("passed", elastic.NewSumAggregation().Field("controls_sums.passed.total")).
		Aggregation("skipped", elastic.NewSumAggregation().Field("controls_sums.skipped.total")).
		Aggregation("major", elastic.NewSumAggregation().Field("controls_sums.failed.major")).
		Aggregation("minor", elastic.NewSumAggregation().Field("controls_sums.failed.minor")).
		Aggregation("critical", elastic.NewSumAggregation().Field("controls_sums.failed.critical")).
		Size(0)

	//this is awesome! we can print out the entire query that we've constructed.. really nice for debugging.
	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "GetStatsSummaryControls unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetStatsSummaryControls query searchSource")

	searchResult, err := client.Search().SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"aggregations").
		Do(context.Background())

	if err != nil {
		logrus.Warnf("%s", err)
		return aggregatedComplianceSummary, err
	}
	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetStatsSummaryControls searchresult aggs")
	logrus.Debugf("GetStatsSummaryControls got %d control failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	if searchResult.Aggregations != nil && len(searchResult.Aggregations) > 0 {
		passed, _ := searchResult.Aggregations.Sum("passed")
		aggregatedComplianceSummary.Passed = int32(*passed.Value)

		failed, _ := searchResult.Aggregations.Sum("failed")
		aggregatedComplianceSummary.Failures = int32(*failed.Value)

		critical, _ := searchResult.Aggregations.Sum("critical")
		aggregatedComplianceSummary.Criticals = int32(*critical.Value)

		major, _ := searchResult.Aggregations.Sum("major")
		aggregatedComplianceSummary.Majors = int32(*major.Value)

		minor, _ := searchResult.Aggregations.Sum("minor")
		aggregatedComplianceSummary.Minors = int32(*minor.Value)

		skipped, _ := searchResult.Aggregations.Sum("skipped")
		aggregatedComplianceSummary.Skipped = int32(*skipped.Value)
	}

	return aggregatedComplianceSummary, nil
}

//GetControlListStatsByProfileId across nodes
func (backend ES2Backend) GetControlListStatsByProfileId(profileId string, from int, size int,
	filters map[string][]string, sort_field string, sort_asc bool) ([]*stats.ControlStats, error) {

	controlStats := make([]*stats.ControlStats, 0)
	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return controlStats, err
	}

	esIndex, err := GetEsIndex(filters, false, false)
	if err != nil {
		return controlStats, errors.Wrap(err, "GetAllReports unable to get index dates")
	}

	filterQuery := backend.getFiltersQuery(filters, false, true)

	profileIdQuery := elastic.NewTermQuery("profiles.sha256", profileId)

	reportIdsAndProfileIdQuery := elastic.NewBoolQuery()
	reportIdsAndProfileIdQuery = reportIdsAndProfileIdQuery.Must(profileIdQuery)

	passedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "skipped"))

	//controls
	totals := elastic.NewTermsAggregation().Field("profiles.controls.id")

	//TODO !!! - Sort and Size shown below will not work here it's accuracy is not guaranteed as it's an agg
	if sort_field == "control-name" {
		totals.Order("_term", sort_asc)
	}
	totals.Size(size)

	totals.SubAggregation("passed", passedFilterAgg)
	totals.SubAggregation("failed", failedFilterAgg)
	totals.SubAggregation("skipped", skippedFilterAgg)

	controls := elastic.NewNestedAggregation().Path("profiles.controls")
	controls.SubAggregation("totals", totals)
	controls.SubAggregation("passed", passedFilterAgg)
	controls.SubAggregation("failed", failedFilterAgg)
	controls.SubAggregation("skipped", skippedFilterAgg)

	profilesFilter := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.sha256", profileId))
	profilesFilter.SubAggregation("controls", controls)

	profilesAgg := elastic.NewNestedAggregation().Path("profiles")
	profilesAgg.SubAggregation("profiles_filter", profilesFilter)

	searchSource := elastic.NewSearchSource().
		Query(filterQuery).
		Aggregation("profiles", profilesAgg).
		From(from).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "GetControlListStatsByProfileId unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetControlListStatsByProfileId query")

	searchResult, err := client.Search().
		Index(esIndex).
		FilterPath("took,aggregations").
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "GetControlListStatsByProfileId - search results controls")

	logrus.Debugf("GetControlListStatsByProfileId got %d control failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	var profilesAggResult, profilesFilterAggResult, controlsAggResult *elastic.AggregationSingleBucket

	profilesAggResult, _ = searchResult.Aggregations.Nested("profiles")
	if profilesAggResult != nil {
		profilesFilterAggResult, _ = profilesAggResult.Aggregations.Filter("profiles_filter")
	}
	if profilesFilterAggResult != nil {
		controlsAggResult, _ = profilesFilterAggResult.Aggregations.Nested("controls")
	}
	if controlsAggResult != nil {
		totalsAgg, _ := controlsAggResult.Aggregations.Terms("totals")
		if totalsAgg != nil {
			controlMetaMap, err := backend.getControlsMetadata(profileId)
			if err != nil {
				return nil, err
			}
			for _, bucket := range totalsAgg.Buckets {
				controlID := string(bucket.KeyNumber)
				controlMeta := controlMetaMap[controlID]
				passedCount, _ := bucket.Filter("passed")
				failedCount, _ := bucket.Filter("failed")
				skippedCount, _ := bucket.Filter("skipped")
				statSummary := stats.ControlStats{
					Control: controlID,
					Passed:  int32(passedCount.DocCount),
					Failed:  int32(failedCount.DocCount),
					Skipped: int32(skippedCount.DocCount),
					Impact:  controlMeta.Impact,
					Title:   controlMeta.Title,
				}
				controlStats = append(controlStats, &statSummary)
			}
		}

	}
	return controlStats, nil

}

//GetProfileListWithAggregatedComplianceSummaries - Report #6
func (backend ES2Backend) GetProfileListWithAggregatedComplianceSummaries(
	filters map[string][]string, size int32) ([]*stats.ProfileList, error) {

	aggregatedComplianceSummaries := make([]*stats.ProfileList, 0)

	for filterName, filterValue := range filters {
		logrus.Debugf("filter: name=>%s value=>%s\n", filterName, filterValue)
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return aggregatedComplianceSummaries, err
	}

	esIndex, err := GetEsIndex(filters, true, false)
	if err != nil {
		return nil, errors.Wrap(err, "GetStatsSummaryControls")
	}

	filterQuery := backend.getFiltersQuery(filters, true, true)

	termsQuery := elastic.NewTermsAggregation().
		Field("profiles.profile").
		Size(int(size))
	termsQuery.SubAggregation("failures", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.total"))
	termsQuery.SubAggregation("passed", elastic.NewSumAggregation().
		Field("profiles.controls_sums.passed.total"))
	termsQuery.SubAggregation("skipped", elastic.NewSumAggregation().
		Field("profiles.controls_sums.skipped.total"))
	termsQuery.SubAggregation("major", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.major"))
	termsQuery.SubAggregation("minor", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.minor"))
	termsQuery.SubAggregation("critical", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.critical"))

	aggs := elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("totals", termsQuery)

	searchSource := elastic.NewSearchSource().
		Query(filterQuery).
		Aggregation("profiles", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "GetProfileListWithAggregatedComplianceSummaries unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "GetProfileListWithAggregatedComplianceSummaries query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"aggregations.profiles.totals.buckets.key",
			"aggregations.profiles.totals.buckets").
		Do(context.Background())

	if err != nil {
		logrus.Warnf("%s", err)
		return aggregatedComplianceSummaries, err
	}

	logrus.Debugf("GetProfileListWithAggregatedComplianceSummaries got %d control failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	outermostAgg, _ := searchResult.Aggregations.Nested("profiles")
	if outermostAgg != nil {
		totalsAgg, _ := outermostAgg.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				profile_name, profile_id := rightSplit(string(bucket.KeyNumber), "|") // bucket.KeyNumber

				//if we have a profile_id filter and if our result set doesn't contain it, skip it
				if profilesFilterArray, found := filters["profile_id"]; found {
					if !stringutils.SliceContains(profilesFilterArray, profile_id) {
						continue
					}
				}
				sumFailures, _ := bucket.Aggregations.Sum("failures")
				sumPassed, _ := bucket.Aggregations.Sum("passed")
				sumSkipped, _ := bucket.Aggregations.Sum("skipped")
				sumMajors, _ := bucket.Aggregations.Sum("major")
				sumMinors, _ := bucket.Aggregations.Sum("minor")
				sumCriticals, _ := bucket.Aggregations.Sum("critical")

				summary := stats.ProfileList{
					Name:      profile_name,
					Id:        profile_id,
					Failures:  int32(*sumFailures.Value),
					Passed:    int32(*sumPassed.Value),
					Skipped:   int32(*sumSkipped.Value),
					Majors:    int32(*sumMajors.Value),
					Minors:    int32(*sumMinors.Value),
					Criticals: int32(*sumCriticals.Value),
				}
				aggregatedComplianceSummaries = append(aggregatedComplianceSummaries, &summary)
			}
		}
	}
	return aggregatedComplianceSummaries, nil
}

func (backend ES2Backend) getStatsProfilesFailures(esIndex string, client *elastic.Client,
	statsTopFailures *stats.Failures,
	size int,
	filters map[string][]string,
	filtQuery *elastic.BoolQuery) (*stats.Failures, error) {

	//aggs
	aggs := elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("totals", elastic.NewTermsAggregation().
			Field("profiles.profile").
			Size(size).
			Order("failures", false).
			SubAggregation("failures", elastic.NewFilterAggregation().
				Filter(elastic.NewRangeQuery("profiles.controls_sums.failed.total").Gt(0))))

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("profiles", aggs).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getStatsProfilesFailures unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getStatsProfilesFailures query")

	searchResult, err := client.Search().
		Index(esIndex).
		FilterPath(
			"took",
			"aggregations.profiles.totals.buckets.key",
			"aggregations.profiles.totals.buckets.failures").
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "getStatsProfilesFailures - search results")

	logrus.Debugf("getStatsProfilesFailures got %d Profiles Failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	outermostAgg, _ := searchResult.Aggregations.Terms("profiles")
	if outermostAgg != nil {
		totalsAgg, _ := outermostAgg.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				failures, _ := bucket.Aggregations.Filter("failures")
				count := int(failures.DocCount)
				if count > 0 {
					profile_name, profile_id := rightSplit(string(bucket.KeyNumber), "|")
					profileStatSummary := stats.FailureSummary{Name: profile_name, Id: profile_id, Failures: int32(count)}
					statsTopFailures.Profiles = append(statsTopFailures.Profiles, &profileStatSummary)
				}
			}
		}
	}
	return statsTopFailures, nil
}

func (reporting ES2Backend) getStatsPlatformsFailures(esIndex string, client *elastic.Client,
	statsTopFailures *stats.Failures,
	size int,
	filters map[string][]string,
	filtQuery *elastic.BoolQuery) (*stats.Failures, error) {

	//aggs
	aggs := elastic.NewTermsAggregation().
		Field("platform.name").
		Size(size).
		Order("failures", false).
		SubAggregation("failures",
			elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed")))

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("platforms", aggs).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getStatsPlatformsFailures unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getStatsPlatformsFailures query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"aggregations.platforms.buckets",
			"aggregations.platforms.buckets.failures.value").
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "getStatsPlatformsFailures - search results aggs")

	outermostAgg, _ := searchResult.Aggregations.Terms("platforms")
	if outermostAgg != nil {
		for _, bucket := range outermostAgg.Buckets {
			nodes := int(bucket.DocCount)
			if nodes > 0 {
				failuresAgg, _ := bucket.Aggregations.Filter("failures")
				failuresCount := int(failuresAgg.DocCount)
				if failuresCount > 0 {
					statSummary := stats.FailureSummary{Name: string(bucket.KeyNumber), Failures: int32(failuresCount)}
					statsTopFailures.Platforms = append(statsTopFailures.Platforms, &statSummary)
				}
			}
		}
	}
	return statsTopFailures, nil
}

func (backend ES2Backend) getStatsControlsFailures(esIndex string, client *elastic.Client,
	statsTopFailures *stats.Failures,
	size int,
	filters map[string][]string,
	filtQuery *elastic.BoolQuery) (*stats.Failures, error) {

	//aggs
	aggs := elastic.NewNestedAggregation().Path("profiles.controls").
		SubAggregation("inner", elastic.NewFilterAggregation().
			Filter(elastic.NewTermQuery("profiles.controls.status", "failed")).
			SubAggregation("totals", elastic.NewTermsAggregation().
				Field("profiles.controls.id").
				Size(size)))

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("profiles", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getStatsControlsFailures unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getStatsControlsFailures query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		Do(context.Background())

	if err != nil {
		//todo -handle this better.  we return it but then eat it! not tasty!
		logrus.Errorf("error in getStatsControlsFailures %v", err)
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "getStatsControlsFailures - search results aggs")

	logrus.Debugf("getStatsControlsFailures got %d control failures in %d milliseconds\n", searchResult.TotalHits(), searchResult.TookInMillis)

	outermostAgg, _ := searchResult.Aggregations.Nested("profiles")
	if outermostAgg != nil {
		inner, _ := outermostAgg.Aggregations.Filter("inner")
		totalsAgg, _ := inner.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				count := int(bucket.DocCount)
				if count > 0 {
					statSummary := stats.FailureSummary{Name: string(bucket.KeyNumber), Failures: int32(count)}
					statsTopFailures.Controls = append(statsTopFailures.Controls, &statSummary)
				}
			}
		}
	}
	return statsTopFailures, nil
}

//todo.. this function is very similar to getStatsPlatformFailures - should refactor to use one func and pass in type
func (backend ES2Backend) getStatsEnvironmentsFailures(esIndex string, client *elastic.Client,
	statsTopFailures *stats.Failures,
	size int,
	filters map[string][]string,
	filtQuery *elastic.BoolQuery) (*stats.Failures, error) {

	//aggs
	aggs := elastic.NewTermsAggregation().
		Field("environment").
		Size(size).
		Order("failures", false).
		SubAggregation("failures",
			elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed")))

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("environments", aggs).
		Size(1)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getStatsEnvironmentsFailures unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getStatsEnvironmentsFailures query")

	searchResult, err := client.Search().
		Index(esIndex).
		SearchSource(searchSource).
		FilterPath(
			"took",
			"aggregations.environments.buckets",
			"aggregations.environments.buckets.failures.value").
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult.Aggregations, "getStatsEnvironmentsFailures - search results aggs")

	outermostAgg, _ := searchResult.Aggregations.Terms("environments")
	if outermostAgg != nil {
		for _, bucket := range outermostAgg.Buckets {
			nodes := int(bucket.DocCount)
			if nodes > 0 {
				failuresAgg, _ := bucket.Aggregations.Filter("failures")
				failuresCount := int(failuresAgg.DocCount)
				if failuresCount > 0 {
					statSummary := stats.FailureSummary{Name: string(bucket.KeyNumber), Failures: int32(failuresCount)}
					statsTopFailures.Environments = append(statsTopFailures.Environments, &statSummary)
				}
			}
		}
	}
	return statsTopFailures, nil
}

func computeIndexDate(endTime string) (string, error) {
	var indexDate time.Time

	if len(endTime) > 0 {
		endTimeAsTime, err := time.Parse(time.RFC3339, endTime)
		if err != nil {
			return "", errors.New(fmt.Sprintf("computeIndexDate - could not parse end_time %s.", endTime))
		}

		indexDate = endTimeAsTime
	} else {
		indexDate = time.Now().UTC()
	}

	return indexDate.Format(time.RFC3339), nil
}
