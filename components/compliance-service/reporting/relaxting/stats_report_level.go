package relaxting

import (
	"fmt"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
)

func (depth *ReportDepth) getControlListStatsByProfileIdAggs(
	size int,
	sortField string,
	sortAsc bool) map[string]elastic.Aggregation {
	//there is no such thing as report level here because it's by profileId.. so min depth is profile.
	return nil
}

func (depth *ReportDepth) getControlListStatsByProfileIdResults(
	backend *ES2Backend,
	searchResult *elastic.SearchResult,
	profileId string) ([]*stats.ControlStats, error) {
	//there is no such thing as report level here because it's by profileId.. so min depth is profile.
	return nil, nil
}

//todo - this is almost identical to ProfileDepth::getProfileListWithAggregatedComplianceSummariesAggs - harmonize
func (depth *ReportDepth) getProfileListWithAggregatedComplianceSummariesAggs(
	filters map[string][]string,
	size int32) map[string]elastic.Aggregation {

	aggs := make(map[string]elastic.Aggregation)

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

	aggs["profiles"] = elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("totals", termsQuery)

	return aggs
}

//todo - this is almost identical to ProfileDepth::getProfileListWithAggregatedComplianceSummariesResults - harmonize
func (depth *ReportDepth) getProfileListWithAggregatedComplianceSummariesResults(aggRoot *elastic.SearchResult,
	filters map[string][]string) []*stats.ProfileList {

	aggregatedComplianceSummaries := make([]*stats.ProfileList, 0)

	outermostAgg, _ := aggRoot.Aggregations.Nested("profiles")
	if outermostAgg != nil {
		totalsAgg, _ := outermostAgg.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				profileName, profileId := rightSplit(string(bucket.KeyNumber), "|") // bucket.KeyNumber

				//if we have a profile_id filter and if our result set doesn't contain it, skip it
				if profilesFilterArray, found := filters["profile_id"]; found {
					if !stringutils.SliceContains(profilesFilterArray, profileId) {
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
					Name:      profileName,
					Id:        profileId,
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

	return aggregatedComplianceSummaries
}

func (depth *ReportDepth) getStatsTopFailuresAggs(
	size int,
	reportTypes []string) (map[string]elastic.Aggregation, error) {

	aggs := make(map[string]elastic.Aggregation)

	for _, reportType := range reportTypes {
		switch reportType {
		case "control":
			//todo -N.B. same as profile level - you got to har-mo-nize.. are you gonna do it?
			aggs["controls"] = elastic.NewNestedAggregation().
				Path("profiles.controls").
				SubAggregation("filtered_controls",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.controls.status", "failed")).
						SubAggregation("totals", elastic.NewTermsAggregation().
							Field("profiles.controls.id").
							Order("_count", false).
							Size(size)))
		case "environment":
			aggs["environments"] = elastic.NewTermsAggregation().
				Field("environment").
				Size(size).
				Order("failures", false).
				SubAggregation("failures",
					elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed")))
		case "platform":
			aggs["platforms"] = elastic.NewTermsAggregation().
				Field("platform.name").
				Size(size).
				Order("failures", false).
				SubAggregation("failures",
					elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed")))
		case "profile":
			aggs["profiles"] = elastic.NewNestedAggregation().Path("profiles").
				SubAggregation("totals", elastic.NewTermsAggregation().
					Field("profiles.profile").
					Size(size).
					Order("failures", false).
					SubAggregation("failures", elastic.NewFilterAggregation().
						Filter(elastic.NewRangeQuery("profiles.controls_sums.failed.total").Gt(0))))
		default:
			return aggs, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}

		if err != nil {
			return aggs, errors.Wrap(err, "GetStatsFailures")
		}
	}

	return aggs, nil
}

func (depth *ReportDepth) getStatsTopFailuresResult(
	aggRoot *elastic.SearchResult,
	reportTypes []string) (*stats.Failures, error) {

	topFailures := &stats.Failures{}

	for _, reportType := range reportTypes {
		switch reportType {
		case "control":
			depth.topFailuresResultControl(aggRoot, topFailures)
		case "environment":
			depth.topFailuresResultEnvironment(aggRoot, topFailures)
		case "platform":
			depth.topFailuresResultPlatform(aggRoot, topFailures)
		case "profile":
			depth.topFailuresResultProfile(aggRoot, topFailures)
		default:
			return topFailures, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}

	}
	return topFailures, nil
}

//todo - reduce duplicate code among the following top failure funcs
func (depth *ReportDepth) topFailuresResultProfile(aggRoot *elastic.SearchResult, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("profiles"); found {
		if totalsAgg, found := outermostAgg.Terms("totals"); found {
			for _, bucket := range totalsAgg.Buckets {
				failures, _ := bucket.Aggregations.Filter("failures")
				count := int(failures.DocCount)
				if count > 0 {
					profileName, profileId := rightSplit(string(bucket.KeyNumber), "|")
					profileStatSummary := stats.FailureSummary{
						Name:     profileName,
						Id:       profileId,
						Failures: int32(count)}
					topFailures.Profiles = append(topFailures.Profiles, &profileStatSummary)
				}
			}
		}
	}
}

func (depth *ReportDepth) topFailuresResultPlatform(aggRoot *elastic.SearchResult, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("platforms"); found {
		for _, bucket := range outermostAgg.Buckets {
			nodes := int(bucket.DocCount)
			if nodes > 0 {
				failuresAgg, _ := bucket.Aggregations.Filter("failures")
				failuresCount := int(failuresAgg.DocCount)
				if failuresCount > 0 {
					statSummary := stats.FailureSummary{
						Name:     string(bucket.KeyNumber),
						Failures: int32(failuresCount)}
					topFailures.Platforms = append(topFailures.Platforms, &statSummary)
				}
			}
		}
	}
}

func (depth *ReportDepth) topFailuresResultEnvironment(aggRoot *elastic.SearchResult, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("environments"); found {
		for _, bucket := range outermostAgg.Buckets {
			nodes := int(bucket.DocCount)
			if nodes > 0 {
				failuresAgg, _ := bucket.Aggregations.Filter("failures")
				failuresCount := int(failuresAgg.DocCount)
				if failuresCount > 0 {
					statSummary := stats.FailureSummary{
						Name:     string(bucket.KeyNumber),
						Failures: int32(failuresCount)}
					topFailures.Environments = append(topFailures.Environments, &statSummary)
				}
			}
		}
	}
}

func (depth *ReportDepth) topFailuresResultControl(aggRoot *elastic.SearchResult, topFailures *stats.Failures) {
	outermostAgg, _ := aggRoot.Aggregations.Nested("controls")
	if outermostAgg != nil {
		inner, _ := outermostAgg.Aggregations.Filter("filtered_controls")
		totalsAgg, _ := inner.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				count := int(bucket.DocCount)
				if count > 0 {
					statSummary := stats.FailureSummary{
						Name:     string(bucket.KeyNumber),
						Failures: int32(count)}
					topFailures.Controls = append(topFailures.Controls, &statSummary)
				}
			}
		}
	}
}

//GetStatsSummary - Report #16 - profile level
func (depth *ReportDepth) getStatsSummaryAggs() map[string]elastic.Aggregation {
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "skipped"))

	//We have nodeUUIDTermsQSize set to 1 because we don't need to return the actual values.
	// This works for node_uuid because it's unique to the report_id.
	// We will use when we compute reportSummary.Stats.Nodes (below)
	nodeUUIDTermsQSize := 1
	nodeUUIDTerms := elastic.NewTermsAggregation().Field("node_uuid").Size(nodeUUIDTermsQSize)
	platformTerms := elastic.NewTermsAggregation().Field("platform.name").Size(reporting.ESize)
	environmentTerms := elastic.NewTermsAggregation().Field("environment").Size(reporting.ESize)

	profilesTerms := elastic.NewTermsAggregation().Field("profiles.profile").Size(reporting.ESize)
	profilesNested := elastic.NewNestedAggregation().Path("profiles").
		SubAggregation("profiles_shas", profilesTerms)

	controlsCard := elastic.NewCardinalityAggregation().Field("profiles.controls.id")
	controlsNested := elastic.NewNestedAggregation().Path("profiles.controls").
		SubAggregation("control_count", controlsCard)

	aggs := make(map[string]elastic.Aggregation)
	aggs["passed"] = passedFilter
	aggs["failed"] = failedFilter
	aggs["skipped"] = skippedFilter
	aggs["nodes"] = nodeUUIDTerms
	aggs["platforms"] = platformTerms
	aggs["environment"] = environmentTerms
	aggs["profiles"] = profilesNested
	aggs["controls"] = controlsNested

	return aggs
}

func (depth *ReportDepth) getStatsSummaryResult(aggRoot *elastic.SearchResult) *stats.ReportSummary {
	summary := &stats.ReportSummary{Status: "unknown", Stats: &stats.Stats{}}

	if environments, found := aggRoot.Aggregations.Terms("environment"); found {
		summary.Stats.Environments = int32(len(environments.Buckets))
	}
	if nodes, found := aggRoot.Aggregations.Terms("nodes"); found {
		//we need to do this because the number of nodes can get huge and we only need the node count
		//by setting the terms query for node_id to 1, we won't return any values but then we can just
		//use nodes.SumOfOtherDocCount which works well because node_uuid is unique per node and therefore
		summary.Stats.NodesCnt = int32(len(nodes.Buckets)) + int32(nodes.SumOfOtherDocCount)
		summary.Stats.Nodes = int64(len(nodes.Buckets)) + nodes.SumOfOtherDocCount
	}
	if platforms, found := aggRoot.Aggregations.Terms("platforms"); found {
		summary.Stats.Platforms = int32(len(platforms.Buckets))
	}

	/* the logic for summary status is:
	if any fail => overall fail
	if all skip => overall skip
	otherwise pass
	if there are no reports, then the status is simply unknown
	*/
	if failedResult, found := aggRoot.Aggregations.Filter("failed"); found && (failedResult.DocCount > 0) {
		summary.Status = "failed"
	} else if passedResult, found := aggRoot.Aggregations.Filter("passed"); found &&
		(passedResult.DocCount > 0) {
		summary.Status = "passed"
	} else if skippedResult, found := aggRoot.Aggregations.Filter("skipped"); found &&
		(skippedResult.DocCount > 0) {
		summary.Status = "skipped"
	} else {
		summary.Status = "unknown"
	}

	if profilesAggResult, found := aggRoot.Aggregations.Nested("profiles"); found {
		if profiles, found := profilesAggResult.Aggregations.Terms("profiles_shas"); found {
			if len(depth.filters["profile_id"]) == 0 {
				summary.Stats.Profiles = int32(len(profiles.Buckets))
			} else {
				numberOfProfiles := 0
				for _, profileBucket := range profiles.Buckets {
					_, profileId := rightSplit(string(profileBucket.KeyNumber), "|")
					if stringutils.SliceContains(depth.filters["profile_id"], profileId) {
						numberOfProfiles++
					}
				}
				summary.Stats.Profiles = int32(numberOfProfiles)
			}
		}
	}

	if controlsAggResult, found := aggRoot.Aggregations.Nested("controls"); found {
		if controls, found := controlsAggResult.Aggregations.Cardinality("control_count"); found {
			summary.Stats.Controls = int32(*controls.Value)
		}
	}

	return summary
}

func (depth *ReportDepth) getStatsSummaryNodesAggs() map[string]elastic.Aggregation {
	aggCompliant := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery("status", "passed"))

	aggSkipped := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery("status", "skipped"))

	aggNoncompliant := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery("status", "failed"))

	aggHighRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewRangeQuery("controls_sums.failed.critical").Gt(0))

	aggMediumRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.critical", 0)).
			Must(elastic.NewRangeQuery("controls_sums.failed.major").Gt(0)))

	aggLowRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("controls_sums.failed.critical", 0)).
			Must(elastic.NewTermQuery("controls_sums.failed.major", 0)).
			Must(elastic.NewRangeQuery("controls_sums.failed.minor").Gt(0)))

	aggs := make(map[string]elastic.Aggregation)
	aggs["compliant"] = aggCompliant
	aggs["skipped"] = aggSkipped
	aggs["noncompliant"] = aggNoncompliant
	aggs["high_risk"] = aggHighRisk
	aggs["medium_risk"] = aggMediumRisk
	aggs["low_risk"] = aggLowRisk

	return aggs
}

func (depth *ReportDepth) getStatsSummaryNodesResult(aggRoot *elastic.SearchResult) *stats.NodeSummary {
	summary := &stats.NodeSummary{}

	singleBucket, found := aggRoot.Aggregations.Filter("compliant")
	if found {
		summary.Compliant = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("skipped")
	if found {
		summary.Skipped = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("noncompliant")
	if found {
		summary.Noncompliant = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("high_risk")
	if found {
		summary.HighRisk = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("medium_risk")
	if found {
		summary.MediumRisk = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("low_risk")
	if found {
		summary.LowRisk = int32(singleBucket.DocCount)
	}
	return summary
}

func (depth *ReportDepth) getStatsSummaryControlsAggs() map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)
	aggs["failed"] = elastic.NewSumAggregation().Field("controls_sums.failed.total")
	aggs["passed"] = elastic.NewSumAggregation().Field("controls_sums.passed.total")
	aggs["skipped"] = elastic.NewSumAggregation().Field("controls_sums.skipped.total")
	aggs["major"] = elastic.NewSumAggregation().Field("controls_sums.failed.major")
	aggs["minor"] = elastic.NewSumAggregation().Field("controls_sums.failed.minor")
	aggs["critical"] = elastic.NewSumAggregation().Field("controls_sums.failed.critical")

	return aggs
}

func (depth *ReportDepth) getStatsSummaryControlsResult(aggRoot *elastic.SearchResult) *stats.ControlsSummary {
	summary := &stats.ControlsSummary{}

	if aggRoot.Aggregations != nil && len(aggRoot.Aggregations) > 0 {
		passed, _ := aggRoot.Aggregations.Sum("passed")
		summary.Passed = int32(*passed.Value)

		failed, _ := aggRoot.Aggregations.Sum("failed")
		summary.Failures = int32(*failed.Value)

		critical, _ := aggRoot.Aggregations.Sum("critical")
		summary.Criticals = int32(*critical.Value)

		major, _ := aggRoot.Aggregations.Sum("major")
		summary.Majors = int32(*major.Value)

		minor, _ := aggRoot.Aggregations.Sum("minor")
		summary.Minors = int32(*minor.Value)

		skipped, _ := aggRoot.Aggregations.Sum("skipped")
		summary.Skipped = int32(*skipped.Value)
	}

	return summary
}

func (depth *ReportDepth) getQueryInfo() *QueryInfo {
	return depth.QueryInfo
}
