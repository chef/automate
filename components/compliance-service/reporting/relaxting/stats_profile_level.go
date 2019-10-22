package relaxting

import (
	"fmt"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
)

func (depth *ProfileDepth) getControlListStatsByProfileIdAggs(
	size int,
	sortField string,
	sortAsc bool) map[string]elastic.Aggregation {

	aggs := make(map[string]elastic.Aggregation)

	passedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilterAgg := elastic.NewFilterAggregation().Filter(
		elastic.NewTermQuery("profiles.controls.status", "skipped"))

	//controls
	totals := elastic.NewTermsAggregation().Field("profiles.controls.id")

	//TODO !!! - Sort and Size shown below will not work here it's accuracy is not guaranteed as it's an agg
	if sortField == "control-name" {
		totals.Order("_term", sortAsc)
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

	aggs["controls"] = controls

	return depth.wrap(aggs)
}

func (depth *ProfileDepth) getControlListStatsByProfileIdResults(
	backend *ES2Backend,
	searchResult *elastic.SearchResult,
	profileId string) ([]*stats.ControlStats, error) {

	controlStats := make([]*stats.ControlStats, 0)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if controlsAggResult, found := aggRoot.Aggregations.Nested("controls"); found {
			totalsAgg, _ := controlsAggResult.Aggregations.Terms("totals")
			if totalsAgg != nil && len(totalsAgg.Buckets) > 0 {
				//todo - let's not do this anymore.. we now have impact and title in the report.. use it.. don't make another call!
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
	}
	return controlStats, nil
}

//todo - this is almost identical to ReportDepth::getProfileListWithAggregatedComplianceSummariesAggs - harmonize
func (depth *ProfileDepth) getProfileListWithAggregatedComplianceSummariesAggs(
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

	aggs["totals"] = termsQuery

	return depth.wrap(aggs)
}

//todo - this is almost identical to ReportDepth::getProfileListWithAggregatedComplianceSummariesResults - harmonize
func (depth *ProfileDepth) getProfileListWithAggregatedComplianceSummariesResults(
	searchResult *elastic.SearchResult,
	filters map[string][]string) []*stats.ProfileList {

	aggregatedComplianceSummaries := make([]*stats.ProfileList, 0)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		logrus.Info(aggRoot)
		totalsAgg, _ := aggRoot.Terms("totals")
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

func (depth *ProfileDepth) getStatsTopFailuresAggs(size int, reportTypes []string) (map[string]elastic.Aggregation, error) {
	aggs := make(map[string]elastic.Aggregation)

	for _, reportType := range reportTypes {
		switch reportType {
		case "control":
			//todo -N.B. same as report level - you got to har-mo-nize.. are you gonna do it?
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
			//todo -N.B. almost same as platforms on this level (see just below) - you got to har-mo-nize.. are you gonna do it?
			aggs["environments"] = elastic.NewTermsAggregation().
				Field("profiles.status").
				Include("failed").
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewReverseNestedAggregation().
						SubAggregation("top",
							elastic.NewTermsAggregation().
								Field("environment"). //only diff with respect to platforms (below)
								Order("_count", false).
								Size(size)))
		case "platform":
			//todo -N.B. almost same as environments on this level (see just above) - you got to har-mo-nize.. are you gonna do it?
			aggs["platforms"] = elastic.NewTermsAggregation().
				Field("profiles.status").
				Include("failed").
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewReverseNestedAggregation().
						SubAggregation("top",
							elastic.NewTermsAggregation().
								Field("platform.name"). //only diff with respect to environments (above)
								Order("_count", false).
								Size(size)))
		case "profile":
			aggs["profiles"] = elastic.NewTermsAggregation().
				Field("profiles.status").
				Include("failed").
				Order("failures", false). //todo do we really need this? methinks not.. because are just counting docs and the sorting happens below on _count - confirm and get rid of this line asap
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewTermsAggregation().
						Field("profiles.name"). //only diff with respect to platforms (below)
						Size(size).
						Order("_count", false))
		default:
			return aggs, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}

		if err != nil {
			return aggs, errors.Wrap(err, "GetStatsFailures")
		}
	}
	return depth.wrap(aggs), nil
}

func (depth *ProfileDepth) getStatsTopFailuresResult(
	searchResult *elastic.SearchResult,
	reportTypes []string) (*stats.Failures, error) {

	topFailures := &stats.Failures{}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
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
	}
	return topFailures, nil
}

//todo - reduce duplicate code among the following top failure funcs
func (depth *ProfileDepth) topFailuresResultProfile(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("profiles"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.Terms("topsy"); found {
				for _, statBucket := range topsy.Buckets {
					count := int(statBucket.DocCount)
					if count > 0 {
						statSummary := stats.FailureSummary{Name: string(statBucket.KeyNumber), Failures: int32(count)}
						topFailures.Profiles = append(topFailures.Profiles, &statSummary)
					}
				}

			}
		}
	}
}

func (depth *ProfileDepth) topFailuresResultPlatform(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("platforms"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.ReverseNested("topsy"); found {
				if toplist, found := topsy.Terms("top"); found {
					for _, statBucket := range toplist.Buckets {
						count := int(statBucket.DocCount)
						if count > 0 {
							statSummary := stats.FailureSummary{Name: string(statBucket.KeyNumber), Failures: int32(count)}
							topFailures.Platforms = append(topFailures.Platforms, &statSummary)
						}
					}

				}
			}
		}
	}
}

func (depth *ProfileDepth) topFailuresResultEnvironment(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("environments"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.ReverseNested("topsy"); found {
				if toplist, found := topsy.Terms("top"); found {
					for _, statBucket := range toplist.Buckets {
						count := int(statBucket.DocCount)
						if count > 0 {
							statSummary := stats.FailureSummary{Name: string(statBucket.KeyNumber), Failures: int32(count)}
							topFailures.Environments = append(topFailures.Environments, &statSummary)
						}
					}

				}
			}
		}
	}
}

func (depth *ProfileDepth) topFailuresResultControl(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Nested("controls"); found {
		inner, _ := outermostAgg.Aggregations.Filter("filtered_controls")
		totalsAgg, _ := inner.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				count := int(bucket.DocCount)
				if count > 0 {
					statSummary := stats.FailureSummary{Name: string(bucket.KeyNumber), Failures: int32(count)}
					topFailures.Controls = append(topFailures.Controls, &statSummary)
				}
			}
		}
	}
}

//GetStatsSummary - Report #16 - profile level
func (depth *ProfileDepth) getStatsSummaryAggs() map[string]elastic.Aggregation {
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "skipped"))

	//we have nodeUUIDTermsQSize set to 1 because we don't need to return the actual values.
	//this works for node_uuid because it's unique to the report_id. we will use when we compute reportMeta.Stats.Nodes (below)
	nodeUUIDTermsQSize := 1
	nodeUUIDTerms := elastic.NewTermsAggregation().Field("node_uuid").Size(nodeUUIDTermsQSize)
	platformTerms := elastic.NewTermsAggregation().Field("platform.name").Size(reporting.ESize)
	environmentTerms := elastic.NewTermsAggregation().Field("environment").Size(reporting.ESize)

	profilesTerms := elastic.NewTermsAggregation().Field("profiles.profile").Size(1)

	controlsCard := elastic.NewCardinalityAggregation().Field("profiles.controls.id")
	controlsNested := elastic.NewNestedAggregation().Path("profiles.controls").
		SubAggregation("control_count", controlsCard)

	aggs := make(map[string]elastic.Aggregation)
	aggs["passed"] = passedFilter
	aggs["failed"] = failedFilter
	aggs["skipped"] = skippedFilter
	aggs["nodes"] = elastic.NewReverseNestedAggregation().SubAggregation("nodes", nodeUUIDTerms)
	aggs["platforms"] = elastic.NewReverseNestedAggregation().SubAggregation("platforms", platformTerms)
	aggs["environment"] = elastic.NewReverseNestedAggregation().SubAggregation("environment", environmentTerms)
	aggs["profiles"] = profilesTerms
	aggs["controls"] = controlsNested

	return depth.wrap(aggs)
}

func (depth *ProfileDepth) getStatsSummaryResult(searchResult *elastic.SearchResult) *stats.ReportSummary {
	reportSummary := &stats.ReportSummary{Status: "unknown", Stats: &stats.Stats{}}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if environmentsRevNest, found := aggRoot.Aggregations.ReverseNested("environment"); found {
			if environments, found := environmentsRevNest.Aggregations.Terms("environment"); found {
				reportSummary.Stats.Environments = int32(len(environments.Buckets))
			}
		}
		if nodeRevNest, found := aggRoot.Aggregations.ReverseNested("nodes"); found {
			if nodes, found := nodeRevNest.Aggregations.Terms("nodes"); found {
				//we need to do this because the number of nodes can get huge and we only need the node count
				//by setting the terms query for node_id to 1, we won't return any values but then we can just
				//use nodes.SumOfOtherDocCount which works well because node_uuid is unique per node and therefore
				reportSummary.Stats.NodesCnt = int32(len(nodes.Buckets)) + int32(nodes.SumOfOtherDocCount)
				reportSummary.Stats.Nodes = int64(len(nodes.Buckets)) + nodes.SumOfOtherDocCount
			}
		}
		if platformsRevNest, found := aggRoot.Aggregations.ReverseNested("platforms"); found {
			if platforms, found := platformsRevNest.Aggregations.Terms("platforms"); found {
				reportSummary.Stats.Platforms = int32(len(platforms.Buckets))
			}
		}

		/* the logic for summary status is:
		if any fail => overall fail
		if all skip => overall skip
		otherwise pass
		if there are no reports, then the status is simply unknown
		*/
		if failedResult, found := aggRoot.Aggregations.Filter("failed"); found && (failedResult.DocCount > 0) {
			reportSummary.Status = "failed"
		} else if passedResult, found := aggRoot.Aggregations.Filter("passed"); found && (passedResult.DocCount > 0) {
			reportSummary.Status = "passed"
		} else if skippedResult, found := aggRoot.Aggregations.Filter("skipped"); found && (skippedResult.DocCount > 0) {
			reportSummary.Status = "skipped"
		} else {
			reportSummary.Status = "unknown"
		}

		if profiles, found := aggRoot.Aggregations.Terms("profiles"); found {
			if len(depth.filters["profile_id"]) == 0 {
				reportSummary.Stats.Profiles = int32(len(profiles.Buckets))
			} else {
				numberOfProfiles := 0
				for _, profileBucket := range profiles.Buckets {
					_, profileId := rightSplit(string(profileBucket.KeyNumber), "|")
					if stringutils.SliceContains(depth.filters["profile_id"], profileId) {
						numberOfProfiles++
					}
				}
				reportSummary.Stats.Profiles = int32(numberOfProfiles)
			}
		}

		if controlsAggResult, found := aggRoot.Aggregations.Nested("controls"); found {
			if controls, found := controlsAggResult.Aggregations.Cardinality("control_count"); found {
				reportSummary.Stats.Controls = int32(*controls.Value)
			}
		}
	}

	return reportSummary
}

func (depth *ProfileDepth) getStatsSummaryNodesAggs() map[string]elastic.Aggregation {
	aggCompliant := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("profiles.controls_sums.failed.total", 0)).
			Should(elastic.NewTermQuery("profiles.controls_sums.skipped.total", 0),
				elastic.NewRangeQuery("profiles.controls_sums.passed.total").Gt(0)))

	aggSkipped := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("profiles.controls_sums.failed.total", 0)).
			Must(elastic.NewTermQuery("profiles.controls_sums.passed.total", 0)).
			Must(elastic.NewRangeQuery("profiles.controls_sums.skipped.total").Gt(0)))

	aggNoncompliant := elastic.NewFilterAggregation().
		Filter(elastic.NewRangeQuery("profiles.controls_sums.failed.total").Gt(0))

	aggHighRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewRangeQuery("profiles.controls_sums.failed.critical").Gt(0))

	aggMediumRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("profiles.controls_sums.failed.critical", 0)).
			Must(elastic.NewRangeQuery("profiles.controls_sums.failed.major").Gt(0)))

	aggLowRisk := elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermQuery("profiles.controls_sums.failed.critical", 0)).
			Must(elastic.NewTermQuery("profiles.controls_sums.failed.major", 0)).
			Must(elastic.NewRangeQuery("profiles.controls_sums.failed.minor").Gt(0)))

	aggs := make(map[string]elastic.Aggregation)
	aggs["compliant"] = aggCompliant
	aggs["skipped"] = aggSkipped
	aggs["noncompliant"] = aggNoncompliant
	aggs["high_risk"] = aggHighRisk
	aggs["medium_risk"] = aggMediumRisk
	aggs["low_risk"] = aggLowRisk

	return depth.wrap(aggs)
}

func (depth *ProfileDepth) getStatsSummaryNodesResult(searchResult *elastic.SearchResult) *stats.NodeSummary {
	summary := &stats.NodeSummary{}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
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
	}
	return summary
}

func (depth *ProfileDepth) getStatsSummaryControlsAggs() map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)
	aggs["failed"] = elastic.NewSumAggregation().Field("profiles.controls_sums.failed.total")
	aggs["passed"] = elastic.NewSumAggregation().Field("profiles.controls_sums.passed.total")
	aggs["skipped"] = elastic.NewSumAggregation().Field("profiles.controls_sums.skipped.total")
	aggs["major"] = elastic.NewSumAggregation().Field("profiles.controls_sums.failed.major")
	aggs["minor"] = elastic.NewSumAggregation().Field("profiles.controls_sums.failed.minor")
	aggs["critical"] = elastic.NewSumAggregation().Field("profiles.controls_sums.failed.critical")

	return depth.wrap(aggs)
}

func (depth *ProfileDepth) getStatsSummaryControlsResult(searchResult *elastic.SearchResult) *stats.ControlsSummary {
	summary := &stats.ControlsSummary{}
	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
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
	}

	return summary
}

func (depth *ProfileDepth) wrap(aggs map[string]elastic.Aggregation) map[string]elastic.Aggregation {
	filteredProfileAgg := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.sha256", depth.filters["profile_id"][0]))
	profilesAgg := elastic.NewNestedAggregation().SubAggregation("filtered_profiles", filteredProfileAgg).Path("profiles")
	for aggName, agg := range aggs {
		filteredProfileAgg.SubAggregation(aggName, agg)
	}
	wrappedAggs := make(map[string]elastic.Aggregation)
	wrappedAggs["profiles"] = profilesAgg

	return wrappedAggs
}

func (depth *ProfileDepth) unwrap(aggs *elastic.Aggregations) (*elastic.AggregationSingleBucket, bool) {
	if outerProfilesAggResult, found := aggs.Nested("profiles"); found {
		return outerProfilesAggResult.Aggregations.Nested("filtered_profiles")
	}
	return nil, false
}

func (depth *ProfileDepth) getQueryInfo() *QueryInfo {
	return depth.QueryInfo
}
