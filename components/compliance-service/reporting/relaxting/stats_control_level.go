package relaxting

import (
	"fmt"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func (depth *ControlDepth) getControlListStatsByProfileIdAggs(
	size int,
	sortField string,
	sortAsc bool) map[string]elastic.Aggregation {

	aggs := make(map[string]elastic.Aggregation)

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "skipped"))

	impactTerms := elastic.NewTermsAggregation().Field("profiles.controls.impact").Size(1).
		SubAggregation("passed", passedFilter).
		SubAggregation("failed", failedFilter).
		SubAggregation("skipped", skippedFilter)

	titleTerms := elastic.NewTermsAggregation().Field("profiles.controls.title").Size(1).
		SubAggregation("impact", impactTerms)
	controlTerms := elastic.NewTermsAggregation().Field("profiles.controls.id").Size(1).
		SubAggregation("title", titleTerms)

	aggs["control"] = controlTerms

	return depth.wrap(aggs)
}

func (depth *ControlDepth) getControlListStatsByProfileIdResults(
	backend *ES2Backend,
	searchResult *elastic.SearchResult,
	profileId string) ([]*stats.ControlStats, error) {

	controlStats := make([]*stats.ControlStats, 0)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if controlAggResult, found := aggRoot.Aggregations.Terms("control"); found {
			//there can only be one
			controlBucket := controlAggResult.Buckets[0]
			controlId := controlBucket.Key.(string)
			if titlesAggResult, found := controlBucket.Aggregations.Terms("title"); found {
				//there can only be one
				titleBucket := titlesAggResult.Buckets[0]
				title := titleBucket.Key.(string)
				if impactAggResult, found := titleBucket.Aggregations.Terms("impact"); found {
					//there can only be one
					impactBucket := impactAggResult.Buckets[0]
					impactAsNumber, ok := impactBucket.Key.(float64)
					if !ok {
						//todo - what should we do in this case? as it is now, we will just move forward and set it to low risk
						logrus.Errorf("could not convert the value of impact: %v, to a float!", impactBucket)
					}

					passedCount, _ := impactBucket.Filter("passed")
					failedCount, _ := impactBucket.Filter("failed")
					skippedCount, _ := impactBucket.Filter("skipped")
					statSummary := stats.ControlStats{
						Control: controlId,
						Passed:  int32(passedCount.DocCount),
						Failed:  int32(failedCount.DocCount),
						Skipped: int32(skippedCount.DocCount),
						Impact:  float32(impactAsNumber),
						Title:   title,
					}
					controlStats = append(controlStats, &statSummary)
				}
			}
		}
	}
	return controlStats, nil
}

//todo - Where in A2 UI is this being called.  it now works deeply but not sure if we need it.
//this is the list that appears on the "profiles" tab
func (depth *ControlDepth) getProfileListWithAggregatedComplianceSummariesAggs(filters map[string][]string, size int32) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "skipped"))

	profileSha := elastic.NewTermsAggregation().Field("profiles.sha256").Size(int(size))
	profileAgg := elastic.NewReverseNestedAggregation().Path("profiles").
		SubAggregation("profile-info",
			elastic.NewTermsAggregation().
				Field("profiles.name").
				Size(int(size)).SubAggregation("sha", profileSha))

	impactTerms := elastic.NewTermsAggregation().Field("profiles.controls.impact").Size(1).
		SubAggregation("compliant", passedFilter).
		SubAggregation("noncompliant", failedFilter).
		SubAggregation("skipped", skippedFilter).
		SubAggregation("profile", profileAgg)

	aggs["impact"] = impactTerms

	return depth.wrap(aggs)
}

//todo - Where in A2 UI is this being called.  it now works deeply but not sure if we need it.
// this needs to be complete for deep!
//this is the list that appears on the "profiles" tab
func (depth *ControlDepth) getProfileListWithAggregatedComplianceSummariesResults(searchResult *elastic.SearchResult,
	filters map[string][]string) []*stats.ProfileList {

	aggregatedComplianceSummaries := make([]*stats.ProfileList, 0)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if impactBuckets, found := aggRoot.Aggregations.Terms("impact"); found {
			summary := stats.ProfileList{}
			//there can only be one
			impact := impactBuckets.Buckets[0]
			if failedResult, found := impact.Aggregations.Filter("noncompliant"); found {
				summary.Failures = int32(failedResult.DocCount)

				impactAsNumber, ok := impact.Key.(float64)
				if !ok {
					//todo - what should we do in this case? as it is now, we will just move forward and set it to low risk
					logrus.Errorf("could not convert the value of impact: %v, to a float!", impact)
				}

				if impactAsNumber < 0.4 {
					summary.Minors = summary.Failures
				} else if impactAsNumber < 0.7 {
					summary.Majors = summary.Failures
				} else {
					summary.Criticals = summary.Failures
				}
			}
			if passedResult, found := impact.Aggregations.Filter("compliant"); found {
				summary.Passed = int32(passedResult.DocCount)
			}
			if skippedResult, found := impact.Aggregations.Filter("skipped"); found {
				summary.Skipped = int32(skippedResult.DocCount)
			}
			if profileResult, found := impact.Aggregations.ReverseNested("profile"); found {
				if profileInfoResult, found := profileResult.Terms("profile-info"); found {
					profileInfoBucket := profileInfoResult.Buckets[0]
					name := profileInfoBucket.KeyNumber
					summary.Name = string(name)

					if profileShaResult, found := profileInfoBucket.Terms("sha"); found {
						sha := profileShaResult.Buckets[0].KeyNumber
						summary.Id = string(sha)
					}
				}
			}
			aggregatedComplianceSummaries = append(aggregatedComplianceSummaries, &summary)
		}
	}

	return aggregatedComplianceSummaries
}

func (depth *ControlDepth) getStatsTopFailuresAggs(size int, reportTypes []string) (map[string]elastic.Aggregation, error) {
	aggs := make(map[string]elastic.Aggregation)

	for _, reportType := range reportTypes {
		switch reportType {
		case "control":
			aggs["controls"] = elastic.NewTermsAggregation().
				Field("profiles.controls.status").
				Include("failed").
				Order("failures", false). //todo do we really need this? methinks not.. because are just counting docs and the sorting happens below on _count - confirm and get rid of this line asap
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewTermsAggregation().
						Field("profiles.controls.id"). //only diff with respect to platforms (below)
						Size(size).
						Order("_count", false))
		case "environment":
			aggs["environments"] = elastic.NewTermsAggregation().
				Field("profiles.controls.status").
				Include("failed").
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewReverseNestedAggregation().
						SubAggregation("top",
							elastic.NewTermsAggregation().
								Field("environment"). //only diff with respect to platforms (below)
								Order("_count", false).
								Size(size)))
		case "platform":
			aggs["platforms"] = elastic.NewTermsAggregation().
				Field("profiles.controls.status").
				Include("failed").
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewReverseNestedAggregation().
						SubAggregation("top",
							elastic.NewTermsAggregation().
								Field("platform.name"). //only diff with respect to environments (above)
								Order("_count", false).
								Size(size)))
		case "profile":
			aggs["profiles"] = elastic.NewTermsAggregation().
				Field("profiles.controls.status").
				Include("failed").
				Order("failures", false). //todo do we really need this? methinks not.. because are just counting docs and the sorting happens below on _count - confirm and get rid of this line asap
				SubAggregation("failures",
					elastic.NewFilterAggregation().
						Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))).
				SubAggregation("topsy",
					elastic.NewReverseNestedAggregation().Path("profiles").
						SubAggregation("top",
							elastic.NewTermsAggregation().
								Field("profiles.name"). //only diff with respect to environments (above)
								Order("_count", false).
								Size(size)))
		default:
			return aggs, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}

		if err != nil {
			return aggs, errors.Wrap(err, "GetStatsFailures")
		}
	}

	return depth.wrap(aggs), nil
}

func (depth *ControlDepth) getStatsTopFailuresResult(
	searchResult *elastic.SearchResult,
	reportTypes []string) (*stats.Failures, error) {

	topFailures := &stats.Failures{}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		logrus.Info(aggRoot)
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

func (depth *ControlDepth) topFailuresResultProfile(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("profiles"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.ReverseNested("topsy"); found {
				if topList, found := topsy.Aggregations.Terms("top"); found {
					for _, statBucket := range topList.Buckets {
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
}

func (depth *ControlDepth) topFailuresResultPlatform(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("platforms"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.ReverseNested("topsy"); found {
				if topList, found := topsy.Terms("top"); found {
					for _, statBucket := range topList.Buckets {
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

func (depth *ControlDepth) topFailuresResultEnvironment(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("environments"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.ReverseNested("topsy"); found {
				if topList, found := topsy.Terms("top"); found {
					for _, statBucket := range topList.Buckets {
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

func (depth *ControlDepth) topFailuresResultControl(aggRoot *elastic.AggregationSingleBucket, topFailures *stats.Failures) {
	if outermostAgg, found := aggRoot.Aggregations.Terms("controls"); found {
		for _, bucket := range outermostAgg.Buckets {
			if topsy, found := bucket.Aggregations.Terms("topsy"); found {
				for _, statBucket := range topsy.Buckets {
					count := int(statBucket.DocCount)
					if count > 0 {
						statSummary := stats.FailureSummary{Name: string(statBucket.KeyNumber), Failures: int32(count)}
						topFailures.Controls = append(topFailures.Controls, &statSummary)
					}
				}

			}
		}
	}
}

//GetStatsSummary - Report #16 - profile level
func (depth *ControlDepth) getStatsSummaryAggs() map[string]elastic.Aggregation {
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "skipped"))
	controlsCard := elastic.NewCardinalityAggregation().Field("profiles.controls.id")

	//we have nodeUUIDTermsQSize set to 1 because we don't need to return the actual values.
	//this works for node_uuid because it's unique to the report_id. we will use when we compute reportMeta.Stats.Nodes (below)
	nodeUUIDTermsQSize := 1
	nodeUUIDTerms := elastic.NewTermsAggregation().Field("node_uuid").Size(nodeUUIDTermsQSize)
	platformTerms := elastic.NewTermsAggregation().Field("platform.name").Size(reporting.ESize)
	environmentTerms := elastic.NewTermsAggregation().Field("environment").Size(reporting.ESize)

	profilesTerms := elastic.NewTermsAggregation().Field("profiles.profile").Size(1)

	aggs := make(map[string]elastic.Aggregation)
	aggs["passed"] = passedFilter
	aggs["failed"] = failedFilter
	aggs["skipped"] = skippedFilter
	aggs["nodes"] = elastic.NewReverseNestedAggregation().SubAggregation("nodes", nodeUUIDTerms)
	aggs["platforms"] = elastic.NewReverseNestedAggregation().SubAggregation("platforms", platformTerms)
	aggs["environment"] = elastic.NewReverseNestedAggregation().SubAggregation("environment", environmentTerms)
	aggs["profiles"] = elastic.NewReverseNestedAggregation().Path("profiles").SubAggregation("profiles", profilesTerms)
	aggs["control_count"] = controlsCard

	return depth.wrap(aggs)
}

func (depth *ControlDepth) getStatsSummaryResult(searchResult *elastic.SearchResult) *stats.ReportSummary {
	summary := &stats.ReportSummary{Status: "unknown", Stats: &stats.Stats{}}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if environmentsRevNest, found := aggRoot.Aggregations.ReverseNested("environment"); found {
			if environments, found := environmentsRevNest.Aggregations.Terms("environment"); found {
				summary.Stats.Environments = int32(len(environments.Buckets))
			}
		}
		if nodeRevNest, found := aggRoot.Aggregations.ReverseNested("nodes"); found {
			if nodes, found := nodeRevNest.Aggregations.Terms("nodes"); found {
				//we need to do this because the number of nodes can get huge and we only need the node count
				//by setting the terms query for node_id to 1, we won't return any values but then we can just
				//use nodes.SumOfOtherDocCount which works well because node_uuid is unique per node and therefore
				summary.Stats.NodesCnt = int32(len(nodes.Buckets)) + int32(nodes.SumOfOtherDocCount)
				summary.Stats.Nodes = int64(len(nodes.Buckets)) + nodes.SumOfOtherDocCount
			}
		}
		if platformsRevNest, found := aggRoot.Aggregations.ReverseNested("platforms"); found {
			if platforms, found := platformsRevNest.Aggregations.Terms("platforms"); found {
				summary.Stats.Platforms = int32(len(platforms.Buckets))
			}
		}

		/* the logic for summary status is:
		if any fail => overall fail
		if all skip => overall skip
		otherwise pass
		if there are no reports, then the status is simply unknown
		*/
		if failedResult, found := aggRoot.Aggregations.Filter("failed"); found && (failedResult.DocCount > 0) {
			summary.Status = "failed"
		} else if passedResult, found := aggRoot.Aggregations.Filter("passed"); found && (passedResult.DocCount > 0) {
			summary.Status = "passed"
		} else if skippedResult, found := aggRoot.Aggregations.Filter("skipped"); found && (skippedResult.DocCount > 0) {
			summary.Status = "skipped"
		} else {
			summary.Status = "unknown"
		}

		if profilesAggResult, found := aggRoot.Aggregations.Nested("profiles"); found {
			if profiles, found := profilesAggResult.Aggregations.Terms("profiles"); found {
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

		if controls, found := aggRoot.Aggregations.Cardinality("control_count"); found {
			summary.Stats.Controls = int32(*controls.Value)
		}
	}
	return summary
}

func (depth *ControlDepth) getStatsSummaryNodesAggs() map[string]elastic.Aggregation {
	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "skipped"))

	impactTerms := elastic.NewTermsAggregation().Field("profiles.controls.impact").Size(1).
		SubAggregation("compliant", passedFilter).
		SubAggregation("noncompliant", failedFilter).
		SubAggregation("skipped", skippedFilter)

	aggs := make(map[string]elastic.Aggregation)
	aggs["impact"] = impactTerms

	return depth.wrap(aggs)
}

func (depth *ControlDepth) getStatsSummaryNodesResult(searchResult *elastic.SearchResult) *stats.NodeSummary {
	summary := &stats.NodeSummary{}

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if impactBuckets, found := aggRoot.Aggregations.Terms("impact"); found && len(impactBuckets.Buckets) > 0 {
			//there can only be one
			impact := impactBuckets.Buckets[0]
			if failedResult, found := impact.Aggregations.Filter("noncompliant"); found {
				summary.Noncompliant = int32(failedResult.DocCount)

				impactAsNumber, ok := impact.Key.(float64)
				if !ok {
					//todo - what should we do in this case? as it is now, we will just move forward and set it to low risk
					logrus.Errorf("could not convert the value of impact: %v, to a float!", impact)
				}

				if impactAsNumber < 0.4 {
					summary.LowRisk = summary.Noncompliant
				} else if impactAsNumber < 0.7 {
					summary.MediumRisk = summary.Noncompliant
				} else {
					summary.HighRisk = summary.Noncompliant
				}
			}
			if passedResult, found := impact.Aggregations.Filter("compliant"); found {
				summary.Compliant = int32(passedResult.DocCount)
			}
			if skippedResult, found := impact.Aggregations.Filter("skipped"); found {
				summary.Skipped = int32(skippedResult.DocCount)
			}
		}
	}
	return summary
}

func (depth *ControlDepth) getStatsSummaryControlsAggs() map[string]elastic.Aggregation {
	/**
	this one is identical to getStatsSummaryNodesAggs except for the field names that get called in getStatsSummaryControlsResult.
	We will, therefore, use the one for nodes and then when we harvest the resultSet in getStatsSummaryControlsResult, we'll translate to ControlsSummary
	*/
	return depth.getStatsSummaryNodesAggs()
}

func (depth *ControlDepth) getStatsSummaryControlsResult(searchResult *elastic.SearchResult) *stats.ControlsSummary {
	//this one is identical to getStatsSummaryNodes except for the field names that get called.. so we'll use that
	summary := &stats.ControlsSummary{}

	nodesSummary := depth.getStatsSummaryNodesResult(searchResult)

	summary.Failures = nodesSummary.Noncompliant
	summary.Passed = nodesSummary.Compliant
	summary.Skipped = nodesSummary.Skipped
	summary.Criticals = nodesSummary.HighRisk
	summary.Majors = nodesSummary.MediumRisk
	summary.Minors = nodesSummary.LowRisk

	return summary
}

func (depth *ControlDepth) wrap(aggs map[string]elastic.Aggregation) map[string]elastic.Aggregation {
	filteredControlAgg := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.id", depth.filters["control"][0]))
	controlsAgg := elastic.NewNestedAggregation().SubAggregation("filtered_controls", filteredControlAgg).Path("profiles.controls")
	for aggName, agg := range aggs {
		filteredControlAgg.SubAggregation(aggName, agg)
	}
	wrappedAggs := make(map[string]elastic.Aggregation)
	wrappedAggs["controls"] = controlsAgg

	plStats := &ProfileDepth{depth.QueryInfo}
	return plStats.wrap(wrappedAggs)
}

func (depth *ControlDepth) unwrap(aggs *elastic.Aggregations) (*elastic.AggregationSingleBucket, bool) {
	profileDepth := &ProfileDepth{depth.QueryInfo}
	if filteredProfiles, found := profileDepth.unwrap(aggs); found {
		if outerControlsAggResult, found := filteredProfiles.Aggregations.Nested("controls"); found {
			return outerControlsAggResult.Aggregations.Filter("filtered_controls")
		}
	}
	return nil, false
}

func (depth *ControlDepth) getQueryInfo() *QueryInfo {
	return depth.QueryInfo
}
