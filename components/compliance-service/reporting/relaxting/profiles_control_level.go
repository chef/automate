package relaxting

import (
	"github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
)

//todo - refactor this to call ControlDepth::getControlListStatsByProfileIdResults and adjust.
func (depth *ControlDepth) getProfileMinsFromNodesAggs(filters map[string][]string) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	// create the waived query first as we are going to use it in the next filters
	waivedQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes", "yes_run")

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermsQuery("profiles.controls.status", "passed")).
		MustNot(waivedQuery))

	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermsQuery("profiles.controls.status", "failed")).
		MustNot(waivedQuery))

	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewBoolQuery().
		Must(elastic.NewTermsQuery("profiles.controls.status", "skipped")).
		MustNot(waivedQuery))

	waivedFilter := elastic.NewFilterAggregation().Filter(waivedQuery)

	profileSha := elastic.NewTermsAggregation().Field("profiles.sha256").Size(int(1))
	profileAgg := elastic.NewReverseNestedAggregation().Path("profiles").
		SubAggregation("profile-info",
			elastic.NewTermsAggregation().
				Field("profiles.name").
				Size(int(1)).SubAggregation("sha", profileSha))

	impactTerms := elastic.NewTermsAggregation().Field("profiles.controls.impact").Size(1).
		SubAggregation("compliant", passedFilter).
		SubAggregation("noncompliant", failedFilter).
		SubAggregation("skipped", skippedFilter).
		SubAggregation("waived", waivedFilter).
		SubAggregation("profile", profileAgg)

	aggs["impact"] = impactTerms

	return depth.wrap(aggs)
}

//todo - refactor this to call ControlDepth::getProfileListWithAggregatedComplianceSummariesResults and adjust.
func (depth *ControlDepth) getProfileMinsFromNodesResults(
	filters map[string][]string,
	searchResult *elastic.SearchResult,
	statusFilters []string) ([]reporting.ProfileMin, *reportingapi.ProfileCounts, error) {

	profileMins := make([]reporting.ProfileMin, 0)
	var counts *reportingapi.ProfileCounts
	statusMap := make(map[string]int, 4)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		if impactBuckets, found := aggRoot.Aggregations.Terms("impact"); found && len(impactBuckets.Buckets) > 0 {
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
			if waivedResult, found := impact.Aggregations.Filter("waived"); found {
				summary.Waived = int32(waivedResult.DocCount)
			}
			if profileResult, found := impact.Aggregations.ReverseNested("profile"); found {
				if profileInfoResult, found := profileResult.Terms("profile-info"); found &&
					len(profileInfoResult.Buckets) > 0 {

					profileInfoBucket := profileInfoResult.Buckets[0]
					summary.Name = profileInfoBucket.Key.(string)

					if profileShaResult, found := profileInfoBucket.Terms("sha"); found &&
						len(profileShaResult.Buckets) > 0 {
						summary.Id = profileShaResult.Buckets[0].Key.(string)
					}
				}
			}
			profileStatus := computeStatus(summary.Failures, summary.Passed, summary.Skipped, summary.Waived)
			summaryRep := reporting.ProfileMin{
				Name:   summary.Name,
				ID:     summary.Id,
				Status: profileStatus,
			}
			profileMins = append(profileMins, summaryRep)

			//let's keep track of the counts even if they're not in the filter so that we may know that they're there for UI chicklets
			statusMap[profileStatus]++
			counts = &reportingapi.ProfileCounts{
				Total:   int32(statusMap["failed"] + statusMap["passed"] + statusMap["skipped"] + statusMap["waived"]),
				Failed:  int32(statusMap["failed"]),
				Passed:  int32(statusMap["passed"]),
				Skipped: int32(statusMap["skipped"]),
				Waived:  int32(statusMap["waived"]),
			}
		}
	}

	return profileMins, counts, nil
}
