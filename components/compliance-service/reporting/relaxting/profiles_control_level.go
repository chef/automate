package relaxting

import (
	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
)

//todo - refactor this to call ControlDepth::getControlListStatsByProfileIdResults and adjust.
func (depth *ControlDepth) getProfileMinsFromNodesAggs(filters map[string][]string) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	passedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "passed"))
	failedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "failed"))
	skippedFilter := elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.controls.status", "skipped"))

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
		SubAggregation("profile", profileAgg)

	aggs["impact"] = impactTerms

	return depth.wrap(aggs)
}

//todo - refactor this to call ControlDepth::getProfileListWithAggregatedComplianceSummariesResults and adjust.
func (depth *ControlDepth) getProfileMinsFromNodesResults(
	filters map[string][]string,
	searchResult *elastic.SearchResult,
	statusFilters []string) (map[string]reporting.ProfileMin, *reportingapi.ProfileCounts, error) {

	profileMins := make(map[string]reporting.ProfileMin)
	var counts *reportingapi.ProfileCounts

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
			if profileResult, found := impact.Aggregations.ReverseNested("profile"); found {
				if profileInfoResult, found := profileResult.Terms("profile-info"); found &&
					len(profileInfoResult.Buckets) > 0 {

					profileInfoBucket := profileInfoResult.Buckets[0]
					name := profileInfoBucket.KeyNumber
					summary.Name = string(name)

					if profileShaResult, found := profileInfoBucket.Terms("sha"); found &&
						len(profileShaResult.Buckets) > 0 {
						sha := profileShaResult.Buckets[0].KeyNumber
						summary.Id = string(sha)
					}
				}
			}
			profileStatus := computeStatus(summary.Failures, summary.Passed, summary.Skipped)
			summaryRep := reporting.ProfileMin{
				Name:   summary.Name,
				ID:     summary.Id,
				Status: profileStatus,
			}
			profileMins[summary.Id] = summaryRep
			counts = &reportingapi.ProfileCounts{
				Total:   summary.Failures + summary.Passed + summary.Skipped,
				Failed:  summary.Failures,
				Passed:  summary.Passed,
				Skipped: summary.Skipped,
			}
		}
	}

	return profileMins, counts, nil
}
