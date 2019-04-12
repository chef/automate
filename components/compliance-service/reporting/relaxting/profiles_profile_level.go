package relaxting

import (
	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/lib/stringutils"
	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
)

//todo - this looks very close to the report depth version of it.. double check and if so lets' harmonize them
func (depth *ProfileDepth) getProfileMinsFromNodesAggs(filters map[string][]string) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	termsQuery := elastic.NewTermsAggregation().
		Field("profiles.profile").Size(reporting.ESize)
	termsQuery.SubAggregation("failures", elastic.NewSumAggregation().
		Field("profiles.controls_sums.failed.total"))
	termsQuery.SubAggregation("passed", elastic.NewSumAggregation().
		Field("profiles.controls_sums.passed.total"))
	termsQuery.SubAggregation("skipped", elastic.NewSumAggregation().
		Field("profiles.controls_sums.skipped.total"))
	termsQuery.SubAggregation("status", elastic.NewTermsAggregation().
		Field("profiles.status"))

	aggs["totals"] = termsQuery

	return depth.wrap(aggs)
}

//todo - this looks very close to the report depth version of it.. double check and if so lets' harmonize them
func (depth *ProfileDepth) getProfileMinsFromNodesResults(
	filters map[string][]string,
	searchResult *elastic.SearchResult,
	statusFilters []string) (map[string]reporting.ProfileMin, *reportingapi.ProfileCounts, error) {
	myName := "ProfileDepth::getProfileMinsFromNodes"

	profileMins := make(map[string]reporting.ProfileMin)
	statusMap := make(map[string]int, 3)

	if aggRoot, found := depth.unwrap(&searchResult.Aggregations); found {
		totalsAgg, _ := aggRoot.Terms("totals")
		if totalsAgg != nil {
			for _, bucket := range totalsAgg.Buckets {
				profileName, profileId := rightSplit(string(bucket.KeyNumber), "|") // bucket.KeyNumber

				if profilesFilterArray, found := filters["profile_id"]; found {
					if !stringutils.SliceContains(profilesFilterArray, profileId) {
						continue
					}
				}

				// Using the status of the profile, introduced with inspec 3.0 to overwrite the status calculations from totals
				profileStatusHash := make(map[string]int64, 0)
				statuses, _ := bucket.Aggregations.Terms("status")
				if statuses.Buckets != nil {
					for _, statusBucket := range statuses.Buckets {
						status := statusBucket.Key.(string)
						profileStatusHash[status] = statusBucket.DocCount
					}
				}

				var profileStatus string
				if profileStatusHash["skipped"] > 0 && profileStatusHash["loaded"] == 0 && profileStatusHash[""] == 0 {
					profileStatus = "skipped"
					logrus.Debugf("%s profile_name=%q, status=%q", myName, profileName, profileStatus)
				} else {
					sumFailures, _ := bucket.Aggregations.Sum("failures")
					sumPassed, _ := bucket.Aggregations.Sum("passed")
					sumSkipped, _ := bucket.Aggregations.Sum("skipped")
					profileStatus = computeStatus(int32(*sumFailures.Value), int32(*sumPassed.Value), int32(*sumSkipped.Value))

					logrus.Debugf(
						"%s profile_name=%s, status=%s (sumFailures=%d, sumPassed=%d, sumSkipped=%d)",
						myName, profileName, profileStatus, int32(*sumFailures.Value), int32(*sumPassed.Value),
						int32(*sumSkipped.Value))
				}

				if len(statusFilters) > 0 && !stringutils.SliceContains(statusFilters, profileStatus) {
					continue
				}

				statusMap[profileStatus]++

				summary := reporting.ProfileMin{
					Name:   profileName,
					ID:     profileId,
					Status: profileStatus,
				}
				profileMins[profileId] = summary
			}
		}
	}
	logrus.Debugf("%s Done with statusMap=%+v", myName, statusMap)
	logrus.Debugf("%s Done with statusMap['something']=%+v", myName, statusMap["passed"])
	counts := &reportingapi.ProfileCounts{
		Total:   int32(statusMap["failed"] + statusMap["passed"] + statusMap["skipped"]),
		Failed:  int32(statusMap["failed"]),
		Passed:  int32(statusMap["passed"]),
		Skipped: int32(statusMap["skipped"]),
	}
	return profileMins, counts, nil
}
