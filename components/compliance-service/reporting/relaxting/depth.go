package relaxting

import (
	"encoding/json"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type QueryInfo struct {
	filters   map[string][]string
	filtQuery *elastic.BoolQuery
	client    *elastic.Client
	esIndex   string
	level     int
}

type ControlDepth struct {
	*QueryInfo
}

type ProfileDepth struct {
	*QueryInfo
}

type ReportDepth struct {
	*QueryInfo
}

type Depth interface {
	getQueryInfo() *QueryInfo

	getStatsSummaryAggs() map[string]elastic.Aggregation
	getStatsSummaryResult(searchResult *elastic.SearchResult) *stats.ReportSummary

	getStatsSummaryNodesAggs() map[string]elastic.Aggregation
	getStatsSummaryNodesResult(searchResult *elastic.SearchResult) *stats.NodeSummary

	getStatsSummaryControlsAggs() map[string]elastic.Aggregation
	getStatsSummaryControlsResult(searchResult *elastic.SearchResult) *stats.ControlsSummary

	getStatsTopFailuresAggs(size int, reportTypes []string) (map[string]elastic.Aggregation, error)
	getStatsTopFailuresResult(result *elastic.SearchResult, reportTypes []string) (*stats.Failures, error)

	getProfileListWithAggregatedComplianceSummariesAggs(filters map[string][]string,
		size int32) map[string]elastic.Aggregation
	getProfileListWithAggregatedComplianceSummariesResults(result *elastic.SearchResult,
		filters map[string][]string) []*stats.ProfileList

	getControlListStatsByProfileIdAggs(size int, sortField string, sortAsc bool) map[string]elastic.Aggregation
	getControlListStatsByProfileIdResults(backend *ES2Backend, searchResult *elastic.SearchResult,
		profileId string) ([]*stats.ControlStats, error)

	getProfileMinsFromNodesAggs(filters map[string][]string) map[string]elastic.Aggregation
	getProfileMinsFromNodesResults(
		filters map[string][]string,
		searchResult *elastic.SearchResult,
		statusFilters []string) (map[string]reporting.ProfileMin, *reportingapi.ProfileCounts, error)

	getTrendAggs(trendType string, filters map[string][]string) map[string]elastic.Aggregation
	getTrendResults(trendType string,
		searchResult *elastic.SearchResult) (map[string]*stats.Trend, error)
}

func (backend ES2Backend) NewDepth(filters map[string][]string, useStartTime bool, latestOnly bool) (Depth, error) {
	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return nil, err
	}

	esIndex, err := GetEsIndex(filters, false, useStartTime)
	if err != nil {
		return nil, errors.Wrap(err, "GetLevelStats")
	}

	filtQuery := backend.getFiltersQuery(filters, latestOnly)

	level := GetFilterDepth(filters)

	queryInfo := &QueryInfo{
		filters:   filters,
		filtQuery: filtQuery,
		esIndex:   esIndex,
		client:    client,
		level:     level,
	}

	if level == ReportLevel {
		return &ReportDepth{queryInfo}, nil
	} else if level == ProfileLevel {
		return &ProfileDepth{queryInfo}, nil
	} else if level == ControlLevel {
		return &ControlDepth{queryInfo}, nil
	}

	return nil, errors.New("Invalid depth")
}

func getDeepControlsSums(hit *elastic.SearchHit,
	queryInfo *QueryInfo) (nodeControlSummary reporting.NodeControlSummary, status string, err error) {

	if hit.InnerHits != nil {
		for _, innerHit := range hit.InnerHits {
			if innerHit != nil {
				for _, profileHit := range innerHit.Hits.Hits {
					var profile ProfileSource
					if profileHit.Source != nil {
						err := json.Unmarshal(*profileHit.Source, &profile)
						if err == nil {
							if queryInfo.level == ControlLevel {
								return getControlLevelControlSums(profileHit)
							} else {
								return profile.ControlsSums, profile.Status, nil
							}
						}
					}
				}
			}
		}
	}
	return nodeControlSummary, status, nil
}

func getControlLevelControlSums(hit *elastic.SearchHit) (nodeControlSummary reporting.NodeControlSummary,
	status string, err error) {
	if hit.InnerHits != nil {
		for _, innerHit := range hit.InnerHits {
			if innerHit != nil {
				for _, controlHit := range innerHit.Hits.Hits {
					var control ControlSource
					if controlHit.Source != nil {
						err := json.Unmarshal(*controlHit.Source, &control)
						if err == nil {
							// this is for one node and since this control should only be in this profile once, it's
							// reasonable to list it with a cardinality of 1
							if control.Status == "failed" {
								nodeControlSummary.Failed.Total = 1
								if control.Impact < 0.4 {
									nodeControlSummary.Failed.Minor = 1
								} else if control.Impact < 0.7 {
									nodeControlSummary.Failed.Major = 1
								} else {
									nodeControlSummary.Failed.Critical = 1
								}
							} else if control.Status == "passed" {
								nodeControlSummary.Passed.Total = 1
							} else {
								nodeControlSummary.Skipped.Total = 1
							}
							nodeControlSummary.Total = 1
							return nodeControlSummary, control.Status, nil
						}
					}
				}
			}
		}
	}
	return
}

func getDeepInspecProfiles(hit *elastic.SearchHit,
	queryInfo *QueryInfo) (profiles []ESInSpecReportProfile, status string, err error) {
	if hit.InnerHits != nil {
		for _, innerHit := range hit.InnerHits {
			if innerHit != nil {
				for _, profileHit := range innerHit.Hits.Hits {
					var profile ESInSpecReportProfile
					if profileHit.Source != nil {
						err := json.Unmarshal(*profileHit.Source, &profile)
						if err == nil {
							if queryInfo.level == ControlLevel {
								controls, status, err := getControlLevelControls(profileHit)
								if err != nil {
									return profiles, status, err
								}
								profile.Controls = controls
								profile.Status = status
								profiles = append(profiles, profile)
								return profiles, status, err
							} else {
								profiles = append(profiles, profile)
								return profiles, profile.Status, nil
							}
						}
					}
				}
			}
		}
	}
	return profiles, status, nil
}

func getControlLevelControls(hit *elastic.SearchHit) (controls []ESInSpecReportControl, status string, err error) {
	if hit.InnerHits != nil {
		logrus.Debug(controls)
		for _, innerHit := range hit.InnerHits {
			if innerHit != nil {
				for _, controlHit := range innerHit.Hits.Hits {
					var control ESInSpecReportControl
					if controlHit.Source != nil {
						err := json.Unmarshal(*controlHit.Source, &control)
						if err == nil {
							status = control.Status
							controls = append(controls, control)
						}
					}
				}
			}
		}
	}
	return controls, status, err
}
