package relaxting

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/olivere/elastic/v7"
	"github.com/stretchr/testify/assert"
)

func TestFiltersForAssetIndex(t *testing.T) {

	tests := []struct {
		name          string
		filtersKey    string
		filtersValue  []string
		expectedQuery string
		esr           ES2Backend
	}{
		{
			name:          "Test for Environment",
			filtersKey:    "environment",
			filtersValue:  []string{"Test ENV"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"environment.lower":["Test ENV"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Organisation",
			filtersKey:    "organization",
			filtersValue:  []string{"Test Org"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"organization.lower":["Test Org"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for chef_server",
			filtersKey:    "chef_server",
			filtersValue:  []string{"fqdn"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"chef_server.lower":["fqdn"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Policy group",
			filtersKey:    "policy_group",
			filtersValue:  []string{"Policy"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"policy_group.lower":["Policy"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Policy Name",
			filtersKey:    "policy_name",
			filtersValue:  []string{"name"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"policy_name.lower":["name"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Inspec Version",
			filtersKey:    "inspec_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"inspec_version.lower":["1"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for platform with version",
			filtersKey:    "platform_with_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"platform.full.lower":["1"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for recipe",
			filtersKey:    "recipe",
			filtersValue:  []string{"testrecipe"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"recipe.lower":["testrecipe"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Role",
			filtersKey:    "role",
			filtersValue:  []string{"testrole"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"role.lower":["testrole"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Platform",
			filtersKey:    "platform",
			filtersValue:  []string{"Test Plat"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"platform.name.lower":["Test Plat"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Profile ID",
			filtersKey:    "profile_id",
			filtersValue:  []string{"Profile"},
			expectedQuery: `{"bool":{"must":[{"bool":{"should":{"terms":{"profile_id":["Profile"]}}}},{"nested":{"inner_hits":{"_source":{"includes":["profiles.name","profiles.sha256"]}},"path":"profiles","query":{"bool":{"must":{"query_string":{"query":"profiles.sha256:/(Profile)/"}}}}}}]}}`,
			esr:           esr,
		},
		{
			name:          "Test for Control ID",
			filtersKey:    "control",
			filtersValue:  []string{"Control"},
			expectedQuery: `{"bool":{"must":{"nested":{"path":"profiles","query":{"bool":{"must":[{"query_string":{"query":"profiles.sha256:/(.*)/"}},{"nested":{"path":"profiles.controls","query":{"query_string":{"query":"profiles.controls.id:/(Control)/"}}}}]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Node Id",
			filtersKey:    "node_id",
			filtersValue:  []string{"NodeUUId1", "NodeUUd2"},
			expectedQuery: `{"bool":{"must":{"terms":{"node_uuid":["NodeUUId1","NodeUUd2"]}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Control tags",
			filtersKey:    "control_tag:focus_area",
			filtersValue:  []string{""},
			expectedQuery: `{"bool":{"must":{"nested":{"path":"control_tag","query":{"bool":{"must":[{"terms":{"control_tag.key.lower":["focus_area"]}},{"bool":{"should":{"bool":{"must_not":{"exists":{"field":"control_tag.values.lower"}}}}}}]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Control tags and Values",
			filtersKey:    "control_tag:focus_area",
			filtersValue:  []string{"test"},
			expectedQuery: `{"bool":{"must":{"nested":{"path":"control_tag","query":{"bool":{"must":[{"terms":{"control_tag.key.lower":["focus_area"]}},{"bool":{"should":{"terms":{"control_tag.values.lower":["test"]}}}}]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for chef tags",
			filtersKey:    "chef_tags",
			filtersValue:  []string{"tags"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"chef_tags.lower":["tags"]}}}}}}`,
			esr:           esr,
		},
	}

	for _, test := range tests {
		filters := make(map[string][]string)
		filters[test.filtersKey] = test.filtersValue
		t.Run(test.name, func(t *testing.T) {
			got := esr.getFiltersQueryForAssetFilters(filters)
			query, _ := got.Source()
			data, _ := json.Marshal(query)
			actualQuery := string(data)
			assert.Equal(t, test.expectedQuery, actualQuery)

		})

	}

}

func TestStartTimeAndEndTimeFilters(t *testing.T) {
	startTime := "2022-07-17T00:00:00Z"
	endTime := "2022-07-18T23:59:59Z"
	tests := []struct {
		name              string
		startTime         string
		endTime           string
		expectedQuery     string
		unreachableConfig int
		esr               ES2Backend
	}{
		{name: "Test for Starttime and endTime",
			endTime:       endTime,
			startTime:     startTime,
			expectedQuery: `{"range":{"last_run":{"from":"2022-07-17T00:00:00Z","include_lower":true,"include_upper":true,"to":"2022-07-18T23:59:59Z"}}}`,
			esr:           esr,
		},
		{name: "Test for null end time and start time",
			startTime:     startTime,
			expectedQuery: `{"range":{"last_run":{"from":null,"include_lower":false,"include_upper":true,"to":"2022-07-17T00:00:00Z"}}}`,
			esr:           esr,
			endTime:       "",
		},
	}
	for _, test := range tests {
		filters := make(map[string][]string)
		filters["start_time"] = []string{test.startTime}
		filters["end_time"] = []string{test.endTime}

		t.Run(test.name, func(t *testing.T) {
			got := getStartTimeAndEndTimeRangeForAsset(filters)
			query, _ := got.Source()
			data, _ := json.Marshal(query)
			actualQuery := string(data)
			assert.Equal(t, test.expectedQuery, actualQuery)

		})

	}

}

func TestReachableAssetFilters(t *testing.T) {
	startTime := "2022-07-17T00:00:00Z"
	endTime := "2022-07-18T23:59:59Z"
	tests := []struct {
		name              string
		startTime         string
		endTime           string
		expectedQuery     string
		unreachableConfig int
		esr               ES2Backend
	}{
		{name: "Test for 15 days unreachable config",
			endTime:           endTime,
			unreachableConfig: 15,
			expectedQuery:     fmt.Sprintf(`{"range":{"last_run":{"from":null,"include_lower":true,"include_upper":true,"to":"%s"}}}`, startTime),
			esr:               esr,
		},
		{name: "Test for 0 days unreachable config",
			endTime:           endTime,
			unreachableConfig: 0,
			expectedQuery:     "",
			esr:               esr,
		},
	}
	for _, test := range tests {
		filters := make(map[string][]string)
		filters["start_time"] = []string{test.startTime}
		filters["end_time"] = []string{test.endTime}

		if test.unreachableConfig > 0 {
			startTimeBefore := time.Now().Add(-24 * time.Hour * time.Duration(test.unreachableConfig)).UTC().Format(time.RFC3339)
			test.expectedQuery = fmt.Sprintf(`{"range":{"last_run":{"from":null,"include_lower":true,"include_upper":true,"to":"%s"}}}`, startTimeBefore)
		}

		t.Run(test.name, func(t *testing.T) {
			got := getUnReachableAssetTimeRangeQuery(test.unreachableConfig)
			actualQuery := ""
			if got != nil {
				query, _ := got.Source()
				data, _ := json.Marshal(query)
				actualQuery = string(data)
			}

			assert.Equal(t, test.expectedQuery, actualQuery)

		})

	}

}

func TestGetResultAssetSummary(t *testing.T) {
	assetSummary := AssetSummary{
		Passed:  0,
		Failed:  10,
		Waived:  16,
		Skipped: 20,
	}

	contentPassed := []byte(`{"doc_count": 0}`)
	contentFailed := []byte(`{"doc_count": 10}`)
	contentWaived := []byte(`{"doc_count": 16}`)
	contentSkipped := []byte(`{"doc_count": 20}`)

	aggregations := make(map[string]json.RawMessage)
	aggregations["failed"] = contentFailed
	aggregations["passed"] = contentPassed
	aggregations["waived"] = contentWaived
	aggregations["skipped"] = contentSkipped

	searchResult := &elastic.SearchResult{
		Aggregations: aggregations,
	}

	assetSummaryActual := getSummaryAssetAggResult(searchResult)

	assert.Equal(t, assetSummary.Passed, assetSummaryActual.Passed)
	assert.Equal(t, assetSummary.Failed, assetSummaryActual.Failed)
	assert.Equal(t, assetSummary.Skipped, assetSummaryActual.Skipped)
	assert.Equal(t, assetSummary.Waived, assetSummaryActual.Waived)

}

func TestGetAggsForAssets(t *testing.T) {

	tests := []struct {
		Name          string
		expectedQuery string
		aggsType      string
	}{
		{
			Name:          "Test Failed Aggregation",
			expectedQuery: `{"filter":{"term":{"status":"failed"}}}`,
			aggsType:      "failed",
		},
		{
			Name:          "Test Passed Aggregation",
			expectedQuery: `{"filter":{"term":{"status":"passed"}}}`,
			aggsType:      "passed",
		},
		{
			Name:          "Test Waived Aggregation",
			expectedQuery: `{"filter":{"term":{"status":"waived"}}}`,
			aggsType:      "waived",
		},
		{
			Name:          "Test Waived Aggregation",
			expectedQuery: `{"filter":{"term":{"status":"skipped"}}}`,
			aggsType:      "skipped",
		},
	}

	for _, test := range tests {
		aggsMap := getSummaryAssetAggregation()

		failedAggs, found := aggsMap[test.aggsType]
		actualQuery := ""
		if found {

			source, _ := failedAggs.Source()
			data, _ := json.Marshal(source)
			actualQuery = string(data)
		}
		assert.Equal(t, test.expectedQuery, actualQuery)
	}

}
