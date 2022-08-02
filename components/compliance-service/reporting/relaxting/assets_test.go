package relaxting

import (
	"encoding/json"
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
	"time"
)

var esr = ES2Backend{
	ESUrl:             "",
	Enterprise:        "",
	ChefDeliveryUser:  "",
	ChefDeliveryToken: "",
}

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
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"environment":["Test ENV"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Organisation",
			filtersKey:    "organization",
			filtersValue:  []string{"Test Org"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"organization":["Test Org"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for chef_server",
			filtersKey:    "chef_server",
			filtersValue:  []string{"fqdn"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"chef_server":["fqdn"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Policy group",
			filtersKey:    "policy_group",
			filtersValue:  []string{"Policy"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"policy_group":["Policy"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Policy Name",
			filtersKey:    "policy_name",
			filtersValue:  []string{"name"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"policy_name":["name"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Inspec Version",
			filtersKey:    "inspec_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"inspec_version":["1"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for platform with version",
			filtersKey:    "platform_with_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"platform_with_version":["1"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for recipe",
			filtersKey:    "recipe",
			filtersValue:  []string{"testrecipe"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"recipe":["testrecipe"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Role",
			filtersKey:    "role",
			filtersValue:  []string{"testrole"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"role":["testrole"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Platform",
			filtersKey:    "platform",
			filtersValue:  []string{"Test Plat"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"platform":["Test Plat"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Profile ID",
			filtersKey:    "profile_id",
			filtersValue:  []string{"Profile"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"profile_id":["Profile"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Control ID",
			filtersKey:    "control",
			filtersValue:  []string{"Control"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"control_id":["Control"]}}}}}}`,
			esr:           esr,
		},
		{
			name:          "Test for Node Id",
			filtersKey:    "node_id",
			filtersValue:  []string{"NodeUUId"},
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"node_id":["NodeUUId"]}}}}}}`,
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
			expectedQuery: `{"bool":{"must":{"bool":{"should":{"terms":{"chef_tags":["tags"]}}}}}}`,
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
		{name: "Test for 0 unreachable config",
			startTime:         startTime,
			endTime:           endTime,
			unreachableConfig: 0,
			expectedQuery:     `{"range":{"last_run":{"from":"2022-07-17T00:00:00Z","include_lower":true,"include_upper":true,"to":"2022-07-18T23:59:59Z"}}}`,
			esr:               esr,
		},
		{name: "Test for 15 days unreachable config",
			endTime:           endTime,
			unreachableConfig: 15,
			expectedQuery:     fmt.Sprintf(`{"range":{"last_run":{"from":"%s","include_lower":false,"include_upper":true,"to":null}}}`, startTime),
			esr:               esr,
		},
	}
	for _, test := range tests {
		filters := make(map[string][]string)
		filters["start_time"] = []string{test.startTime}
		filters["end_time"] = []string{test.endTime}

		if test.unreachableConfig > 0 {
			startTimeBefore := time.Now().Add(-24 * time.Hour * time.Duration(test.unreachableConfig)).UTC().Format(time.RFC3339)
			test.expectedQuery = fmt.Sprintf(`{"range":{"last_run":{"from":"%s","include_lower":false,"include_upper":true,"to":null}}}`, startTimeBefore)
		}

		t.Run(test.name, func(t *testing.T) {
			got := getStartTimeAndEndTimeRangeForAsset(filters, test.unreachableConfig)
			query, _ := got.Source()
			data, _ := json.Marshal(query)
			actualQuery := string(data)
			assert.Equal(t, test.expectedQuery, actualQuery)

		})

	}

}
