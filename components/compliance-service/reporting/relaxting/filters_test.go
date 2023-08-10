package relaxting

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"
)

var esr = ES2Backend{
	ESUrl:             "",
	Enterprise:        "",
	ChefDeliveryUser:  "",
	ChefDeliveryToken: "",
}

var dummyTime = "2022-07-18T08:14:26Z"

func TestFiltersForControlIndex(t *testing.T) {

	startTime := "2022-07-17T00:00:00Z"
	endTime := "2022-07-18T23:59:59Z"
	tests := []struct {
		name          string
		filtersKey    string
		filtersValue  []string
		expectedQuery string
		esr           ES2Backend
		startTime     string
		endTime       string
		latestOnly    bool
	}{
		{
			name:          "Test for Environment",
			filtersKey:    "environment",
			filtersValue:  []string{"Test ENV"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.environment.lower":["Test ENV"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Chef Server",
			filtersKey:    "chef_server",
			filtersValue:  []string{"fqdn"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.source_fqdn.lower":["fqdn"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Inspec Version",
			filtersKey:    "inspec_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.version.lower":["1"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for platform with version",
			filtersKey:    "platform_with_version",
			filtersValue:  []string{"1"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.platform.full.lower":["1"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for recipe",
			filtersKey:    "recipe",
			filtersValue:  []string{"testrecipe"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.recipes.lower":["testrecipe"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Role",
			filtersKey:    "role",
			filtersValue:  []string{"testrole"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.roles.lower":["testrole"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Platform",
			filtersKey:    "platform",
			filtersValue:  []string{"Test Plat"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.platform.name.lower":["Test Plat"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Profile ID",
			filtersKey:    "profile_id",
			filtersValue:  []string{"Profile"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"terms":{"profile.profile_id":["Profile"]}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Control ID",
			filtersKey:    "control",
			filtersValue:  []string{"Control"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"terms":{"control_id":["Control"]}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Control Name",
			filtersKey:    "control_name",
			filtersValue:  []string{"ControlName"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"bool":{"should":{"terms":{"title.lower":["ControlName"]}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Profile Name",
			filtersKey:    "profile_name",
			filtersValue:  []string{"ProfileName"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"bool":{"should":{"terms":{"profiles.title.lower":["ProfileName"]}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Node Id",
			filtersKey:    "node_id",
			filtersValue:  []string{"NodeUUId"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"terms":{"nodes.node_uuid":["NodeUUId"]}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Job Id",
			filtersKey:    "job_id",
			filtersValue:  []string{"JobUUId"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"terms":{"nodes.job_uuid":["JobUUId"]}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Start Time",
			filtersKey:    "job_id",
			filtersValue:  []string{"JobUUId"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"terms":{"nodes.job_uuid":["JobUUId"]}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":null}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       "",
		},
		{
			name:          "Test for Day Latest and Daily Latest Flags",
			filtersKey:    "organization",
			filtersValue:  []string{"ORG"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.organization_name.lower":["ORG"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-17T00:00:00Z","include_lower":true,"include_upper":true,"to":"2022-07-18T23:59:59Z"}}},{"terms":{"daily_latest":[true]}}]}}`,
			esr:           esr,
			startTime:     startTime,
			endTime:       endTime,
			latestOnly:    true,
		},
		{
			name:          "Test for String tags",
			filtersKey:    "control_tag:focus_area",
			filtersValue:  []string{""},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}},{"nested":{"path":"string_tags","query":{"bool":{"must":[{"terms":{"string_tags.key.lower":["focus_area"]}},{"bool":{"should":{"bool":{"must_not":{"exists":{"field":"string_tags.values.lower"}}}}}}]}}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for String tags and Values",
			filtersKey:    "control_tag:focus_area",
			filtersValue:  []string{"test"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}},{"nested":{"path":"string_tags","query":{"bool":{"must":[{"terms":{"string_tags.key.lower":["focus_area"]}},{"bool":{"should":{"terms":{"string_tags.values.lower":["test"]}}}}]}}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for chef tags",
			filtersKey:    "chef_tags",
			filtersValue:  []string{"tags"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.chef_tags.lower":["tags"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for policy group",
			filtersKey:    "policy_group",
			filtersValue:  []string{"group"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.policy_group.lower":["group"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for policy group",
			filtersKey:    "policy_name",
			filtersValue:  []string{"policy name"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.policy_name.lower":["policy name"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
		{
			name:          "Test for Node Name",
			filtersKey:    "node_name",
			filtersValue:  []string{"TestName"},
			expectedQuery: `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{"must":{"bool":{"should":{"terms":{"nodes.node_name.lower":["TestName"]}}}}}}}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`,
			esr:           esr,
			startTime:     dummyTime,
			endTime:       dummyTime,
		},
	}

	for _, test := range tests {
		filters := make(map[string][]string)
		filters[test.filtersKey] = test.filtersValue
		filters["start_time"] = []string{test.startTime}
		filters["end_time"] = []string{test.endTime}
		t.Run(test.name, func(t *testing.T) {
			got := esr.getFiltersQueryForStatsSummaryControls(filters, test.latestOnly)
			query, _ := got.Source()
			data, _ := json.Marshal(query)
			actualQuery := string(data)
			assert.Equal(t, test.expectedQuery, actualQuery)

		})

	}

}

func TestForControlIdAndProfileId(t *testing.T) {
	filters := make(map[string][]string)
	filters["profile_id"] = []string{"testProfile"}
	filters["control"] = []string{"testControl"}
	filters["start_time"] = []string{dummyTime}
	filters["end_time"] = []string{dummyTime}
	expectedQuery := `{"bool":{"must":[{"nested":{"path":"nodes","query":{"bool":{}}}},{"terms":{"control_id":["testControl"]}},{"terms":{"profile.profile_id":["testProfile"]}},{"range":{"end_time":{"from":"2022-07-18T08:14:26Z","include_lower":true,"include_upper":true,"to":"2022-07-18T08:14:26Z"}}}]}}`
	got := esr.getFiltersQueryForStatsSummaryControls(filters, false)
	query, _ := got.Source()
	data, _ := json.Marshal(query)
	actualQuery := string(data)
	assert.Equal(t, expectedQuery, actualQuery)

}
