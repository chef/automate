package postgres

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestBuildWhereConstraintsFromFiltersNoFilterReturnsEmptyString(t *testing.T) {
	expected := ""
	actual, err := buildWhereConstraintsFromFilters(nil)
	assert.Nil(t, err)
	assert.Equal(t, expected, actual)
}

func TestBuildWhereConstraintsFromFiltersMatrix(t *testing.T) {
	noWhereConstraints := ""

	cases := []struct {
		message           string
		filters           map[string][]string
		expected          string
		shouldReturnError bool
		errMsg            string
	}{
		{
			message: "with service_group_id filter with single value returns built WHERE statement",
			filters: map[string][]string{
				"service_group_id": {"123"},
			},
			expected:          "WHERE group_id = '123'",
			shouldReturnError: false,
		},
		{
			message: "with service_group_id filter with multiple values returns built WHERE statement",
			filters: map[string][]string{
				"service_group_id": {"123", "456"},
			},
			expected:          "WHERE group_id = '123' OR group_id = '456'",
			shouldReturnError: false,
		},
		{
			message: "with health filter with single value returns built WHERE statement",
			filters: map[string][]string{
				"health": {"UNKNOWN"},
			},
			expected:          "WHERE health = 'UNKNOWN'",
			shouldReturnError: false,
		},
		{
			message: "with multiple valid filters and multiple values returns built WHERE statement",
			filters: map[string][]string{
				"service_group_id": {"123", "456"},
				"health":           {"WARNING", "OK"},
			},
			expected:          "WHERE group_id = '123' OR group_id = '456' AND health = 'WARNING' OR health = 'OK'",
			shouldReturnError: false,
		},
		{
			message: "valid filter with no value returns empty where constraints",
			filters: map[string][]string{
				"not-valid-filter": {},
			},
			expected:          noWhereConstraints,
			shouldReturnError: false,
		},
		{
			message: "invalid filter with no value returns empty where constraints",
			filters: map[string][]string{
				"not-valid-filter": {},
			},
			expected:          noWhereConstraints,
			shouldReturnError: false,
		},
		{
			message: "with invalid filters and a single value returns an error",
			filters: map[string][]string{
				"not-valid-filter": {"value"},
			},
			expected:          noWhereConstraints,
			shouldReturnError: true,
			errMsg:            "invalid filter. (not-valid-filter:[value])",
		},
		{
			message: "with invalid filters with multiple values returns an error",
			filters: map[string][]string{
				"not-valid-filter": {"value"},
				"not-awesome":      {"c", "o", "o", "l"},
				"more":             {"not", "valid", "filters"},
			},
			expected:          noWhereConstraints,
			shouldReturnError: true,
			errMsg:            "invalid filter.",
		},
		{
			message: "with valid and invalid filters returns an error",
			filters: map[string][]string{
				"service_group_id": {"1234"},
				"not-awesome":      {"c", "o", "o", "l"},
			},
			expected:          noWhereConstraints,
			shouldReturnError: true,
			errMsg:            "invalid filter. (not-awesome:[c o o l])",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual, err := buildWhereConstraintsFromFilters(test.filters)

			if test.shouldReturnError {
				assert.NotNil(t, err)
				assert.Contains(t, err.Error(), test.errMsg)
			} else {
				assert.Nil(t, err)
			}

			assert.Equal(t, test.expected, actual)
		})
	}
}
