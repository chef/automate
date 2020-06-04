package postgres

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/stretchr/testify/assert"
)

func TestConvertComposedServiceToStorage(t *testing.T) {
	expected := &storage.Service{}
	assert.Equal(t, expected, convertComposedServiceToStorage(&composedService{}))

	expected = &storage.Service{ID: 1, SupMemberID: "foo"}
	assert.Equal(t, expected, convertComposedServiceToStorage(&composedService{ID: 1, SupMemberID: "foo"}))

	expected = &storage.Service{Application: "abcd", PreviousHealth: "MORE-OR-LESS-OK"}
	assert.Equal(t, expected, convertComposedServiceToStorage(&composedService{
		Application: "abcd", PreviousHealth: "MORE-OR-LESS-OK",
	}))
}

func TestConvertComposedServicesToStorage(t *testing.T) {
	expected := []*storage.Service{
		&storage.Service{},
		&storage.Service{ID: 1, SupMemberID: "foo"},
		&storage.Service{Application: "abcd", PreviousHealth: "MORE-OR-LESS-OK"},
	}
	subject := convertComposedServicesToStorage([]*composedService{
		&composedService{},
		&composedService{ID: 1, SupMemberID: "foo"},
		&composedService{Application: "abcd", PreviousHealth: "MORE-OR-LESS-OK"},
	})
	for _, e := range expected {
		assert.Contains(t, subject, e)
	}
}

func TestBuildWhereConstraintsFromFiltersNoFilterReturnsEmptyString(t *testing.T) {
	expected := ""
	actual, err := buildWhereConstraintsFromFilters(nil, "WHERE", true)
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
			expected:          "WHERE (service_group_id = '123')",
			shouldReturnError: false,
		},
		{
			message: "with service_group_id filter with multiple values returns built WHERE statement",
			filters: map[string][]string{
				"service_group_id": {"123", "456"},
			},
			expected:          "WHERE (service_group_id = '123' OR service_group_id = '456')",
			shouldReturnError: false,
		},
		{
			message: "with health filter with single value returns built WHERE statement",
			filters: map[string][]string{
				"status": {"UNKNOWN"},
			},
			expected:          "WHERE (health = 'UNKNOWN')",
			shouldReturnError: false,
		},
		{
			message: "with multiple valid filters and multiple values returns built WHERE statement",
			filters: map[string][]string{
				"service_group_id": {"123", "456"},
				"status":           {"WARNING", "OK"},
			},
			//
			// NOTE: If you change the query generation, you must keep the parenthesis
			// or have flawless operator precedence, because this:
			//
			//   WHERE service_group_id = '123' OR service_group_id = '456' AND health = 'WARNING' OR health = 'OK'
			//
			// actually means this:
			//
			//   WHERE (service_group_id = '123') OR (service_group_id = '456' AND health = 'WARNING' OR health = 'OK')
			//
			expected:          "WHERE (service_group_id = '123' OR service_group_id = '456') AND (health = 'WARNING' OR health = 'OK')",
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
			actual, err := buildWhereConstraintsFromFilters(test.filters, "WHERE", true)

			if test.shouldReturnError {
				assert.NotNil(t, err)
				assert.Contains(t, err.Error(), test.errMsg)
			} else {
				assert.NoError(t, err)
			}

			extraFailureMessage := fmt.Sprintf("\nactual: >>%s<<\nexpect: >>%s<<\n", actual, test.expected)

			assert.ElementsMatch(t, clauses(test.expected), clauses(actual), extraFailureMessage)
		})
	}
}

func clauses(predicate string) []string {
	return regexp.MustCompile("WHERE | AND ").Split(predicate, -1)
}
