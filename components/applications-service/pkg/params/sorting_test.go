package params_test

import (
	"testing"

	"github.com/chef/automate/api/external/common/query"
	subject "github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/stretchr/testify/assert"
)

func TestSortingForServiceGroupsDefaults(t *testing.T) {
	var (
		sorting                  = &query.Sorting{}
		expectedSortField string = "name"
		expectedSortAsc   bool   = true
	)
	actualSortField, actualSortAsc, err := subject.GetSortParamsForServiceGroups(sorting)
	assert.Nil(t, err)
	assert.Equal(t, expectedSortField, actualSortField)
	assert.Equal(t, expectedSortAsc, actualSortAsc)
}

func TestSortingForServiceGroupsNilReturnsDefaults(t *testing.T) {
	var (
		expectedSortField string = "name"
		expectedSortAsc   bool   = true
	)
	actualSortField, actualSortAsc, err := subject.GetSortParamsForServiceGroups(nil)
	assert.Nil(t, err)
	assert.Equal(t, expectedSortField, actualSortField)
	assert.Equal(t, expectedSortAsc, actualSortAsc)
}

func TestSortingForServiceGroupsMatrix(t *testing.T) {
	cases := []struct {
		message           string
		sorting           *query.Sorting
		expectedSortField string
		expectedSortAsc   bool
		shouldReturnError bool
		errMsg            string
	}{
		{
			message:           "with nil sorting query params should return defaults",
			sorting:           nil,
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message:           "with empty sorting query params should return defaults",
			sorting:           &query.Sorting{},
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message: "with valid sorting field 'name' and order 'ASC' should return field and order params",
			sorting: &query.Sorting{
				Field: "name",
				Order: query.SortOrder_ASC,
			},
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message: "with valid sorting field 'percent_ok' and order 'DESC' should return field and order params",
			sorting: &query.Sorting{
				Field: "percent_ok",
				Order: query.SortOrder_DESC,
			},
			expectedSortField: "percent_ok",
			expectedSortAsc:   false,
			shouldReturnError: false,
		},
		{
			message:           "with empty sorting field use default",
			sorting:           &query.Sorting{Field: ""},
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message:           "with invalid sorting field returns an error",
			sorting:           &query.Sorting{Field: "not-valid-field"},
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: true,
			errMsg:            "Invalid sort field 'not-valid-field'.",
		},
		{
			message:           "with invalid sorting field returns an error",
			sorting:           &query.Sorting{Field: "names"},
			expectedSortField: "name",
			expectedSortAsc:   true,
			shouldReturnError: true,
			errMsg:            "Invalid sort field 'names'.",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actualSortField, actualSortAsc, err := subject.GetSortParamsForServiceGroups(test.sorting)

			if test.shouldReturnError {
				assert.NotNil(t, err)
				assert.Contains(t, err.Error(), test.errMsg)
			} else {
				assert.Nil(t, err)
			}

			assert.Equal(t, test.expectedSortField, actualSortField)
			assert.Equal(t, test.expectedSortAsc, actualSortAsc)
		})
	}
}

func TestSortingForServicesMatrix(t *testing.T) {
	cases := []struct {
		message           string
		sorting           *query.Sorting
		expectedSortField string
		expectedSortAsc   bool
		shouldReturnError bool
		errMsg            string
	}{
		{
			message:           "with nil sorting query params should return defaults",
			sorting:           nil,
			expectedSortField: "status",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message:           "with empty sorting query params should return defaults",
			sorting:           &query.Sorting{},
			expectedSortField: "status",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message: "with valid sorting field 'status' and order 'DESC' should return field and order params",
			sorting: &query.Sorting{
				Field: "status",
				Order: query.SortOrder_DESC,
			},
			expectedSortField: "status",
			expectedSortAsc:   false,
			shouldReturnError: false,
		},
		{
			message:           "with empty sorting field use default",
			sorting:           &query.Sorting{Field: ""},
			expectedSortField: "status",
			expectedSortAsc:   true,
			shouldReturnError: false,
		},
		{
			message:           "with invalid sorting field returns an error",
			sorting:           &query.Sorting{Field: "not-valid-field"},
			expectedSortField: "status",
			expectedSortAsc:   true,
			shouldReturnError: true,
			errMsg:            "Invalid sort field 'not-valid-field'.",
		},
		{
			message:           "with invalid sorting field returns an error",
			sorting:           &query.Sorting{Field: "name"},
			expectedSortField: "status",
			expectedSortAsc:   true,
			shouldReturnError: true,
			errMsg:            "Invalid sort field 'name'.",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actualSortField, actualSortAsc, err := subject.GetSortParamsForServices(test.sorting)

			if test.shouldReturnError {
				assert.NotNil(t, err)
				assert.Contains(t, err.Error(), test.errMsg)
			} else {
				assert.Nil(t, err)
			}

			assert.Equal(t, test.expectedSortField, actualSortField)
			assert.Equal(t, test.expectedSortAsc, actualSortAsc)
		})
	}
}
