package params_test

import (
	"fmt"
	"testing"

	"github.com/chef/automate/api/external/common/query"
	subject "github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/stretchr/testify/assert"
)

func TestPaginationDefaults(t *testing.T) {
	var (
		pagination             = &query.Pagination{}
		expectedPage     int32 = 1
		expectedPageSize int32 = 25
	)
	actualPage, actualPageSize := subject.GetPageParams(pagination)
	assert.Equal(t, expectedPage, actualPage)
	assert.Equal(t, expectedPageSize, actualPageSize)
}

func TestPaginationNilQueryReturnsDefaults(t *testing.T) {
	var (
		expectedPage     int32 = 1
		expectedPageSize int32 = 25
	)
	actualPage, actualPageSize := subject.GetPageParams(nil)
	assert.Equal(t, expectedPage, actualPage)
	assert.Equal(t, expectedPageSize, actualPageSize)
}

func TestPaginationMatrix(t *testing.T) {
	cases := []struct {
		pagination   *query.Pagination
		expectedPage int32
		expectedSize int32
	}{
		{
			pagination: &query.Pagination{
				Page: 123,
				Size: 50,
			},
			expectedPage: 123,
			expectedSize: 50,
		},
		{
			pagination: &query.Pagination{
				Page: 5234233,
				Size: 1234567,
			},
			expectedPage: 5234233,
			expectedSize: 1234567,
		},
		{
			pagination:   &query.Pagination{Page: 3},
			expectedPage: 3,
			expectedSize: 25,
		},
		{
			pagination:   &query.Pagination{Size: 3},
			expectedPage: 1,
			expectedSize: 3,
		},
		{
			pagination:   nil,
			expectedPage: 1,
			expectedSize: 25,
		},
		{
			pagination: &query.Pagination{
				Page: -1,
				Size: -1,
			},
			expectedPage: 1,
			expectedSize: 25,
		},
		{
			pagination: &query.Pagination{
				Page: -1,
				Size: 2,
			},
			expectedPage: 1,
			expectedSize: 2,
		},
		{
			pagination: &query.Pagination{
				Page: 0,
				Size: 0,
			},
			expectedPage: 1,
			expectedSize: 25,
		},
		{
			pagination: &query.Pagination{
				Page: 9,
				Size: -10,
			},
			expectedPage: 9,
			expectedSize: 25,
		},
	}

	for i, test := range cases {
		t.Run(fmt.Sprintf("test-%d", i), func(t *testing.T) {
			actualPage, actualPageSize := subject.GetPageParams(test.pagination)
			assert.Equal(t, test.expectedPage, actualPage)
			assert.Equal(t, test.expectedSize, actualPageSize)
		})
	}
}
