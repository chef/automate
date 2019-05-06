package params_test

import (
	"fmt"
	"testing"

	subject "github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/stretchr/testify/assert"
)

func TestFormatFiltersEmptyArray(t *testing.T) {
	var (
		empty    = make([]string, 0)
		expected = make(map[string][]string, 0)
	)
	filters, err := subject.FormatFilters(empty)
	assert.Nil(t, err)
	assert.Equal(t, expected, filters)
}

func TestFormatFiltersMatrixOfFilters(t *testing.T) {
	cases := []struct {
		message  string
		filters  []string
		expected map[string][]string
	}{
		{
			message: "with a single unique filter (1:1)",
			filters: []string{"platform:centos"},
			expected: map[string][]string{
				"platform": {"centos"},
			},
		},
		{
			message: "with a single filter repeated multiple times (1:*)",
			filters: []string{
				"platform:centos",
				"platform:ubuntu",
				"platform:redhat",
			},
			expected: map[string][]string{
				"platform": {"centos", "ubuntu", "redhat"},
			},
		},
		{
			message: "with multiple unique filters (*:1)",
			filters: []string{
				"status:critical",
				"service_name:redis",
				"application:cafe",
				"environment:prod",
			},
			expected: map[string][]string{
				"status":       {"critical"},
				"service_name": {"redis"},
				"application":  {"cafe"},
				"environment":  {"prod"},
			},
		},
		{
			message: "with multiple filters repeated multiple times (*:*)",
			filters: []string{
				"status:critical",
				"service_name:redis",
				"application:cafe-us",
				"environment:prod",
				"status:warning",
				"service_name:postgres",
				"application:cafe-europe",
				"application:cafe-asia",
				"service_name:myapp",
			},
			expected: map[string][]string{
				"status":       {"critical", "warning"},
				"service_name": {"redis", "postgres", "myapp"},
				"application":  {"cafe-us", "cafe-europe", "cafe-asia"},
				"environment":  {"prod"},
			},
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			filters, err := subject.FormatFilters(test.filters)
			assert.Nil(t, err)
			assert.Equal(t, test.expected, filters)
		})
	}
}

func TestFormatFiltersWrongFilters(t *testing.T) {
	cases := []struct {
		filters []string
	}{
		{[]string{"platform=centos"}},
		{[]string{"wrong"}},
		{[]string{":success"}},
		{[]string{"platform:"}},
		{[]string{"platform:foo:bar"}},
		{[]string{"platform:%20"}}, // space
		{[]string{"platform:%09"}}, // tab
		{[]string{"platform:%0A"}}, // new line
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with filter(s) %v it should return an error", test.filters), func(t *testing.T) {
			filters, err := subject.FormatFilters(test.filters)

			assert.NotNil(t, err)
			assert.Contains(t, err.Error(), "Invalid filter")
			assert.Contains(t, err.Error(), "format: key:value")
			assert.Nil(t, filters)
		})
	}
}
