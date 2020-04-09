package grpcserver_test

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/config-mgmt-service/grpcserver"
	"github.com/stretchr/testify/assert"
)

func TestValidDurations(t *testing.T) {

	cases := []struct {
		description string
		durations   []string
		expected    bool
	}{
		{
			description: "Empty",
			durations:   []string{},
			expected:    true,
		},
		{
			description: "hours valid",
			durations:   []string{"1h", "1h", "2h", "12h", "359h"},
			expected:    true,
		},
		{
			description: "days valid",
			durations:   []string{"1d", "3d", "12d", "359d"},
			expected:    true,
		},
		{
			description: "weeks valid",
			durations:   []string{"1w", "3w", "4w", "53w"},
			expected:    true,
		},
		{
			description: "months valid",
			durations:   []string{"1M", "4M", "2M", "13M"},
			expected:    true,
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {
			actual := grpcserver.ValidDurations(testCase.durations)
			assert.Equal(t, testCase.expected, actual, "%v", testCase.durations)
		})
	}
}

func TestValidDurationsNot(t *testing.T) {

	durations := []string{"h1", "1y", "1h2", "3H*", "1Q", "3p", "4.1w", "1H", "1hy"}

	for _, duration := range durations {
		t.Run(fmt.Sprintf("invalid %s", duration), func(t *testing.T) {
			actual := grpcserver.ValidDurations([]string{duration})
			assert.False(t, actual)
		})
	}
}
