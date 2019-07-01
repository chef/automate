package server

import (
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/stretchr/testify/assert"
)

func TestConvertHealthStatusToProtoMatrix(t *testing.T) {
	cases := []struct {
		message  string
		health   string
		expected applications.HealthStatus
	}{
		{
			message:  "Status OK",
			health:   "OK",
			expected: applications.HealthStatus_OK,
		},
		{
			message:  "Status CRITICAL",
			health:   "CRITICAL",
			expected: applications.HealthStatus_CRITICAL,
		},
		{
			message:  "Status WARNING",
			health:   "WARNING",
			expected: applications.HealthStatus_WARNING,
		},
		{
			message:  "Status NONE",
			health:   "NONE",
			expected: applications.HealthStatus_NONE,
		},
		{
			message:  "Status UNKNOWN",
			health:   "UNKNOWN",
			expected: applications.HealthStatus_UNKNOWN,
		},
		{
			message:  "any other status returns UNKNOWN",
			health:   "FOO",
			expected: applications.HealthStatus_UNKNOWN,
		},
		{
			message:  "empty returns UNKNOWN",
			health:   "",
			expected: applications.HealthStatus_UNKNOWN,
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := convertHealthStatusToProto(test.health)
			assert.Equal(t, test.expected, actual)
		})
	}
}
