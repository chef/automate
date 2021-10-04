package stringutils_test

import (
	"testing"

	"github.com/chef/automate/lib/stringutils"
	"github.com/stretchr/testify/assert"
)

func TestTrimAfter(t *testing.T) {
	tests := []struct {
		source   string
		find     string
		expected string
	}{
		{source: "a:b", find: ":", expected: "a:"},
		{source: ":b", find: ":", expected: ":"},
		{source: "a:", find: ":", expected: "a:"},
		{source: "ab", find: ":", expected: "ab"},
	}

	for _, test := range tests {
		actual := stringutils.TrimAfter(test.source, test.find)

		assert.Equal(t, test.expected, actual)
	}
}

func TestTrimBefore(t *testing.T) {
	tests := []struct {
		source   string
		find     string
		expected string
	}{
		{source: "a:b", find: ":", expected: ":b"},
		{source: ":b", find: ":", expected: ":b"},
		{source: "a:", find: ":", expected: ":"},
		{source: "ab", find: ":", expected: "ab"},
	}

	for _, test := range tests {
		actual := stringutils.TrimBefore(test.source, test.find)

		assert.Equal(t, test.expected, actual)
	}
}
