package config_test

import (
	"testing"

	"github.com/chef/automate/lib/config"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParseNameServers(t *testing.T) {
	cases := []struct {
		desc     string
		content  []byte
		expected []string
	}{
		{
			desc: "parses IPv4 nameserver",
			content: []byte(`
nameserver 10.0.0.3
	`),
			expected: []string{"10.0.0.3"},
		},
		{
			desc: "parses IPv6 nameserver",
			content: []byte(`
	nameserver 2001:0db8:85a3:0000:0000:8a2e:0370:7334
	`),
			expected: []string{"2001:0db8:85a3:0000:0000:8a2e:0370:7334"},
		},
		{
			desc: "parses spaces between nameserver and IP",
			content: []byte(`
nameserver                          10.0.0.3
	`),
			expected: []string{"10.0.0.3"},
		},
		{
			desc: "parses multiple nameservers",
			content: []byte(`
	nameserver 10.0.0.3
	nameserver 10.0.0.4
	`),
			expected: []string{"10.0.0.3", "10.0.0.4"},
		},
		{
			desc: "parses content with single-line comments",
			content: []byte(`
	# Here goes the name servers
	nameserver 10.0.0.3
	`),
			expected: []string{"10.0.0.3"},
		},
		{
			desc: "parses content with same-line comments",
			content: []byte(`
	nameserver 10.0.0.3 # Here goes the name servers
	`),
			expected: []string{"10.0.0.3"},
		},
	}

	for _, c := range cases {
		t.Run(c.desc, func(t *testing.T) {
			ns := config.ParseNameServers(c.content)
			assert.Equal(t, c.expected, ns)

		})
	}
}

func TestGetNameServersFromResolveConfig(t *testing.T) {
	var filePath = "./testdata/sample_resolv_conf"
	ns, err := config.GetNameServersFromResolveConfig(filePath)
	require.NoError(t, err, "failed to parse config")

	var expectedResult = []string{"255.0.0.1"}

	require.Equal(t, expectedResult, ns)
}
