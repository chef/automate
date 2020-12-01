package config_test

import (
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/stretchr/testify/require"
)

func TestParseNameServers(t *testing.T) {
	filecontent := []byte(`
	nameserver 10.0.0.3
	`)

	ns := config.ParseNameServers(filecontent)
	expectedResult := []string{"10.0.0.3"}

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}

func TestParseNameServersWithIPv6Address(t *testing.T) {
	filecontent := []byte(`
	nameserver 2001:0db8:85a3:0000:0000:8a2e:0370:7334
	`)

	ns := config.ParseNameServers(filecontent)
	expectedResult := []string{"2001:0db8:85a3:0000:0000:8a2e:0370:7334"}

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}

func TestParseNameServersWithMultipleNameServers(t *testing.T) {
	filecontent := []byte(`
	nameserver 10.0.0.3
	nameserver 10.0.0.4
	`)

	ns := config.ParseNameServers(filecontent)
	expectedResult := []string{"10.0.0.3", "10.0.0.4"}

	require.Equal(t, len(expectedResult), len(ns))

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}

func TestParseNameServersWithNewLineComment(t *testing.T) {
	filecontent := []byte(`
	# Here goes the name servers
	nameserver 10.0.0.3
	`)

	ns := config.ParseNameServers(filecontent)
	expectedResult := []string{"10.0.0.3"}

	require.Equal(t, len(expectedResult), len(ns))

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}

func TestParseNameServersWithInLineComment(t *testing.T) {
	filecontent := []byte(`
	nameserver 10.0.0.3 # Here goes the name servers
	`)

	ns := config.ParseNameServers(filecontent)
	expectedResult := []string{"10.0.0.3"}

	require.Equal(t, len(expectedResult), len(ns))

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}

func TestGetNameServersFromResolveConfig(t *testing.T) {
	var filePath = "./testdata/sample_resolv_conf"
	ns, e := config.GetNameServersFromResolveConfig(filePath)

	if e != nil {
		t.Fatalf("Failed to parse config file: %v", e.Error())
	}

	var expectedResult = []string{"255.0.0.1"}

	require.Equal(t, len(expectedResult), len(ns))

	for _, m := range expectedResult {
		require.Contains(t, ns, m)
	}
}
