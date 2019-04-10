package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParseConfig(t *testing.T) {
	t.Run("no such file", func(t *testing.T) {
		_, err := parseConfig("this-config-file-should-not-exist-2018-04.toml")
		assert.Contains(t, err.Error(), "unable to read config file")
	})

	t.Run("unable to parse", func(t *testing.T) {
		_, err := parseConfig("testdata/bad_xml.toml")
		assert.Contains(t, err.Error(), "unable to read config file")
	})

	t.Run("bad fields", func(t *testing.T) {
		_, err := parseConfig("testdata/missing_fields.toml")
		assert.Contains(t, err.Error(), "missing required config key")
	})

	t.Run("good", func(t *testing.T) {
		cfg, err := parseConfig("testdata/good.toml")
		require.NoError(t, err)
		assert.Equal(t, cfg.ListenHost, "localhost")
		assert.Equal(t, cfg.ListenPort, "10165")
		assert.Equal(t, cfg.LicGenURL, "https://lic-gen-server.com")
		assert.Equal(t, cfg.LicGenAuthToken, "abc-sesame")
		assert.Equal(t, cfg.SegmentIOWriteKey, "def-sesame")
		assert.Equal(t, cfg.LogFormat, "json")
		assert.Equal(t, cfg.LogLevel, "info")
	})
}
