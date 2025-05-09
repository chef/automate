package main

import (
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/stretchr/testify/assert"
)

func TestRunGetExternalDbIpCmd(t *testing.T) {
	runGetExternalDbIpCmd(cmd, []string{CONFIG_PATH + "/config_with_only_db.toml"})
	tomlbyte, _ := fileutils.ReadFile(CONFIG_PATH + "/config_with_only_db.toml")
	configString := string(tomlbyte)
	var config dc.AutomateConfig
	toml.Decode(configString, &config)
	assert.NotEmpty(t, config.Global.V1.External.Opensearch.Nodes)
	assert.NotEmpty(t, config.Global.V1.External.Postgresql.Nodes)
}
