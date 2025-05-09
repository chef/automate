package main

import (
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/stretchr/testify/assert"
)

func TestRunDecodePasswordCmd(t *testing.T) {
	runDecodePasswordCmd(cmd, []string{CONFIG_PATH + "/config_with_only_db.toml"})
	tomlbyte, _ := fileutils.ReadFile(CONFIG_PATH + "/config_with_only_db.toml")
	configString := string(tomlbyte)
	var config dc.AutomateConfig
	toml.Decode(configString, &config)
	assert.Equal(t, "admin", config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value)
	assert.Equal(t, "admin", config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password.Value)
	assert.Equal(t, "admin", config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value)
}