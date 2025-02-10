package main

import (
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

const CONFIG_PATH = "../../pkg/testfiles/onprem"

var cmd = &cobra.Command{}

func TestRunEncodePasswordCmd(t *testing.T) {
	runEncodePasswordCmd(cmd, []string{CONFIG_PATH + "/config_externaldb.toml"})
	tomlbyte, _ := fileutils.ReadFile(CONFIG_PATH + "/config_externaldb.toml")
	configString := string(tomlbyte)
	var config dc.AutomateConfig
	toml.Decode(configString, &config)
	assert.Equal(t, "YWRtaW4=", config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value)
	assert.Equal(t, "YWRtaW4=", config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password.Value)
	assert.Equal(t, "YWRtaW4=", config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value)
	runDecodePasswordCmd(cmd, []string{CONFIG_PATH + "/config_externaldb.toml"})
}