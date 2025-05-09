package main

import (
	"fmt"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/spf13/cobra"
)

var updateBackendIpCmdFlags = struct {
	config string
}{}

func init() {
	RootCmd.AddCommand(updateBackendIpCmd)
	updateBackendIpCmd.PersistentFlags().StringVarP(
		&updateBackendIpCmdFlags.config,
		"config",
		"c",
		"",
		"Config file that needs to be updated with backend ip address")
}

var updateBackendIpCmd = &cobra.Command{
	Use:    "update-backend-ip [/path/to/config.toml]",
	Short:  "Updates the nodes field for postgresql and opensearch in the config.toml file",
	Long:   "Updates the nodes field for postgresql and opensearch in the config.toml file",
	RunE:   runUpdateBackendIpCmd,
	Args:   cobra.ExactArgs(1),
	Hidden: true,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func runUpdateBackendIpCmd(cmd *cobra.Command, args []string) error {
	if len(args) > 0 {
		configFile := args[0]
		if len(configFile) > 0 {
			if checkIfFileExist(configFile) {
				tomlbyte, _ := fileutils.ReadFile(configFile) // nosemgrep
				configString := string(tomlbyte)
				var config dc.AutomateConfig
				if _, err := toml.Decode(configString, &config); err != nil {
					return err
				}
				if config.Global != nil && config.Global.V1 != nil && config.Global.V1.External != nil {
					var postgresNodes, opensearchNodes []string
					if config.Global.V1.External.Postgresql != nil && config.Global.V1.External.Postgresql.Nodes != nil {
						postgresNodesList := config.Global.V1.External.Postgresql.GetNodes()
						postgresNodes = make([]string, len(postgresNodesList))
						for i, node := range postgresNodesList {
							if node != nil {
								postgresNodes[i] = node.Value
							}
						}

					}
					if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Nodes != nil {
						opensearchNodesList := config.Global.V1.External.Opensearch.GetNodes()
						opensearchNodes = make([]string, len(opensearchNodesList))
						for i, node := range opensearchNodesList {
							if node != nil {
								opensearchNodes[i] = node.Value
							}
						}
					}
					tomlFileContent := fmt.Sprintf(`
					[global.v1.external.postgresql]
        				nodes = %v
 					[global.v1.external.opensearch]
        				nodes = %v`, postgresNodes, opensearchNodes)

					backend_Ip := "backendIpPatch.toml"
					patchConfig, err := fileutils.CreateTempFile(tomlFileContent, backend_Ip, "")
					if err != nil {
						return err
					}
					er := fileutils.Move(patchConfig, "/hab/backendIpPatch.toml")
					if er != nil {
						return er
					}
				}
			}
		}
	}
	return nil
}
