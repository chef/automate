package main

import (
	"fmt"
	"strconv"
	"strings"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/spf13/cobra"
)

var getExternalDbIpCmdFlags = struct {
	config string
}{}

func init() {
	RootCmd.AddCommand(getExternalDbIpCmd)
	getExternalDbIpCmd.PersistentFlags().StringVarP(
		&getExternalDbIpCmdFlags.config,
		"config",
		"c",
		"",
		"Config file that needs to be updated with backend ip address")
}

var getExternalDbIpCmd = &cobra.Command{
	Use:    "get-external-db-ips [/path/to/config.toml]",
	Short:  "Extracts the nodes field for external databases to another config file",
	Long:   "Extract the nodes field for external databases (PostgreSQL and OpenSearch) to another config file from the specified config.toml file",
	RunE:   runGetExternalDbIpCmd,
	Args:   cobra.ExactArgs(1),
	Hidden: true,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func runGetExternalDbIpCmd(cmd *cobra.Command, args []string) error {
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
					var postgresNodes, opensearchNodes string
					if config.Global.V1.External.Postgresql != nil && config.Global.V1.External.Postgresql.Nodes != nil {
						postgresNodesList := config.Global.V1.External.Postgresql.GetNodes()
						if len(postgresNodesList) > 0 {
							postgresNodesSlice := make([]string, len(postgresNodesList))
							for i, node := range postgresNodesList {
								if node != nil {
									postgresNodesSlice[i] = strconv.Quote(node.Value)
								}
							}
							postgresNodes = strings.Join(postgresNodesSlice, ",")
						}

					}
					if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Nodes != nil {
						opensearchNodesList := config.Global.V1.External.Opensearch.GetNodes()
						if len(opensearchNodesList) > 0 {
							opensearchNodesSlice := make([]string, len(opensearchNodesList))
							for i, node := range opensearchNodesList {
								if node != nil {
									opensearchNodesSlice[i] = strconv.Quote(node.Value)
								}
							}
							opensearchNodes = strings.Join(opensearchNodesSlice, ",")
						}
					}
					var tomlFileContent string
					if len(postgresNodes) > 0 && len(opensearchNodes) > 0 {
						tomlFileContent = fmt.Sprintf(`
					[global.v1.external.postgresql]
        				nodes = [%v]
 					[global.v1.external.opensearch]
        				nodes = [%v]`, postgresNodes, opensearchNodes)
					} else if len(postgresNodes) > 0 && len(opensearchNodes) <= 0 {
						tomlFileContent = fmt.Sprintf(`
					[global.v1.external.postgresql]
        				nodes = [%v]`, postgresNodes)
					} else if len(postgresNodes) <= 0 && len(opensearchNodes) > 0 {
						tomlFileContent = fmt.Sprintf(`
					[global.v1.external.opensearch]
						nodes = [%v]`, opensearchNodes)
					} else {
						tomlFileContent = ""
					}
					if len(tomlFileContent) > 0 {
						externalDbIpsConfig := "externalDbIpsConfig.toml"
						extractedConfig, err := fileutils.CreateTempFile(tomlFileContent, externalDbIpsConfig, "")
						if err != nil {
							return err
						}
						er := fileutils.Move(extractedConfig, "/hab/externalDbIpsConfig.toml")
						if er != nil {
							return er
						}
					}
				}
			}
		}
	}
	return nil
}
