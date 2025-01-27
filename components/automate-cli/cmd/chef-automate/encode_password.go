package main

import (
	"encoding/base64"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/spf13/cobra"
)

var encodePasswordCmdFlags = struct {
	config string
}{}

var encodePasswordCmd = &cobra.Command{
	Use:    "encode-password [/path/to/config.toml]",
	Short:  "Encodes the password fields",
	Long:   "Encodes the password fields in the specified config.toml file",
	RunE:   runEncodePasswordCmd,
	Args:   cobra.ExactArgs(1),
	Hidden: true,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func init() {
	RootCmd.AddCommand(encodePasswordCmd)
	encodePasswordCmd.PersistentFlags().StringVarP(
		&encodePasswordCmdFlags.config,
		"config",
		"c",
		"",
		"Config file that needs to be updated with encoded passwords")

}

func runEncodePasswordCmd(cmd *cobra.Command, args []string) error {
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
					if config.Global.V1.External.Postgresql != nil && config.Global.V1.External.Postgresql.Auth != nil && config.Global.V1.External.Postgresql.Auth.Password != nil {
						if config.Global.V1.External.Postgresql.Auth.Password.Superuser != nil && config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password != nil {
							superUserPassword := config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password.Value
							if superUserPassword != "" {
								superUserPassword = base64.StdEncoding.EncodeToString([]byte(superUserPassword))
								config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password.Value = superUserPassword
							}
						}
						if config.Global.V1.External.Postgresql.Auth.Password.Dbuser != nil && config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password != nil {
							dbUserPassword := config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value
							if dbUserPassword != "" {
								dbUserPassword = base64.StdEncoding.EncodeToString([]byte(dbUserPassword))
								config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value = dbUserPassword
							}
						}
					}
					if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Auth != nil && config.Global.V1.External.Opensearch.Auth.BasicAuth != nil && config.Global.V1.External.Opensearch.Auth.BasicAuth.Password != nil {
						userPassword := config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value
						if userPassword != "" {
							userPassword = base64.StdEncoding.EncodeToString([]byte(userPassword))
							config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value = userPassword
						}
					}
					_, err := fileutils.CreateTomlFileFromConfig(&config, configFile)
					if err != nil {
						return err
					}

				}
			}
		}
	}
	return nil
}
