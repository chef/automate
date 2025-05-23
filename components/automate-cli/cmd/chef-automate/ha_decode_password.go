package main

import (
	"encoding/base64"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/toml"
	"github.com/spf13/cobra"
)

var decodePasswordCmdFlags = struct {
	config string
}{}

func init() {
	RootCmd.AddCommand(decodePasswordCmd)
	decodePasswordCmd.PersistentFlags().StringVarP(
		&decodePasswordCmdFlags.config,
		"config",
		"c",
		"",
		"Config file that needs to be updated with decoded passwords")
}

var decodePasswordCmd = &cobra.Command{
	Use:    "decode-password [/path/to/config.toml]",
	Short:  "Decodes the password fields",
	Long:   "Decodes the password fields in the specified config.toml file",
	RunE:   runDecodePasswordCmd,
	Args:   cobra.ExactArgs(1),
	Hidden: true,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func runDecodePasswordCmd(cmd *cobra.Command, args []string) error {
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
								superUserPswd, decErr := base64.StdEncoding.DecodeString(superUserPassword)
								if decErr != nil {
									return decErr
								}
								config.Global.V1.External.Postgresql.Auth.Password.Superuser.Password.Value = string(superUserPswd)
							}
						}
						if config.Global.V1.External.Postgresql.Auth.Password.Dbuser != nil && config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password != nil {
							dbUserPassword := config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value
							if dbUserPassword != "" {
								dbUserPswd, decErr := base64.StdEncoding.DecodeString(dbUserPassword)
								if decErr != nil {
									return decErr
								}
								config.Global.V1.External.Postgresql.Auth.Password.Dbuser.Password.Value = string(dbUserPswd)
							}
						}
					}
					if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Auth != nil && config.Global.V1.External.Opensearch.Auth.Scheme != nil {
						if config.Global.V1.External.Opensearch.Auth.Scheme.Value == "basic_auth" && config.Global.V1.External.Opensearch.Auth.BasicAuth != nil && config.Global.V1.External.Opensearch.Auth.BasicAuth.Password != nil {
							password := config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value
							decodedPassword, err := decodeString(password)
							if err == nil && decodedPassword != "" {
								config.Global.V1.External.Opensearch.Auth.BasicAuth.Password.Value = decodedPassword
							}
						}else if config.Global.V1.External.Opensearch.Auth.Scheme.Value == "aws_os" && config.Global.V1.External.Opensearch.Auth.AwsOs != nil && config.Global.V1.External.Opensearch.Auth.AwsOs.Password != nil {
							password := config.Global.V1.External.Opensearch.Auth.AwsOs.Password.Value
							decodedPassword, err := decodeString(password)
							if err == nil && decodedPassword != "" {
								config.Global.V1.External.Opensearch.Auth.AwsOs.Password.Value = decodedPassword
								
							}
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

func decodeString (encodedStr string) (string, error) {
	decodedStr, decErr := base64.StdEncoding.DecodeString(encodedStr)
	if decErr != nil {
		return "", decErr
	}
	return string(decodedStr), nil
}