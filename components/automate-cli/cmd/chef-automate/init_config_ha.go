// Copyright © 2017 Chef Software

package main

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var initConfigHAPathFlags = struct {
	path string
}{}

func init() {
	initConfigHACmd.PersistentFlags().StringVar(
		&initConfigHAPathFlags.path,
		"file",
		"config.toml",
		"File path to write the config")

	RootCmd.AddCommand(initConfigHACmd)
}

var initConfigHACmd = &cobra.Command{
	Use:   "init-config-ha",
	Short: "Initialize default config for HA",
	Long:  "Initializd default configuration for HA and save it to a file.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runInitConfigHACmd,
}

func runInitConfigHACmd(cmd *cobra.Command, args []string) error {
	if len(args) == 0 {
		msg := "one argument expected as any of deplyment mode like aws or existing_node. "
		writer.Printf(msg)
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	} else if args[0] == "aws" {
		writer.Printf("Generating initial automate high availability configuration for AWS deployment")
		return runInitConfigAwsHACmd()
	} else if args[0] == "existing_node" {
		writer.Printf("Generating initial automate high availability configuration for existing infra nodes deployment")
		return runInitConfigExistingNodeHACmd()
	} else {
		msg := "Incorrect argument expected is any of deplyment mode like aws or existing_node. "
		writer.Printf(msg)
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	}
}
