package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var configCmdFlags = struct {
	overwriteFile bool
	timeout       int64
}{}

func init() {
	configCmd.AddCommand(showConfigCmd)
	configCmd.AddCommand(patchConfigCmd)
	configCmd.AddCommand(setConfigCmd)

	showConfigCmd.Flags().BoolVarP(&configCmdFlags.overwriteFile, "overwrite", "o", false, "Overwrite existing config.toml")

	configCmd.PersistentFlags().Int64VarP(&configCmdFlags.timeout, "timeout", "t", 10, "Request timeout in seconds")

	RootCmd.AddCommand(configCmd)
}

var configCmd = &cobra.Command{
	Use:   "config COMMAND",
	Short: "Chef Automate configuration",
}

var showConfigCmd = &cobra.Command{
	Use:   "show [/path/to/write/config.toml]",
	Short: "show the Chef Automate configuration",
	Long:  "Show the Chef Automate configuration. When given a filepath, the output will be written to the file instead of printed to STDOUT",
	RunE:  runShowCmd,
	Args:  cobra.RangeArgs(0, 1),
}

var patchConfigCmd = &cobra.Command{
	Use:   "patch path/to/config.toml",
	Short: "patch the Chef Automate configuration", Long: "Apply a partial Chef Automate configuration to the deployment. It will take the partial configuration, merge it with the existing configuration, and apply and required changes.",
	RunE: runPatchCommand,
	Args: cobra.ExactArgs(1),
}

var setConfigCmd = &cobra.Command{
	Use:   "set path/to/config.toml",
	Short: "set the Chef Automate configuration",
	Long:  "Set the Chef Automate configuration for the deployment. It will replace the Chef Automate configuration with the given configuration and apply any required changes.",
	RunE:  runSetCommand,
	Args:  cobra.ExactArgs(1),
}

func runShowCmd(cmd *cobra.Command, args []string) error {
	res, err := client.GetAutomateConfig(configCmdFlags.timeout)
	if err != nil {
		return err
	}

	// TODO: should this be done server side?
	res.Config.Redact()

	// Handle writing to a file if a path was given
	if len(args) > 0 && args[0] != "" {
		outFile, err := filepath.Abs(args[0])
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}

		if _, err := os.Stat(outFile); err == nil {
			if !configCmdFlags.overwriteFile {
				ok, err := writer.Confirm(fmt.Sprintf("%s file already exists. Do you wish to overwrite it?", outFile))
				if err != nil || !ok {
					if !ok {
						err = errors.New("failed to confirm overwrite")
					}

					return status.Annotate(err, status.FileAccessError)
				}
			}
		}

		err = res.Config.MarshalToTOMLFile(outFile, 0644)
		if err != nil {
			return status.Wrap(
				err,
				status.MarshalError,
				"Marshaling configuration to config.toml failed",
			)
		}

		writer.Successf("Chef Automate configuration file written to: %s", outFile)
		return nil
	}

	t, err := res.Config.MarshalToTOML()
	if err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML failed",
		)
	}
	status.GlobalResult = res.Config

	writer.Println(string(t))
	return nil
}
func runPatchCommand(cmd *cobra.Command, args []string) error {
	cfg, err := dc.LoadUserOverrideConfigFile(args[0])
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	if err = client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}

	writer.Success("Configuration patched")
	return nil
}

func runSetCommand(cmd *cobra.Command, args []string) error {
	cfg, err := dc.LoadUserOverrideConfigFile(args[0])
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	if err := client.SetAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}

	writer.Success("Configuration set")
	return nil
}
