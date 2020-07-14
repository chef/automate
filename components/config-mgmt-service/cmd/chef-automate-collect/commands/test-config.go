package commands

import (
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var testConfigCommandFlags = struct {
	verbose bool
}{}

func newTestConfigCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "test-config",
		Short: "Make a request to the test API",
		RunE:  runTestConfigCommand,
	}
	c.Flags().BoolVarP(&testConfigCommandFlags.verbose,
		"verbose",
		"v",
		false,
		"enable verbose output (goes to stderr)",
	)
	return c
}

func runTestConfigCommand(cmd *cobra.Command, args []string) error {
	cliIO.EnableVerbose = testConfigCommandFlags.verbose

	loader := NewConfigLoader()
	err := loader.Load()
	if err != nil {
		return errors.Wrap(err, "failed to load configuration")
	}

	automateConfig := loader.LoadedConfig.Automate

	err = automateConfig.Test()
	if err != nil {
		return err
	}
	cliIO.msg("SUCCESS: Configuration Verified")

	return nil
}
