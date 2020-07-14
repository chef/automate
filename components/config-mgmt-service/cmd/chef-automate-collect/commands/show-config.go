package commands

import (
	"os"

	"github.com/BurntSushi/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var showConfigCommands = struct {
	verbose       bool
	privateConfig bool
}{}

func newShowConfigCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "show-config",
		Short: "Load config files and environment variables and show the resulting configuration",
		RunE:  runShowConfigCommand,
	}
	c.Flags().BoolVarP(&showConfigCommands.verbose,
		"verbose",
		"v",
		false,
		"enable verbose output (goes to stderr)",
	)
	c.Flags().BoolVarP(&showConfigCommands.privateConfig,
		"private",
		"p",
		false,
		"show unredacted credentials",
	)
	return c
}

func runShowConfigCommand(cmd *cobra.Command, args []string) error {
	cliIO.EnableVerbose = showConfigCommands.verbose

	loader := NewConfigLoader()
	err := loader.Load()
	if err != nil {
		return errors.Wrap(err, "failed to load configuration")
	}
	cliIO.out("Found config files:")
	cliIO.out("-------------------")
	for _, name := range loader.ViableConfigPaths() {
		cliIO.out("  %s", name)
	}
	cliIO.out("")

	cliIO.out("Computed configuration:")
	cliIO.out("-----------------------\n\n")

	enc := toml.NewEncoder(os.Stdout)
	if showConfigCommands.privateConfig {
		enc.Encode(loader.LoadedConfig.WithPrivate())
	} else {
		enc.Encode(loader.LoadedConfig.Redacted())
	}
	return nil
}
