package commands

import (
	"fmt"
	"time"

	"github.com/spf13/cobra"
)

type serveCmdFlagSet struct {
	configFile string
}

var serveCmdFlags serveCmdFlagSet

func newServeCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "serve",
		Short: "Run notifications-service server",
		RunE:  runServeCommand,
	}
	c.PersistentFlags().StringVar(
		&serveCmdFlags.configFile,
		"config",
		"",
		"path to config file",
	)
	return c
}

func runServeCommand(cmd *cobra.Command, args []string) error {
	for {
		fmt.Println("hello from notifications-service2")
		time.Sleep(5 * time.Second)
	}
	// Tasks:
	// - implement config. parse the toml file
	// - load our service certs
	// - connect to postgres
	// - migrate database to be identical to elixir version, but use the golang migrator.
	//   - fresh install case
	//   - upgrade case
	// - start the gRPC server
	return nil
}
