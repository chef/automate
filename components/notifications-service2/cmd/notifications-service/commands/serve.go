package commands

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/components/notifications-service2/pkg/config"
	"github.com/chef/automate/components/notifications-service2/pkg/storage/postgres"
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
	c, err := config.FromFile(serveCmdFlags.configFile)
	if err != nil {
		return err
	}
	_, err = postgres.Start(c)
	if err != nil {
		return err
	}
	for {
		fmt.Println("hello from notifications-service2")
		s, _ := toml.Marshal(c)
		fmt.Println(string(s))
		time.Sleep(5 * time.Second)
	}
	return nil
}
