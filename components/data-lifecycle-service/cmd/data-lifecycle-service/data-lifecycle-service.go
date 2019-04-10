package main

import (
	"context"
	"fmt"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/data-lifecycle-service/server"
	"github.com/chef/automate/lib/tracing"
)

func commandRoot() *cobra.Command {
	rootCmd := &cobra.Command{
		Use:   "data-lifecycle-service [COMMAND]",
		Short: "Chef Data Lifecycle Service",
	}
	rootCmd.AddCommand(commandServe())
	return rootCmd
}

func commandServe() *cobra.Command {
	return &cobra.Command{
		Use:   "serve [CONFIG_FILE]",
		Short: "Start Data Lifecycle Service",
		Long:  "Start the Chef Data Lifecycle Service",
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			closer, err := tracing.NewGlobalTracer("data-lifecycle-service")
			if err == nil {
				defer tracing.CloseQuietly(closer)
			}

			if err := doServe(cmd, args); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
		},
	}
}

func doServe(cmd *cobra.Command, args []string) error {
	config, err := server.ConfigFromToml(args[0])
	if err != nil {
		return errors.Wrap(err, "Could not load configuration file")
	}

	return server.StartServer(context.Background(), config)
}

func main() {
	if err := commandRoot().Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
