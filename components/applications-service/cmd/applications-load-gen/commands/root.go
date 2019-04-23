package commands

import (
	"os"

	"github.com/spf13/cobra"
)

var rootFlags struct {
	profileFile       string
	useDefaultProfile bool
}

// RootCmd is the command runner.
var RootCmd = &cobra.Command{
	Use:          "applications-load-gen",
	Short:        "Load Generator for Chef Automate Applications Service",
	SilenceUsage: true,
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		os.Exit(-1)
	}
}

func init() {
	// global config
	RootCmd.PersistentFlags().StringVarP(
		&rootFlags.profileFile,
		"profile",
		"p",
		"",
		"file that configures the shape of the load to be generated",
	)
	RootCmd.PersistentFlags().BoolVar(
		&rootFlags.useDefaultProfile,
		"use-default-profile",
		false,
		"use simple builtin load profile",
	)

	RootCmd.AddCommand(newDescribeCmd())
	RootCmd.AddCommand(newRunCmd())
}
