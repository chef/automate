package commands

import (
	"os"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func newRootCmd() *cobra.Command {
	r := &cobra.Command{
		Use:   "chef-automate-collect",
		Short: "Collect data for Chef Automate",
	}
	r.AddCommand(newGenConfigCommand())
	r.AddCommand(newShowConfigCommand())
	r.AddCommand(newTestConfigCommand())
	r.AddCommand(newDescribeCmd())
	r.AddCommand(newReportNewRolloutCommand())
	return r
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := newRootCmd().Execute(); err != nil {
		logrus.Error(err)
		os.Exit(1)
	}
}

func init() {
}
