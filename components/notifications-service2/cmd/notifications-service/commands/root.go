package commands

import (
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func RootCmd() *cobra.Command {
	r := &cobra.Command{
		Use:   "applications-service",
		Short: "Chef Automate Applications Service",
	}
	r.AddCommand(newServeCommand())
	return r
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd().Execute(); err != nil {
		log.Error(err)
		os.Exit(-1)
	}
}
