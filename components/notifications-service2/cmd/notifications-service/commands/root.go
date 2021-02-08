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

func Execute() {
	if err := RootCmd().Execute(); err != nil {
		log.Error(err)
		os.Exit(-1)
	}
}
