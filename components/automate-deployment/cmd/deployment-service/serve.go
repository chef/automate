package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/automate-deployment/pkg/server"
	"github.com/chef/automate/lib/tracing"
)

func init() {
	RootCmd.AddCommand(serveCmd())
}

func serveCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "serve CONFIG_FILE",
		Short: "Start deployment-service",
		Long:  "Start the Chef Automate Deployment Service",
		Args:  cobra.ExactArgs(1),
		Run:   runServeCmd,
	}
}

func runServeCmd(cmd *cobra.Command, args []string) {
	configPath := args[0]
	viper.SetConfigFile(configPath)
	if err := viper.ReadInConfig(); err != nil {
		fmt.Printf("failed to read config file %s: %v", configPath, err)
		os.Exit(1)
	}

	serverConfig := server.DefaultServerConfig()
	if err := viper.Unmarshal(serverConfig); err != nil {
		fmt.Printf("failed to unmarshal config: %v", err)
		os.Exit(1)
	}

	closer, err := tracing.NewGlobalTracer("deployment-service")
	if err == nil {
		defer tracing.CloseQuietly(closer)
	}

	if err := server.StartServer(serverConfig); err != nil {
		fmt.Printf("failed to start server: %v", err)
		os.Exit(1)
	}
}
