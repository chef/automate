// Copyright © 2017 Chef Software

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
)

var initConfigFlags = struct {
	path            string
	channel         string
	upgradeStrategy string
	keyPath         string
	certPath        string
	fqdn            string
	esMem           string
}{}

func init() {
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.path,
		"file",
		"config.toml",
		"File path to write the config")
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.channel,
		"channel",
		"current",
		"Release channel to deploy all services from")
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.upgradeStrategy,
		"upgrade-strategy",
		"at-once",
		"Upgrade strategy to use for this deployment.")
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.certPath,
		"certificate",
		"",
		"The path to a certificate that should be used for external TLS connections (web and API).")
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.keyPath,
		"private-key",
		"",
		"The path to a private key corresponding to the TLS certificate.")
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.fqdn,
		"fqdn",
		"",
		"The fully-qualified domain name that Chef Automate can be accessed at. (default: hostname of this machine)")

	// NOTE(ssd) 2019-02-04: We need this flag for CI so that we
	// can easily lower the default ES heap size without needing
	// to parse the toml file this command creates.
	initConfigCmd.PersistentFlags().StringVar(
		&initConfigFlags.esMem,
		"es-mem",
		"",
		"The amount of system memory to allocate to Elasticsearch's heap.  (default: 25% of system memory)")

	RootCmd.AddCommand(initConfigCmd)
}

var initConfigCmd = &cobra.Command{
	Use:   "init-config",
	Short: "Initialize default config",
	Long:  "Initialize default configuration and save it to a file.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runInitConfigCmd,
}

func runInitConfigCmd(cmd *cobra.Command, args []string) error {
	initConfigPath := initConfigFlags.path
	if _, err := os.Stat(initConfigPath); err == nil {
		writer.Printf("Skipping config initialization. Config already exists at %s\n", initConfigPath)
		return nil
	}

	if initConfigFlags.keyPath != "" && initConfigFlags.certPath == "" {
		msg := "Cannot provide --private-key without also providing --certificate."
		return status.New(status.InvalidCommandArgsError, msg)
	}

	if initConfigFlags.certPath != "" && initConfigFlags.keyPath == "" {
		msg := "Cannot provide --certificate without also providing --private-key."
		return status.New(status.InvalidCommandArgsError, msg)
	}

	cfg, err := deployment.GenerateInitConfig(
		initConfigFlags.channel,
		initConfigFlags.upgradeStrategy,
		deployment.InitialTLSCerts(initConfigFlags.keyPath, initConfigFlags.certPath),
		deployment.InitialFQDN(initConfigFlags.fqdn),
		deployment.ESMem(initConfigFlags.esMem),
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Generating initial configuration failed")
	}

	t, err := cfg.Render()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Rendering initial configuration failed")
	}

	// Make sure our user facing config is a valid AutomateConfig
	ac := deployment.NewAutomateConfig()
	err = toml.Unmarshal([]byte(t), ac)
	if err != nil {
		return status.Wrap(err, status.MarshalError, "Marshaling initial configuration failed")
	}

	// TODO XXX UGH: We have removed creds from init-config/config.toml BUT we
	// still use them under the covers. So we need the validation to ensure they
	// are there. So we need to add fake values here.
	err = ac.AddCredentials("snakes", "snakes", "snakes") // I don't know no snakes
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Applying dummy credentials during validation failed")
	}

	if err := ac.ValidateWithGlobalAndDefaults(); err != nil {
		return status.Wrap(err, status.ConfigError, "Validating initial configuration failed")
	}

	err = ioutil.WriteFile(initConfigPath, []byte(t), 0600)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}

	writer.Successf("Config written to %s", initConfigPath)
	lines := []string{
		"Automate Load Balancer fqdn set to [%[1]s]",
		"When Automate is deployed you will access https://%[1]s to see the dashboard.",
		"If this is not a routable address please update the fqdn appropriately before deploying.",
	}
	writer.Println(fmt.Sprintf(strings.Join(lines, "\n"), cfg.Fqdn))
	return nil
}
