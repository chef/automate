// Copyright © 2017 Chef Software

package main

import (
	"errors"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/spf13/cobra"
)

var validateFlags = struct {
	airgapPath        string
	habitatBundlePath string
}{}

const MISSING_BUNDLE = "for offline mode airgap bundle path and habitat bundle path are mandatory"
const VALIDATION_SCRIPT_PATH = "/var/tmp/validateInfra.sh"

func init() {
	validateCmd.PersistentFlags().StringVar(
		&validateFlags.airgapPath,
		"airgap-bundle",
		"",
		"Path to airgap bundle")
	RootCmd.AddCommand(validateCmd)

	validateCmd.PersistentFlags().StringVar(
		&validateFlags.habitatBundlePath,
		"habitat-bundle",
		"",
		"Path to haitat tar bundle")
	RootCmd.AddCommand(validateCmd)
}

var validateCmd = &cobra.Command{
	Use:   "validate-ha-infrastructure",
	Short: "Validate existing infrastructure of Automate HA",
	Long:  "Validate existing infrastructure of automate HA",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE: runValidateCmd,
}

func runValidateCmd(cmd *cobra.Command, args []string) error {
	var offlineMode bool = false
	var netcatPath string = ""
	if len(args) < 0 {
		return errors.New("config.toml is required as argument")
	}
	var deployer, derr = getDeployer(args[0])
	if derr != nil || deployer == nil {
		return status.Wrap(derr, status.ConfigError, invalidConfig)
	}
	if len(validateFlags.airgapPath) > 0 && len(validateFlags.habitatBundlePath) > 0 {
		offlineMode = true
		_, err := os.Stat(validateFlags.habitatBundlePath)
		if err != nil {
			return err
		}
		metadata, err := unpackAirgap()
		if err != nil {
			return err
		}
		for _, v := range metadata.HartifactPaths {
			if strings.Contains(v, "automate-netcat") {
				netcatPath = v
				break
			}
		}
	}

	if (len(validateFlags.airgapPath) > 0 && len(validateFlags.habitatBundlePath) < 1) ||
		(len(validateFlags.airgapPath) < 1 && len(validateFlags.habitatBundlePath) > 0) {
		return errors.New(MISSING_BUNDLE)
	}
	err := generateValidationScript()
	if err != nil {
		return err
	}
	err = executeValidationScript(args, netcatPath, offlineMode)
	if err != nil {
		return err
	}
	err = removeValidationScript()
	if err != nil {
		return err
	}
	return nil
}

func executeValidationScript(args []string, netcatHartFile string, offlineMode bool) error {
	configFilePath, err := filepath.Abs(args[0])
	if err != nil {
		return err
	}
	if offlineMode {
		return executeShellCommand("/bin/bash", []string{VALIDATION_SCRIPT_PATH, configFilePath, validateFlags.habitatBundlePath, netcatHartFile}, "")
	} else {
		return executeShellCommand("/bin/bash", []string{VALIDATION_SCRIPT_PATH, configFilePath}, "")
	}
}

func unpackAirgap() (airgap.UnpackMetadata, error) {
	return airgap.Unpack(validateFlags.airgapPath)
}

func generateValidationScript() error {
	return ioutil.WriteFile(VALIDATION_SCRIPT_PATH, []byte(validationScript), 0755) // nosemgrep
}

func removeValidationScript() error {
	return os.Remove(VALIDATION_SCRIPT_PATH)
}
