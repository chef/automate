// Copyright © 2017 Chef Software

package main

import (
	"errors"
	"github.com/spf13/cobra"
	"io/ioutil"
	"os"
	"path/filepath"
)

var cleanupFlags = struct {
	nodeName string
	ports    string
}{}

const CLEANUP_SCRIPT_PATH = "/var/tmp/cleanup.sh"

func init() {
	cleanupCmd.PersistentFlags().StringVar(
		&cleanupFlags.nodeName,
		"nodeName",
		"",
		"node on which cleanup needs to be done")
	RootCmd.AddCommand(cleanupCmd)

	cleanupCmd.PersistentFlags().StringVar(
		&cleanupFlags.ports,
		"ports",
		"",
		"Enabling port flag will close all Autmate HA Ports")
	RootCmd.AddCommand(cleanupCmd)
}

var cleanupCmd = &cobra.Command{
	Use:   "clean-infra",
	Short: "Clean existing infrastructure of Automate HA",
	Long:  "Clean existing infrastructure of automate HA",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE:   runCleanupCmd,
	Hidden: true,
}

func runCleanupCmd(cmd *cobra.Command, args []string) error {

	if len(args) < 0 {
		return errors.New("config.toml is required as argument")
	}

	err := generateCleanupScript()
	if err != nil {
		return err
	}
	err = executeCleanupScript(args)
	if err != nil {
		return err
	}
	err = removeCleanupScript()
	if err != nil {
		return err
	}
	return nil
}

func executeCleanupScript(args []string) error {
	configFilePath, err := filepath.Abs(args[0])
	if err != nil {
		return err
	}
	if len(cleanupFlags.nodeName) > 0 && len(cleanupFlags.ports) > 0 {
		return executeShellCommand("/bin/bash", []string{CLEANUP_SCRIPT_PATH, "-c", configFilePath, "-n", cleanupFlags.nodeName, "-p"}, "")
	} else if len(cleanupFlags.nodeName) > 0 && len(cleanupFlags.ports) < 0 {
		return executeShellCommand("/bin/bash", []string{CLEANUP_SCRIPT_PATH, "-c", configFilePath, "-n", cleanupFlags.nodeName}, "")
	} else {
		return executeShellCommand("/bin/bash", []string{CLEANUP_SCRIPT_PATH, "-c", configFilePath, "-p"}, "")
	}

}

func generateCleanupScript() error {
	return ioutil.WriteFile(CLEANUP_SCRIPT_PATH, []byte(cleanupScript), 0755) // nosemgrep
}

func removeCleanupScript() error {
	return os.Remove(CLEANUP_SCRIPT_PATH)
}
