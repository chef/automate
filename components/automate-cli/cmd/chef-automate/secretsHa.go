// Copyright © 2017 Chef Software

package main

import (
	"errors"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

func init() {
	secretsCmd.SetUsageTemplate(secretsHelpDocs)
	RootCmd.AddCommand(secretsCmd)
}

var secretsCmd = &cobra.Command{
	Use:   "secrets",
	Short: "Set secrets to Automate HA",
	Long:  "Set secrets for Automate sudo password and admin password in HA mode.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	Args: cobra.RangeArgs(0, 2),
	RunE: runSecretsConfigCmd,
}

func runSecretsConfigCmd(cmd *cobra.Command, args []string) error {
	if !isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	operation := ""
	for _, v := range args {
		if v == "init" {
			operation = "init"
			break
		}
		if v == "set" {
			operation = "set"
			break
		}
	}
	if operation == "init" {
		response, err := writer.Prompt("your key might get rotated, Would you like to create a new key or rotate the existing credentials? y/n")
		if err != nil {
			return err
		}
		if !strings.Contains(response, "y") {
			return errors.New("canceled secrets init")
		}
	}
	if operation == "set" {
		response, err := writer.Prompt("Enter secret: ")
		if err != nil {
			return err
		}
		args = append(args, response)
	}

	return executeSecretsCommand(args)
}

func executeSecretsCommand(args []string) error {
	if isA2HARBFileExist() {
		if len(args) == 0 {
			writer.Print("please refer \n" + secretsHelpDocs)
			return nil
		}
		return executeAutomateClusterCtlCommand("secrets", args, secretsHelpDocs)
	}
	return status.New(status.InvalidCommandArgsError, secretsHelpDocs)
}
