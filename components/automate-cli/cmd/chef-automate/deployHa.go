// Copyright © 2017 Chef Software

package main

import (
	"strings"

	"github.com/pkg/errors"
)

func executeDeployment(args []string) error {
	var indexOfConfig = 0
	for i, a := range args {
		if strings.Contains(a, ".toml") {
			indexOfConfig = i
			break
		}
	}
	args = append(args[:indexOfConfig], args[indexOfConfig+1:]...)
	args = append(args, "-y")
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommandAsync("deploy", args, automateHADeployHelpDocs)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}
