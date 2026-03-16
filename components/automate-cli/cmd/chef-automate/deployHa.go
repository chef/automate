// Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.

package main

import (
	"strings"

	"github.com/pkg/errors"
)

func executeDeployment(args []string) error {
	indexOfConfig, _ := getConfigFileFromArgs(args)
	args = append(args[:indexOfConfig], args[indexOfConfig+1:]...)
	args = append(args, "-y")
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommandAsync("deploy", args, automateHADeployHelpDocs, true)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}
func getConfigFileFromArgs(args []string) (int, string) {
	var indexOfConfig = 0
	var configFile = ""
	for i, a := range args {
		if strings.Contains(a, ".toml") {
			configFile = a
			indexOfConfig = i
			break
		}
	}
	return indexOfConfig, configFile
}
