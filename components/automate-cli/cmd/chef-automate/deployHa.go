// Copyright © 2017 Chef Software

package main

import (
	"fmt"
	"strings"
)

func executeDeployment(args []string) error {
	writer.Printf("A2HA deployment started \n\n\n")
	var indexOfConfig = 0
	for i, a := range args {
		if strings.Contains(a, ".toml") {
			indexOfConfig = i
			break
		}
	}
	args = append(args[:indexOfConfig], args[indexOfConfig+1:]...)
	args = append(args, "-y")
	fmt.Println(args)
	return executeAutomateClusterCtlCommandAsync("deploy", args, automateHADeployHelpDocs)
}
