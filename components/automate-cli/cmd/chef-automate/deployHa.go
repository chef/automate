// Copyright © 2017 Chef Software

package main

func deployA2HA() error {
	writer.Printf("A2HA deployment started \n\n\n")
	args := []string{"-y"}
	return executeAutomateClusterCtlCommandAsync("deploy", args, automateHADeployHelpDocs)
}
