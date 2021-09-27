// Copyright © 2017 Chef Software

package main

func deployA2HA(args []string) error {
	writer.Printf("A2HA deployment started \n\n\n")
	return executeAutomateClusterCtlCommand("deploy", args, automateHADeployHelpDocs)
}
