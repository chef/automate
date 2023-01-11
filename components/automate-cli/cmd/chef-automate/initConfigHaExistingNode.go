package main

import (
	"io/ioutil"
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

func runInitConfigExistingNodeHACmd() error {
	initConfigHAPath := initConfigHAPathFlags.path
	if _, err := os.Stat(initConfigHAPath); err == nil {
		writer.Printf("Skipping config initialization. Config already exists at %s\n", initConfigHAPath)
		return nil
	}

	err := ioutil.WriteFile(initConfigHAPath, []byte(haExistingNodesConfigTemplate), 0600)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}
	writer.Printf("\nconfig initializatized in a generated file : %s \n", initConfigHAPath)
	return nil
}
