package main

import (
	"io/ioutil"
	"os"

	pgc "github.com/chef/automate/components/automate-cli/pkg/pullandgenerateconfig"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

func runInitConfigAwsHACmd() error {
	initConfigHAPath := pgc.InitConfigHAPathFlags.Path
	if _, err := os.Stat(initConfigHAPath); err == nil {
		writer.Printf("Skipping config initialization. Config already exists at %s\n", initConfigHAPath)
		return nil
	}

	err := ioutil.WriteFile(initConfigHAPath, []byte(haAwsConfigTemplate), 0600)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}
	writer.Printf("\nconfig initialised in a generated file : %s \n", initConfigHAPath)
	return nil
}
