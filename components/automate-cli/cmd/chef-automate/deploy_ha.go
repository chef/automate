// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"io"
	"os"
	"os/exec"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
)

func isA2HADeployment(configPath []string) bool {
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath[0]
	}
	if checkIfFileExist(initConfigHAPath) {
		config, err := ptoml.LoadFile(initConfigHAPath)
		if err != nil {
			writer.Println(err.Error())
			return false
		}
		if config.Get("architecture.existing_infra") != nil || config.Get("architecture.aws") != nil {
			return true
		} else {
			return false
		}
	} else if checkIfFileExist(initConfigHabA2HAPathFlag.a2haDirPath + "a2ha.rb") {
		return true
	} else {
		return false
	}

}

func deployA2HA() error {
	writer.Printf("A2HA deployment started \n")
	//args := []string{"deploy"}
	c := exec.Command("automate-cluster-ctl", "deploy")
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(os.Stdout, &out)
	c.Stderr = io.MultiWriter(os.Stderr, &stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "deployment failed")
	} else {
		writer.Printf("No error in executing commands")
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	writer.Printf("\nout:\n%s\nerr:\n%s\n", outStr, errStr)
	writer.Printf("A2HA deployment done. exiting\n")
	return err
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}
