// Copyright © 2017 Chef Software

package main

import (
	"os/exec"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
)

func isA2HADeployment(configPath string) bool {
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath
	}
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		return false
	}
	if config.Get("architecture.existing_infra") != nil || config.Get("architecture.aws") != nil {
		return true
	} else {
		return false
	}
}

func deployA2HA() error {
	writer.Printf("A2HA deployment started \n")
	cmd, b := exec.Command("automate-cluster-ctl", "deploy"), new(strings.Builder)
	cmd.Dir = "/hab/a2_deploy_workspace"
	cmd.Stdout = b
	err := cmd.Start()
	if err != nil {
		return status.Annotate(err, status.CommandExecutionError)
	}
	writer.Print(b.String())
	writer.Printf("A2HA deployment done. %d, exiting\n", cmd.Process.Pid)
	return err
}
