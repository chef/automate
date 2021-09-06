// Copyright © 2017 Chef Software

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"

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
	} else if checkIfFileExist(FlagPath) {
		flag, err := checkDeploymentModeFromFlag()
		fmt.Println(flag)
		if err != nil {
			writer.Println(err.Error())
			return false
		} else if flag.DeploymentMode == AutomateHAMode {
			return true
		} else {
			return false
		}
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

func checkDeploymentModeFromFlag() (DeploymentModeFlag, error) {
	deploymentModeFlag := DeploymentModeFlag{}
	templateBytes, err := ioutil.ReadFile(FlagPath)
	writer.Println(string(templateBytes))
	if err != nil {
		return deploymentModeFlag, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	err = ptoml.Unmarshal(templateBytes, &deploymentModeFlag)
	fmt.Println(deploymentModeFlag)
	if err != nil {
		fmt.Println(err)
		return deploymentModeFlag, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	} else {
		return deploymentModeFlag, nil
	}
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}
