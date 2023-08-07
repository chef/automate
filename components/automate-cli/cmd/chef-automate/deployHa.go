// Copyright © 2017 Chef Software

package main

import (
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/sshutils"
	"github.com/pkg/errors"
)

func executeDeployment(args []string) error {
	var indexOfConfig = 0
	var configFile = ""
	for i, a := range args {
		if strings.Contains(a, ".toml") {
			configFile = a
			indexOfConfig = i
			break
		}
	}
	args = append(args[:indexOfConfig], args[indexOfConfig+1:]...)
	args = append(args, "-y")
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommandAsync("deploy", args, automateHADeployHelpDocs, true)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}
func executeConfigVerify(configFile string) error {
	if len(configFile) > 1 {
		log, err := logger.NewLogger("text", "info")
		if err != nil {
			return err
		}

		createSystemdServiceWithBinary, err := verifysystemdcreate.NewCreateSystemdService(
			verifysystemdcreate.NewSystemdCreateUtilsImpl(),
			BINARY_DESTINATION_FOLDER,
			SYSTEMD_PATH,
			false,
			writer,
		)
		if err != nil {
			return err
		}
		deps := &verifyCmdDeps{
			getAutomateHAInfraDetails: getAutomateHAInfraDetails,
			PopulateHaCommonConfig:    PopulateHaCommonConfig,
		}
		c := NewVerifyCmdFlow(httputils.NewClient(log), createSystemdServiceWithBinary, verifysystemdcreate.NewSystemdCreateUtilsImpl(), config.NewHaDeployConfig(), sshutils.NewSSHUtilWithCommandExecutor(sshutils.NewSshClient(), log, command.NewExecExecutor()), writer, deps)
		return c.RunVerify(configFile)
	}
	return status.New(status.ConfigError, "Config file not found")
}
