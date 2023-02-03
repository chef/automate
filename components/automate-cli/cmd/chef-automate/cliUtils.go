package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
)

// CommandBuilder builds the original command from the cobra command and args
func CommandBuilder(cmd *cobra.Command, args []string) string {
	fullCommand := cmd.CommandPath()
	cmd.Flags().VisitAll(func(flag *pflag.Flag) {
		if flag.Changed {
			if flag.Value.Type() != "bool" {
				fullCommand += " --" + flag.Name + " " + flag.Value.String()
			} else {
				fullCommand += " --" + flag.Name
			}
		}
	})
	return fmt.Sprint("sudo " + fullCommand + " " + strings.Join(args, " "))
}

// RunCmdOnSingleAutomateNode runs the command on a single automate node
func RunCmdOnSingleAutomateNode(cmd *cobra.Command, args []string) error {
	script := CommandBuilder(cmd, args)

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}

	ips := infra.Outputs.AutomatePrivateIps.Value
	if len(ips) == 0 {
		writer.Fail("No automate IPs are found")
		os.Exit(1)
	}

	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		hostIP:     ips[0],
		timeout:    10,
	}
	sshUtil := NewSSHUtil(sshConfig)

	output, err := sshUtil.connectAndExecuteCommandOnRemote(script, true)
	if err != nil {
		return err
	}
	writer.Print(output)
	return nil
}
