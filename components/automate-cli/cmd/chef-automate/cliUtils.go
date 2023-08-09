package main

import (
	"fmt"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
)

// GenerateOriginalAutomateCLICommand builds the original command from the cobra command and args
func GenerateOriginalAutomateCLICommand(cmd *cobra.Command, args []string) string {
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
func RunCmdOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
	script := GenerateOriginalAutomateCLICommand(cmd, args)
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", err
	}

	ips := infra.Outputs.AutomatePrivateIps.Value
	if len(ips) == 0 {
		return "", errors.New("No automate IPs are found")
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
		if len(strings.TrimSpace(output)) != 0 {
			printErrorMessage("Automate", ips[0], writer, output)
		}
		return "", err
	}
	// writer.Print(output)
	return output, nil
}

// RunCmdOnSingleAutomateNode runs the command on a single automate node
// fileName is the name of the file in Automate - after the command execution, the file will be copied to /tmp path in bastion
func RunCmdOnSingleAutomateNodeNCopyReport(cmd *cobra.Command, args []string, fileName string, infra *AutomateHAInfraDetails) error {
	script := GenerateOriginalAutomateCLICommand(cmd, args)

	ips := infra.Outputs.AutomatePrivateIps.Value
	if len(ips) == 0 {
		return errors.New("No automate IPs are found")
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
	//copy file from remote to bastion
	writer.Printf("Downloading file %s from remote node %s \n", fileName, sshConfig.hostIP)
	destFileName, err := sshUtil.copyFileFromRemote(fileName, fileName)
	if err != nil {
		return err
	}
	writer.Printf("File downloaded %s \n", destFileName)

	return nil
}

func GetEnabledFlags(cmd *cobra.Command, flagsToIgnore map[string]int) string {
	flags := ""
	cmd.Flags().VisitAll(func(flag *pflag.Flag) {
		if flagsToIgnore[flag.Name] != 0 {
			return
		}
		if flag.Changed {
			if flag.Value.Type() != "bool" {
				flags += " --" + flag.Name + " " + flag.Value.String()
			} else {
				flags += " --" + flag.Name
			}
		}
	})
	return flags
}
