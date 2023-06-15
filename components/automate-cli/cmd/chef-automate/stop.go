// Copyright © 2017 Chef Software

package main

import (
	"container/list"
	"context"
	"fmt"
	"os"
	"os/exec"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

const (
	ERROR_ON_MANAGED_SERVICE_STOP = "Stopping the service for externally configured %s is not supported"
)

var stopCmdFlags = struct {
	automate    bool
	chef_server bool
	opensearch  bool
	postgresql  bool
	node        string
}{}

func init() {
	var stopCmd = &cobra.Command{
		Use:   "stop",
		Short: "Stop deployment",
		Long:  "Stop a running deployment of Automate.",
		RunE:  runStopCmd,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}

	stopCmd.PersistentFlags().BoolVarP(&stopCmdFlags.automate, "automate", "a", false, "Stop chef automate services of automate nodes")
	stopCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVar(&stopCmdFlags.automate, "a2", false, "Stop chef automate services of automate nodes")
	stopCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVarP(&stopCmdFlags.chef_server, "chef_server", "c", false, "Stop chef automate services of chef_server nodes")
	stopCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVar(&stopCmdFlags.chef_server, "cs", false, "Stop chef automate services of chef_server nodes")
	stopCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVarP(&stopCmdFlags.opensearch, "opensearch", "o", false, "Stop hab-sup service of opensearch nodes")
	stopCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVar(&stopCmdFlags.opensearch, "os", false, "Stop hab-sup service of opensearch nodes")
	stopCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVarP(&stopCmdFlags.postgresql, "postgresql", "p", false, "Stop hab-sup service of postgresql nodes")
	stopCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	stopCmd.PersistentFlags().BoolVar(&stopCmdFlags.postgresql, "pg", false, "Stop hab-sup service of postgresql nodes")
	stopCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})

	stopCmd.PersistentFlags().StringVar(&stopCmdFlags.node, "node", "", "Node Ip address")
	stopCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(stopCmd)
}

func runStopCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := isFlagEnabled(cmd); err != nil {
			return err
		}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		if err = runStopCommandHA(infra, isManagedServicesOn()); err != nil {
			return err
		}

	} else if isDevMode() {
		if err := runStopDevMode(); err != nil {
			return err
		}

	} else {
		if err := runStopStandalone(); err != nil {
			return err
		}
	}

	return nil
}

func runStopStandalone() error {
	writer.Title("Stopping Chef Automate")
	systemctlCmd := exec.Command("systemctl", "stop", "chef-automate.service")
	systemctlCmd.Stdout = os.Stdout
	systemctlCmd.Stderr = os.Stderr
	if err := systemctlCmd.Run(); err != nil {
		return status.Annotate(err, status.ServiceUnloadError)
	}

	writer.Title("Chef Automate Stopped")
	return nil
}

func runStopDevMode() error {
	writer.Title("Stopping Chef Automate")
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	_, err = connection.Stop(context.Background(), &api.StopRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stop services failed",
		)
	}

	writer.Title("Chef Automate Stopped")
	return nil
}

func runStopCommandHA(infra *AutomateHAInfraDetails, isManagedServices bool) error {
	errorList := list.New()
	count := getFlagCount()

	if count > 1 && stopCmdFlags.node != "" {
		return errors.New("Please remove node flag if you have given multiple service flags.")
	}
	if stopCmdFlags.automate {
		getAndStopHaNodes(AUTOMATE, infra, errorList)
	}
	if stopCmdFlags.chef_server {
		getAndStopHaNodes(CHEF_SERVER, infra, errorList)
	}
	if stopCmdFlags.opensearch {
		if isManagedServices {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICE_STOP, OPENSEARCH)
		}
		getAndStopHaNodes(OPENSEARCH, infra, errorList)
	}
	if stopCmdFlags.postgresql {
		if isManagedServices {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICE_STOP, POSTGRESQL)
		}
		getAndStopHaNodes(POSTGRESQL, infra, errorList)
	}

	if errorList.Len() > 0 {
		return status.New(status.ServiceUnloadError, getSingleErrorFromList(errorList).Error())
	}
	return nil
}

func getAndStopHaNodes(remoteService string, infra *AutomateHAInfraDetails, errorList *list.List) {
	sshConfig := getSshDetails(infra)
	var ips []string

	if stopCmdFlags.node != "" {
		isValid := validateEachIp(remoteService, infra, stopCmdFlags.node)
		if !isValid {
			errorList.PushBack(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
			return
		}
		ips = append(ips, stopCmdFlags.node)
	} else {
		ips = getIps(remoteService, infra)
	}
	stopCommandImplHA(*sshConfig, ips, remoteService, errorList)
}

func stopCommandImplHA(sshConfig SSHConfig, ips []string, remoteService string, errorList *list.List) {
	sshUtilMap := getMapSSHUtils(ips, &sshConfig)
	err := checkNodeType(sshUtilMap, ips, remoteService)
	if err != nil {
		errorList.PushBack(err.Error())
	}
}

func checkNodeType(sshUtilMap map[string]SSHUtil, ips []string, remoteService string) error {
	if len(ips) == 0 {
		writer.Errorf("No %s IPs are found", remoteService)
		return status.Errorf(1, "No %s IPs are found", remoteService)
	}
	if remoteService == OPENSEARCH || remoteService == POSTGRESQL {
		return stopBackEndNodes(sshUtilMap, ips, remoteService)
	}
	return stopFrontEndNodes(sshUtilMap, ips, remoteService)
}

func stopFrontEndNodes(sshUtilMap map[string]SSHUtil, ips []string, remoteService string) error {
	resultChan := make(chan Result, len(ips))
	errorList := list.New()
	scriptCommands := `sudo chef-automate stop`
	for _, hostIP := range ips {
		sshUtil := sshUtilMap[hostIP]
		go commandExecuteFrontEnd(scriptCommands, sshUtil, resultChan)
	}
	printMessageForStopResultChan(resultChan, ips, remoteService, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceUnloadError, "Not able to stop one or more nodes in %s", remoteService)
	}
	return nil
}

func stopBackEndNodes(sshUtilMap map[string]SSHUtil, ips []string, remoteService string) error {
	resultChan := make(chan Result, len(ips))
	scriptCommands := "sudo systemctl stop hab-sup"
	errorList := list.New()
	for _, hostIP := range ips {
		sshUtil := sshUtilMap[hostIP]
		go commandExecuteBackendNode(scriptCommands, sshUtil, resultChan)
	}
	printMessageForStopResultChan(resultChan, ips, remoteService, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceUnloadError, "Not able to stop one or more nodes in %s", remoteService)
	}
	return nil
}

func printMessageForStopResultChan(resultChan chan Result, ips []string, remoteService string, errorList *list.List) {
	for i := 0; i < len(ips); i++ {
		result := <-resultChan
		printStopConnectionMessage(remoteService, result.HostIP)
		if result.Error != nil {
			printStopErrorMessage(remoteService, result.HostIP, result.Error)
			errorList.PushBack(result.Error.Error())
		} else {
			writer.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printStopSuccessMessage(remoteService, result.HostIP)
		}
	}
}

func printStopConnectionMessage(remoteService string, hostIP string) {
	writer.Println("Connecting to the " + remoteService + NODE + hostIP)
	writer.BufferWriter().Flush()
}

func printStopErrorMessage(remoteService string, hostIP string, err error) {
	writer.Failf("Stop Command failed on "+remoteService+NODE+hostIP+" with error:\n%v \n", err)
	writer.BufferWriter().Flush()
}

func printStopSuccessMessage(remoteService string, hostIP string) {
	writer.Success("Stop Command is completed on " + remoteService + NODE + hostIP + "\n")
	writer.BufferWriter().Flush()
}

func validateEachIp(remoteService string, infra *AutomateHAInfraDetails, ip string) bool {
	ips := getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == ip {
			return true
		}
	}
	return false
}

func getIps(remoteService string, infra *AutomateHAInfraDetails) []string {
	if remoteService == AUTOMATE {
		return infra.Outputs.AutomatePrivateIps.Value
	} else if remoteService == CHEF_SERVER {
		return infra.Outputs.ChefServerPrivateIps.Value
	} else if remoteService == POSTGRESQL {
		return infra.Outputs.PostgresqlPrivateIps.Value
	} else if remoteService == OPENSEARCH {
		return infra.Outputs.OpensearchPrivateIps.Value
	}
	return []string{}
}

func getFlagCount() int {
	count := 0
	if stopCmdFlags.automate {
		count++
	}
	if stopCmdFlags.chef_server {
		count++
	}
	if stopCmdFlags.opensearch {
		count++
	}
	if stopCmdFlags.postgresql {
		count++
	}
	return count
}

func isFlagEnabled(cmd *cobra.Command) error {
	if !stopCmdFlags.automate && !stopCmdFlags.chef_server && !stopCmdFlags.opensearch && !stopCmdFlags.postgresql {
		if stopCmdFlags.node != "" {
			writer.Println("Please Provide service flag")
			return errors.New("Please provide service flag")
		}
		writer.Println(cmd.UsageString())
		return errors.New("No flag is enabled. Please provide any flag")
	}
	return nil
}
