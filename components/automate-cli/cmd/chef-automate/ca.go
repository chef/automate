package main

import (
	"container/list"
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var caCmdFlags = struct {
	automate    bool
	chef_server bool
	node        string
}{}

var caCmd = &cobra.Command{
	Use:   "internal-ca COMMAND",
	Short: "Manage Chef Automate's internal certificate authority",
	Long:  "Manage Chef Automate's internal certificate authority. Used for inter-service encryption and authentication.",
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var caInfo = &cobra.Command{
	Use:   "info",
	Short: "Print information of the root certificate for the internal certificate authority",
	RunE:  runCAInfoCmd,
	Args:  cobra.MaximumNArgs(0),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var regen = &cobra.Command{
	Use:   "regenerate",
	Short: "Commands to regenerate certificates issued by the internal certificate authority",
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var regenRoot = &cobra.Command{
	Use:   "root",
	Short: "Regenerate the root certificate for the internal certificate authority",
	RunE:  runRegenRootCmd,
	Args:  cobra.MaximumNArgs(0),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func init() {
	regen.AddCommand(regenRoot)
	caCmd.AddCommand(caInfo)
	caCmd.AddCommand(regen)

	caInfo.PersistentFlags().BoolVarP(&caCmdFlags.automate, "automate", "a", false, "Print information of the root certificate of automate nodes")
	caInfo.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	caInfo.PersistentFlags().BoolVar(&caCmdFlags.automate, "a2", false, "Print information of the root certificate of automate nodes")
	caInfo.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	caInfo.PersistentFlags().BoolVarP(&caCmdFlags.chef_server, "chef_server", "c", false, "Print information of the root certificate of chef_server nodes")
	caInfo.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	caInfo.PersistentFlags().BoolVar(&caCmdFlags.chef_server, "cs", false, "Print information of the root certificate of chef_server nodes")
	caInfo.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	caInfo.PersistentFlags().StringVar(&caCmdFlags.node, "node", "", "Node Ip address")
	caInfo.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})

	regenRoot.PersistentFlags().BoolVarP(&caCmdFlags.automate, "automate", "a", false, "Regenerate the root certificate for automate nodes")
	regenRoot.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	regenRoot.PersistentFlags().BoolVar(&caCmdFlags.automate, "a2", false, "Regenerate the root certificate for automate nodes")
	regenRoot.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	regenRoot.PersistentFlags().BoolVarP(&caCmdFlags.chef_server, "chef_server", "c", false, "Regenerate the root certificate for chef_server nodes")
	regenRoot.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	regenRoot.PersistentFlags().BoolVar(&caCmdFlags.chef_server, "cs", false, "Regenerate the root certificate for chef_server nodes")
	regenRoot.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	regenRoot.PersistentFlags().StringVar(&caCmdFlags.node, "node", "", "Node Ip address")
	regenRoot.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})

	RootCmd.AddCommand(caCmd)
}

func runCAInfoCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		infoCmd := "sudo " + cmd.CommandPath()
		if err := isFeFlagEnabled(cmd); err != nil {
			return err
		}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		if err = runInternalCaHA(infra, infoCmd); err != nil {
			return err
		}

	} else {
		if err := runInternalCaInfoStandalone(); err != nil {
			return err
		}
	}
	return nil
}

func runRegenRootCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		rootGenCmd := "sudo " + cmd.CommandPath()
		if err := isFeFlagEnabled(cmd); err != nil {
			return err
		}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		if err = runInternalCaHA(infra, rootGenCmd); err != nil {
			return err
		}

	} else {
		if err := runGenerateRootStandalone(); err != nil {
			return err
		}
	}
	return nil
}

func runInternalCaInfoStandalone() error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}

	resp, err := connection.GetRootCert(context.Background(), &api.RootCertRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Failed to query certificate authority",
		)
	}

	cert, err := certauthority.PEMToCert(resp.Cert)
	if err != nil {
		return status.Wrap(
			err,
			status.UnknownError,
			"could not parse root certificate",
		)
	}

	writer.Printf("         Authority Name: %s\n", cert.Subject)
	writer.Printf("Root CA Expiration Date: %s (%s from now)\n", cert.NotAfter, prettyDuration(time.Until(cert.NotAfter)))
	return nil
}

func runGenerateRootStandalone() error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}

	_, err = connection.RegenerateRoot(context.Background(), &api.RegenerateRootRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Failed to regenerate certificate authority",
		)
	}

	writer.Title("Certificate authority regenerated.")
	return nil
}

func runInternalCaHA(infra *AutomateHAInfraDetails, scriptCommands string) error {
	sshConfig := getSshDetails(infra)
	if caCmdFlags.automate && caCmdFlags.chef_server && caCmdFlags.node != "" {
		return errors.New("Please remove node flag if you have given multiple service flags.")
	}

	ips, remoteService, err := getHANodes(infra)
	if err != nil {
		return err
	}
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", remoteService))
	}

	err = triggerInternalCaHA(*sshConfig, ips, remoteService, scriptCommands)
	if err != nil {
		return err
	}
	return nil
}

func getHANodes(infra *AutomateHAInfraDetails) ([]string, string, error) {
	var ips []string
	var remoteService string
	if caCmdFlags.automate {
		remoteService = AUTOMATE
		if caCmdFlags.node != "" {
			isValid := validateEachIp(remoteService, infra, caCmdFlags.node)
			if !isValid {
				return []string{}, remoteService, errors.New(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
			}
			ips = append(ips, caCmdFlags.node)
		} else {
			ips = append(ips, getIps(remoteService, infra)...)
		}
	}
	if caCmdFlags.chef_server {
		remoteService = CHEF_SERVER
		if caCmdFlags.node != "" {
			isValid := validateEachIp(remoteService, infra, caCmdFlags.node)
			if !isValid {
				return []string{}, remoteService, errors.New(fmt.Sprintf("Please Enter Valid %s IP", remoteService))
			}
			ips = append(ips, caCmdFlags.node)
		} else {
			ips = append(ips, getIps(remoteService, infra)...)
		}
	}

	if caCmdFlags.automate && caCmdFlags.chef_server {
		remoteService = FRONTEND
	}

	return ips, remoteService, nil
}

func triggerInternalCaHA(sshConfig SSHConfig, ips []string, remoteService, scriptCommands string) error {
	sshUtilMap := getMapSSHUtils(ips, &sshConfig)
	errorList := list.New()

	resultChan := make(chan Result, len(ips))
	for _, hostIP := range ips {
		sshUtil := sshUtilMap[hostIP]
		go commandExecuteFrontEnd(scriptCommands, sshUtil, resultChan)
	}
	printMessageForInternalCAResultChan(resultChan, ips, remoteService, errorList)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceUnloadError, "Not able to generate root or fetch root info")
	}
	return nil
}

func printMessageForInternalCAResultChan(resultChan chan Result, ips []string, remoteService string, errorList *list.List) {
	for i := 0; i < len(ips); i++ {
		result := <-resultChan
		printInternalCAConnectionMessage(remoteService, result.HostIP)
		if result.Error != nil {
			printInternalCAErrorMessage(remoteService, result.HostIP, result.Error)
			errorList.PushBack(result.Error.Error())
		} else {
			writer.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printInternalCASuccessMessage(remoteService, result.HostIP)
		}
	}
}

func printInternalCAConnectionMessage(remoteService string, hostIP string) {
	writer.Println("Connecting to the " + remoteService + NODE + hostIP)
	writer.BufferWriter().Flush()
}

func printInternalCAErrorMessage(remoteService string, hostIP string, err error) {
	writer.Failf("Command failed on "+remoteService+NODE+hostIP+" with error:\n%v \n", err)
	writer.BufferWriter().Flush()
}

func printInternalCASuccessMessage(remoteService string, hostIP string) {
	writer.Success("Command is completed on " + remoteService + NODE + hostIP + "\n")
	writer.BufferWriter().Flush()
}

func isFeFlagEnabled(cmd *cobra.Command) error {
	if !caCmdFlags.automate && !caCmdFlags.chef_server {
		if caCmdFlags.node != "" {
			writer.Println("Please Provide service flag")
			return errors.New("Please provide service flag")
		}
		writer.Println(cmd.UsageString())
		return errors.New("No flag is enabled. Please provide any flag")
	}
	return nil
}

func prettyDuration(d time.Duration) string {
	if d > 24*time.Hour {
		return fmt.Sprintf("%dd", int(d.Hours()/24))
	}

	return d.String()
}
