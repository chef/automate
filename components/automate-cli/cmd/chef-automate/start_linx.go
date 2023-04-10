package main

import (
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const ERROR_SELF_MANAGED_START = "Start services in %s for externally configured is not supported"
const node = " node :"

var startCmdFlags = struct {
	automate    bool
	chef_server bool
	opensearch  bool
	postgresql  bool
}{}

var startCommand = &cobra.Command{
	Use:   "start",
	Short: "Start Chef Automate",
	RunE:  runStartCmd,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func init() {
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.automate, "automate", "a", false, "start chef automate service on automate nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.automate, "a2", false, "start chef automate service on automate nodes[DUPLICATE]")
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.chef_server, "chef_server", "c", false, "start chef automate service on chef-server nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.chef_server, "cs", false, "start chef automate service on chef-server nodes[DUPLICATE]")
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.opensearch, "opensearch", "o", false, "start hab-sup service on opensearch nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.opensearch, "os", false, "start hab-sup service on opensearch nodes[DUPLICATE]")
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.postgresql, "postgresql", "p", false, "start hab-sup service on postgresql nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.postgresql, "pg", false, "start hab-sup service on postgresql nodes[DUPLICATE]")
	RootCmd.AddCommand(startCommand)
}

func runStartCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {

		var err error
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		sshUser := infra.Outputs.SSHUser.Value
		sskKeyFile := infra.Outputs.SSHKeyFile.Value
		sshPort := infra.Outputs.SSHPort.Value
		sshConfig := &SSHConfig{
			sshUser:    sshUser,
			sshKeyFile: sskKeyFile,
			sshPort:    sshPort,
		}
		timestamp := time.Now().Format("20060102150405")
		sshUtil := NewSSHUtil(sshConfig)
		if startCmdFlags.automate {
			frontendIps := infra.Outputs.AutomatePrivateIps.Value
			if len(frontendIps) == 0 {
				writer.Error("No automate IPs are found")
				os.Exit(1)
			}
			err = startFrontEndNodes(args, sshUtil, frontendIps, "automate", timestamp, writer)
		}
		if startCmdFlags.chef_server {
			frontendIps := infra.Outputs.ChefServerPrivateIps.Value
			if len(frontendIps) == 0 {
				writer.Error("No chef-server IPs are found")
				os.Exit(1)
			}
			err = startFrontEndNodes(args, sshUtil, frontendIps, "chef-server", timestamp, writer)
		}
		if startCmdFlags.opensearch {
			Ips := infra.Outputs.OpensearchPrivateIps.Value
			err = checkNodes(args, sshUtil, Ips, "opensearch", writer)
		}
		if startCmdFlags.postgresql {
			Ips := infra.Outputs.PostgresqlPrivateIps.Value
			err = checkNodes(args, sshUtil, Ips, "postgresql", writer)
		}
	}
	return nil
}
func startFrontEndNodes(args []string, sshUtil SSHUtil, Ips []string, remoteService string, timestamp string, cliWriter *cli.Writer) error {
	resultChan := make(chan ResultConfigSet, len(Ips))
	originalSSHConfig := sshUtil.getSSHConfig()

	scriptCommands := "sudo chef-automate start"

	for _, hostIP := range Ips {
		newSSHConfig := &SSHConfig{
			sshUser:    originalSSHConfig.sshUser,
			sshPort:    originalSSHConfig.sshPort,
			sshKeyFile: originalSSHConfig.sshKeyFile,
			hostIP:     hostIP,
		}
		newSSHUtil := NewSSHUtil(newSSHConfig)

		go func(args []string, remoteService string, scriptCommands string, newSSHUtil SSHUtil, resultChan chan ResultConfigSet) {
			rc := ResultConfigSet{newSSHUtil.getSSHConfig().hostIP, "", nil}
			output, err := newSSHUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			err = checkOutputForError(output)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			rc.Output = output
			resultChan <- rc
		}(args, remoteService, scriptCommands, newSSHUtil, resultChan)
	}

	for i := 0; i < len(Ips); i++ {
		result := <-resultChan

		if i == 0 {
			writer.StopSpinner()
		}
		printStartConnectionMessage(remoteService, result.HostIP, cliWriter)
		if result.Error != nil {
			printStartErrorMessage(remoteService, result.HostIP, cliWriter, result.Error)
		} else {
			cliWriter.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printStartSuccessMessage(remoteService, result.HostIP, cliWriter)
		}

		if i < len(Ips)-1 {
			writer.StartSpinner()
		}
	}

	close(resultChan)
	return nil
}

func checkNodes(args []string, sshUtil SSHUtil, Ips []string, remoteService string, cliWriter *cli.Writer) error {
	var err error
	if len(Ips) == 0 {
		writer.Errorf("No %s IPs are found", remoteService)
		os.Exit(1)
	}
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_START, remoteService)
	}
	for i := 0; i < len(Ips); i++ {
		err = startBackEndNodes(args, sshUtil, Ips[i], remoteService, cliWriter)
	}

	return err
}
func startBackEndNodes(args []string, sshUtil SSHUtil, Ips string, remoteService string, cliWriter *cli.Writer) error {
	sshUtil.getSSHConfig().hostIP = Ips
	scriptCommands := "sudo systemctl start hab-sup"
	printStartConnectionMessage(remoteService, sshUtil.getSSHConfig().hostIP, writer)
	_, err := sshUtil.connectAndExecuteCommandOnRemote(`sudo hab license accept; sudo hab svc status`, true)
	if err != nil {
		printStartErrorMessage(remoteService, Ips, cliWriter, err)
		return err
	}

	output, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		cliWriter.Errorf("%v\n", err)
		return err
	}

	err = checkOutputForError(output)
	if err != nil {
		cliWriter.Errorf("%v\n", err)
		return err
	}

	if err != nil {
		printStartErrorMessage(remoteService, Ips, cliWriter, err)
	} else {
		cliWriter.Printf("Output for Host IP %s : %s", Ips, output+"\n")
		printStartSuccessMessage(remoteService, Ips, cliWriter)
	}

	return nil
}

func printStartConnectionMessage(remoteService string, hostIP string, cliWriter *cli.Writer) {
	cliWriter.Println("Connecting to the " + remoteService + node + hostIP)
	cliWriter.BufferWriter().Flush()
}

func printStartErrorMessage(remoteService string, hostIP string, cliWriter *cli.Writer, err error) {
	cliWriter.Failf("Start Command failed on "+remoteService+node+hostIP+" with error:\n%v \n", err)
	cliWriter.BufferWriter().Flush()
}

func printStartSuccessMessage(remoteService string, hostIP string, cliWriter *cli.Writer) {
	cliWriter.Success("Start Command is completed on " + remoteService + node + hostIP + "\n")
	cliWriter.BufferWriter().Flush()
}
