package main

import (
	"container/list"
	"os"
	"os/exec"
	"syscall"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const node = " node :"

type Result struct {
	HostIP string
	Output string
	Error  error
}

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
		if isManagedServicesOn() {
			err := errorOnManaged(startCmdFlags.postgresql, startCmdFlags.opensearch)
			if err != nil {
				return status.Annotate(err, status.InvalidCommandArgsError)
			}
		}
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
		sshUtil := NewSSHUtil(sshConfig)
		errorList := list.New()
		if startCmdFlags.automate {
			frontendIps := infra.Outputs.AutomatePrivateIps.Value
			err = checkNodes(args, sshUtil, frontendIps, "automate", writer)
			if err != nil {
				errorList.PushBack(err.Error())
			}
		}
		if startCmdFlags.chef_server {
			frontendIps := infra.Outputs.ChefServerPrivateIps.Value
			err = checkNodes(args, sshUtil, frontendIps, "chef-server", writer)
			if err != nil {
				errorList.PushBack(err.Error())
			}
		}
		if startCmdFlags.opensearch {
			backendIps := infra.Outputs.OpensearchPrivateIps.Value
			err = checkNodes(args, sshUtil, backendIps, "opensearch", writer)
			if err != nil {
				errorList.PushBack(err.Error())
			}
		}
		if startCmdFlags.postgresql {
			backendIps := infra.Outputs.PostgresqlPrivateIps.Value
			err = checkNodes(args, sshUtil, backendIps, "postgresql", writer)
			return err
		}
		if errorList.Len() > 0 {
			return getSingleErrorFromList(errorList)
		}
		return nil
	} else {
		writer.Title("Starting Chef Automate")
		if isDevMode() {
			if err := exec.Command("hab", "sup", "status").Run(); err == nil {
				return nil
			}

			if err := os.MkdirAll("/hab/sup/default", 0755); err != nil {
				return status.Annotate(err, status.FileAccessError)
			}
			writer.Title("Launching the Habitat Supervisor in the background...")
			out, err := os.Create("/hab/sup/default/sup.log")
			if err != nil {
				return status.Annotate(err, status.FileAccessError)
			}
			startSupCmd := exec.Command("hab", "sup", "run")
			startSupCmd.Env = os.Environ()
			startSupCmd.Env = append(startSupCmd.Env,
				"DS_DEV=true",
				"CHEF_AUTOMATE_SKIP_SYSTEMD=true",
				"HAB_LICENSE=accept-no-persist",
			)
			startSupCmd.Stdout = out
			startSupCmd.Stderr = out
			startSupCmd.SysProcAttr = &syscall.SysProcAttr{
				Setpgid: true,
			}
			if err := startSupCmd.Start(); err != nil {
				return status.Annotate(err, status.HabCommandError)
			}
		} else {
			systemctlCmd := exec.Command("systemctl", "start", "chef-automate.service")
			systemctlCmd.Stdout = os.Stdout
			systemctlCmd.Stderr = os.Stderr
			if err := systemctlCmd.Run(); err != nil {
				return status.Annotate(err, status.ServiceStartError)
			}
		}
	}
	return nil
}

func startFrontEndNodes(args []string, sshUtil SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	resultChan := make(chan Result, len(ips))
	scriptCommands := "sudo chef-automate start"
	for _, hostIP := range ips {
		sshUtil.getSSHConfig().hostIP = hostIP
		rc := Result{hostIP, "", nil}
		output, err := runCommand(scriptCommands, sshUtil)
		if err != nil {
			rc.Error = err
			resultChan <- rc
		}

		err = checkOutputForError(output)
		if err != nil {
			rc.Error = err
			resultChan <- rc
		}

		rc.Output = output
		resultChan <- rc
	}

	for i := 0; i < len(ips); i++ {
		result := <-resultChan

		printStartConnectionMessage(remoteService, result.HostIP, cliWriter)
		if result.Error != nil {
			printStartErrorMessage(remoteService, result.HostIP, cliWriter, result.Error)
			return result.Error
		} else {
			cliWriter.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printStartSuccessMessage(remoteService, result.HostIP, cliWriter)
		}

	}

	close(resultChan)
	return nil
}

func runCommand(scriptCommands string, newSSHUtil SSHUtil) (string, error) {
	output, err := newSSHUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	return output, err
}

func checkNodes(args []string, sshUtil SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	if len(ips) == 0 {
		writer.Errorf("No %s IPs are found", remoteService)
		return status.Errorf(1, "No %s IPs are found", remoteService)
	}
	errorList := list.New()
	if remoteService == "opensearch" || remoteService == "postgresql" {
		for i := 0; i < len(ips); i++ {
			err := startBackEndNodes(args, sshUtil, ips[i], remoteService, cliWriter)
			if err != nil {
				errorList.PushBack(err.Error())
			}
		}
	} else {
		err := startFrontEndNodes(args, sshUtil, ips, remoteService, cliWriter)
		return err
	}
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errorList), status.ServiceStartError, "Not able to start one or more nodes")
	}
	return nil
}

func startBackEndNodes(args []string, sshUtil SSHUtil, ip string, remoteService string, cliWriter *cli.Writer) error {
	sshUtil.getSSHConfig().hostIP = ip
	scriptCommands := "sudo systemctl start hab-sup"
	printStartConnectionMessage(remoteService, sshUtil.getSSHConfig().hostIP, writer)
	_, err := sshUtil.connectAndExecuteCommandOnRemote(`sudo hab license accept; sudo hab svc status`, true)
	if err != nil {
		printStartErrorMessage(remoteService, ip, cliWriter, err)
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
		printStartErrorMessage(remoteService, ip, cliWriter, err)
		return err
	} else {
		cliWriter.Printf("Output for Host IP %s : %s", ip, output+"\n")
		printStartSuccessMessage(remoteService, ip, cliWriter)
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

func errorOnManaged(isPostgresql, isOpenSearch bool) error {

	if isPostgresql {
		return errors.Errorf("Start services in %s for externally configured is not supported", "Postgresql")
	}
	if isOpenSearch {
		return errors.Errorf("Start services in %s for externally configured is not supported", "OpenSearch")
	}
	return nil
}
