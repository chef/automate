package main

import (
	"container/list"
	"os"
	"os/exec"
	"syscall"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const node = " node :"
const (
	ERROR_ON_MANAGED_SERVICES_START = "Starting the service for externally configured %s is not supported"
	opensearch_service              = "opensearch"
	postgresql_service              = "postgresql"
)

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
		if !startCmdFlags.automate && !startCmdFlags.chef_server && !startCmdFlags.opensearch && !startCmdFlags.postgresql {
			writer.Println(cmd.UsageString())
		}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		if err = runStartCommandHA(infra, args); err != nil {
			return err
		}
	} else if isDevMode() {
		if err := runStartDevMode(); err != nil {
			return err
		}

	} else {
		if err := runStartStandalone(); err != nil {
			return err
		}
	}
	return nil
}

func runStartCommandHA(infra *AutomteHAInfraDetails, args []string) error {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
	}
	sshUtil := NewSSHUtil(sshConfig)
	errorList := list.New()
	var err error
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
		if isManagedServicesOn() {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES_START, "OpenSearch")
		}
		backendIps := infra.Outputs.OpensearchPrivateIps.Value
		err = checkNodes(args, sshUtil, backendIps, opensearch_service, writer)
		if err != nil {
			errorList.PushBack(err.Error())
		}
	}
	if startCmdFlags.postgresql {
		if isManagedServicesOn() {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICES_START, "Postgresql")
		}
		backendIps := infra.Outputs.PostgresqlPrivateIps.Value
		err = checkNodes(args, sshUtil, backendIps, postgresql_service, writer)
		if err != nil {
			errorList.PushBack(err.Error())
		}
	}
	if errorList.Len() > 0 {
		return status.New(status.ServiceStartError, getSingleErrorFromList(errorList).Error())
	}
	return nil
}

func runStartStandalone() error {
	writer.Title("Starting Chef Automate")
	systemctlCmd := exec.Command("systemctl", "start", "chef-automate.service")
	systemctlCmd.Stdout = os.Stdout
	systemctlCmd.Stderr = os.Stderr
	if err := systemctlCmd.Run(); err != nil {
		return status.Annotate(err, status.ServiceStartError)
	}
	return nil
}

func runStartDevMode() error {
	writer.Title("Starting Chef Automate")
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
	return nil
}

func checkNodes(args []string, sshUtil SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	if len(ips) == 0 {
		writer.Errorf("No %s IPs are found", remoteService)
		return status.Errorf(1, "No %s IPs are found", remoteService)
	}
	if remoteService == opensearch_service || remoteService == postgresql_service {
		return startBackEndNodes(args, sshUtil, ips, remoteService, cliWriter)
	}
	err := startFrontEndNodes(args, sshUtil, ips, remoteService, cliWriter)
	return err
}

func startFrontEndNodes(args []string, sshUtil SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	resultChan := make(chan Result, len(ips))
	errorList := list.New()
	scriptCommands := `sudo chef-automate start`
	for _, hostIP := range ips {
		go commandExecuteFrontEnd(scriptCommands, sshUtil, resultChan, hostIP)

	}
	printErrorsForStartResultChan(resultChan, ips, remoteService, cliWriter, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceStartError, "Not able to start one or more nodes in %s", remoteService)
	}
	return nil
}

func commandExecuteFrontEnd(scriptCommands string, sshUtil SSHUtil, resultChan chan Result, hostIP string) {
	sshUtil.getSSHConfig().hostIP = hostIP
	rc := Result{sshUtil.getSSHConfig().hostIP, "", nil}
	output, err := runCommand(scriptCommands, sshUtil)
	if err != nil {
		rc.Error = err
		resultChan <- rc
	}
	rc.Output = output
	resultChan <- rc
}

func printErrorsForStartResultChan(resultChan chan Result, ips []string, remoteService string, cliWriter *cli.Writer, errorList *list.List) {
	for i := 0; i < len(ips); i++ {
		result := <-resultChan
		printStartConnectionMessage(remoteService, result.HostIP, cliWriter)
		if result.Error != nil {
			printStartErrorMessage(remoteService, result.HostIP, cliWriter, result.Error)
			errorList.PushBack(result.Error.Error())
		} else {
			cliWriter.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printStartSuccessMessage(remoteService, result.HostIP, cliWriter)
		}
	}
}

func startBackEndNodes(args []string, sshUtil SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	resultChan := make(chan Result, len(ips))
	scriptCommands := "sudo systemctl start hab-sup"
	errorList := list.New()
	for _, hostIP := range ips {
		go commandExecuteBackendNode(scriptCommands, sshUtil, resultChan, hostIP)
	}
	printErrorsForStartResultChan(resultChan, ips, remoteService, cliWriter, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceStartError, "Not able to start one or more nodes in %s", remoteService)
	}
	return nil
}

func commandExecuteBackendNode(scriptCommands string, sshUtil SSHUtil, resultChan chan Result, hostIp string) {
	sshUtil.getSSHConfig().hostIP = hostIp
	rc := Result{sshUtil.getSSHConfig().hostIP, "", nil}
	// Running the 'hab svc status' command to check if hab is present
	_, err := sshUtil.connectAndExecuteCommandOnRemote(`sudo hab license accept; sudo hab svc status`, true)
	if err != nil {
		rc.Error = err
		resultChan <- rc
		return
	}
	// Executing the systemctl command for starting the service
	output, err := runCommand(scriptCommands, sshUtil)
	if err != nil {
		rc.Error = err
		resultChan <- rc
	}
	rc.Output = output
	resultChan <- rc
}

func runCommand(scriptCommands string, newSSHUtil SSHUtil) (string, error) {
	output, err := newSSHUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		return "", err
	}
	err = checkOutputForError(output)
	if err != nil {
		return "", err
	}
	return output, err
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
