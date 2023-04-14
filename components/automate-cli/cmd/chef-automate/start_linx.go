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

const (
	ERROR_ON_MANAGED_SERVICE_START = "Starting the service for externally configured %s is not supported"
	OPENSEARCH_SERVICE             = "opensearch"
	POSTGRES_SERVICE               = "postgresql"
	NODE                           = " node :"
)

type Result struct {
	HostIP string
	Output string
	Error  error
}

var startCmdFlags = struct {
	automate   bool
	chefServer bool
	opensearch bool
	postgresql bool
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
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.chefServer, "chef_server", "c", false, "start chef automate service on chef-server nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.chefServer, "cs", false, "start chef automate service on chef-server nodes[DUPLICATE]")
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.opensearch, "opensearch", "o", false, "start hab-sup service on opensearch nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.opensearch, "os", false, "start hab-sup service on opensearch nodes[DUPLICATE]")
	startCommand.PersistentFlags().BoolVarP(&startCmdFlags.postgresql, "postgresql", "p", false, "start hab-sup service on postgresql nodes")
	startCommand.PersistentFlags().BoolVar(&startCmdFlags.postgresql, "pg", false, "start hab-sup service on postgresql nodes[DUPLICATE]")
	RootCmd.AddCommand(startCommand)
}

func runStartCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if !startCmdFlags.automate && !startCmdFlags.chefServer && !startCmdFlags.opensearch && !startCmdFlags.postgresql {
			writer.Println(cmd.UsageString())
			return nil
		}
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		if err = runStartCommandHA(infra, args, isManagedServicesOn()); err != nil {
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

func runStartCommandHA(infra *AutomateHAInfraDetails, args []string, isManagedServices bool) error {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
	}
	errorList := list.New()
	if startCmdFlags.automate {
		frontendIps := infra.Outputs.AutomatePrivateIps.Value
		startCommandImplHA(args, *sshConfig, frontendIps, "automate", writer, errorList)
	}
	if startCmdFlags.chefServer {
		frontendIps := infra.Outputs.ChefServerPrivateIps.Value
		startCommandImplHA(args, *sshConfig, frontendIps, "chef-server", writer, errorList)
	}
	if startCmdFlags.opensearch {
		if isManagedServices {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICE_START, OPENSEARCH_SERVICE)
		}
		backendIps := infra.Outputs.OpensearchPrivateIps.Value
		startCommandImplHA(args, *sshConfig, backendIps, OPENSEARCH_SERVICE, writer, errorList)
	}
	if startCmdFlags.postgresql {
		if isManagedServices {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_ON_MANAGED_SERVICE_START, POSTGRES_SERVICE)
		}
		backendIps := infra.Outputs.PostgresqlPrivateIps.Value
		startCommandImplHA(args, *sshConfig, backendIps, POSTGRES_SERVICE, writer, errorList)
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

func startCommandImplHA(args []string, sshConfig SSHConfig, ips []string, remoteService string, cliWriter *cli.Writer, errorList *list.List) {
	sshUtilMap := getMapSSHUtils(ips, &sshConfig)
	err := checkNodes(args, sshUtilMap, ips, remoteService, writer)
	if err != nil {
		errorList.PushBack(err.Error())
	}
}

func checkNodes(args []string, sshUtilMap map[string]SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	if len(ips) == 0 {
		writer.Errorf("No %s IPs are found", remoteService)
		return status.Errorf(1, "No %s IPs are found", remoteService)
	}
	if remoteService == OPENSEARCH_SERVICE || remoteService == POSTGRES_SERVICE {
		return startBackEndNodes(args, sshUtilMap, ips, remoteService, cliWriter)
	}
	return startFrontEndNodes(args, sshUtilMap, ips, remoteService, cliWriter)
}

func startFrontEndNodes(args []string, sshUtilMap map[string]SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	resultChan := make(chan Result, len(ips))
	errorList := list.New()
	scriptCommands := `sudo chef-automate start`
	for _, hostIP := range ips {
		sshUtil := sshUtilMap[hostIP]
		go commandExecuteFrontEnd(scriptCommands, sshUtil, resultChan)

	}
	printErrorsForStartResultChan(resultChan, ips, remoteService, cliWriter, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceStartError, "Not able to start one or more nodes in %s", remoteService)
	}
	return nil
}

func commandExecuteFrontEnd(scriptCommands string, sshUtil SSHUtil, resultChan chan Result) {
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

func startBackEndNodes(args []string, sshUtilMap map[string]SSHUtil, ips []string, remoteService string, cliWriter *cli.Writer) error {
	resultChan := make(chan Result, len(ips))
	scriptCommands := "sudo systemctl start hab-sup"
	errorList := list.New()
	for _, hostIP := range ips {
		sshUtil := sshUtilMap[hostIP]
		go commandExecuteBackendNode(scriptCommands, sshUtil, resultChan)
	}
	printErrorsForStartResultChan(resultChan, ips, remoteService, cliWriter, errorList)
	close(resultChan)
	if errorList != nil && errorList.Len() > 0 {
		return status.Wrapf(getSingleErrorFromList(errorList), status.ServiceStartError, "Not able to start one or more nodes in %s", remoteService)
	}

	return nil
}

func commandExecuteBackendNode(scriptCommands string, sshUtil SSHUtil, resultChan chan Result) {
	rc := Result{sshUtil.getSSHConfig().hostIP, "", nil}
	// Running the 'hab svc status' command to check if hab is present
	_, err := sshUtil.connectAndExecuteCommandOnRemote(`sudo hab license accept; sudo hab -V`, true)
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
	cliWriter.Println("Connecting to the " + remoteService + NODE + hostIP)
	cliWriter.BufferWriter().Flush()
}

func printStartErrorMessage(remoteService string, hostIP string, cliWriter *cli.Writer, err error) {
	cliWriter.Failf("Start Command failed on "+remoteService+NODE+hostIP+" with error:\n%v \n", err)
	cliWriter.BufferWriter().Flush()
}

func printStartSuccessMessage(remoteService string, hostIP string, cliWriter *cli.Writer) {
	cliWriter.Success("Start Command is completed on " + remoteService + NODE + hostIP + "\n")
	cliWriter.BufferWriter().Flush()
}

func getMapSSHUtils(ipAddresses []string, config *SSHConfig) map[string]SSHUtil {
	sshUtilMap := make(map[string]SSHUtil)
	for _, ipAddress := range ipAddresses {
		newSSHConfig := &SSHConfig{
			sshUser:    config.sshUser,
			sshPort:    config.sshPort,
			sshKeyFile: config.sshKeyFile,
			hostIP:     ipAddress,
		}
		newSSHUtil := NewSSHUtil(newSSHConfig)
		sshUtilMap[ipAddress] = newSSHUtil
	}
	return sshUtilMap
}
