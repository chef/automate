package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
)

type Cmd struct {
	PreExec   func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error
	CmdInputs *CmdInputs
}

func NewCmd() *Cmd {
	return &Cmd{CmdInputs: &CmdInputs{NodeType: false}}
}

type CmdInputs struct {
	Cmd                      string
	Args                     []string
	WaitTimeout              int
	Single                   bool
	NodeIps                  []string
	InputFiles               []string
	InputFilesPrefix         string
	Outputfiles              []string
	ErrorCheckEnableInOutput bool
	NodeType                 bool
	SkipPrintOutput          bool
	HideSSHConnectionMessage bool
	MutipleCmdWithArgs       map[string]string
}

type NodeTypeAndCmd struct {
	Frontend   *Cmd
	Automate   *Cmd
	ChefServer *Cmd
	Postgresql *Cmd
	Opensearch *Cmd
	Infra      *AutomateHAInfraDetails
}

func NewNodeTypeAndCmd() *NodeTypeAndCmd {
	return &NodeTypeAndCmd{
		Frontend:   NewCmd(),
		Automate:   NewCmd(),
		ChefServer: NewCmd(),
		Opensearch: NewCmd(),
		Postgresql: NewCmd(),
	}
}

type CmdResult struct {
	ScriptName  string
	HostIP      string
	OutputFiles []string
	Output      string
	Error       error
}

type RemoteCmdExecutor interface {
	Execute() (map[string][]*CmdResult, error)
	ExecuteWithNodeMap(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error)
	GetSshUtil() SSHUtil
	SetWriter(writer *cli.Writer)
}

type remoteCmdExecutor struct {
	NodeMap *NodeTypeAndCmd
	SshUtil SSHUtil
	Output  *cli.Writer
}

type MockRemoteCmdExecutor struct {
	ExecuteFunc            func() (map[string][]*CmdResult, error)
	ExecuteWithNodeMapFunc func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error)
	GetSshUtilFunc         func() SSHUtil
	SetWriterFunc          func(cli *cli.Writer)
}

func (m *MockRemoteCmdExecutor) Execute() (map[string][]*CmdResult, error) {
	return m.ExecuteFunc()
}

func (m *MockRemoteCmdExecutor) ExecuteWithNodeMap(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
	return m.ExecuteWithNodeMapFunc(nodeMap)
}

func (m *MockRemoteCmdExecutor) GetSshUtil() SSHUtil {
	return m.GetSshUtilFunc()
}

func (m *MockRemoteCmdExecutor) SetWriter(cli *cli.Writer) {
	m.SetWriterFunc(cli)
}

func NewRemoteCmdExecutor(nodeMap *NodeTypeAndCmd, sshUtil SSHUtil, writer *cli.Writer) RemoteCmdExecutor {
	return &remoteCmdExecutor{
		NodeMap: nodeMap,
		SshUtil: sshUtil,
		Output:  writer,
	}
}

func NewRemoteCmdExecutorWithoutNodeMap(sshUtil SSHUtil, writer *cli.Writer) RemoteCmdExecutor {
	return &remoteCmdExecutor{
		SshUtil: sshUtil,
		Output:  writer,
	}
}

func (c *remoteCmdExecutor) GetSshUtil() SSHUtil {
	return c.SshUtil
}

func (c *remoteCmdExecutor) SetWriter(writer *cli.Writer) {
	c.Output = writer
}

func (c *remoteCmdExecutor) ExecuteWithNodeMap(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
	return c.execute(nodeMap)
}

func (c *remoteCmdExecutor) Execute() (map[string][]*CmdResult, error) {
	return c.execute(c.NodeMap)
}

func (c *remoteCmdExecutor) execute(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
	cmdResult := map[string][]*CmdResult{}

	sshConfig := getSshDetails(nodeMap.Infra)
	c.SshUtil.setSSHConfig(sshConfig)

	switch true {
	case nodeMap.Frontend.CmdInputs.NodeType:
		const remoteService string = FRONTEND
		nodeIps, err := preCmdExecCheck(nodeMap.Frontend, c.SshUtil, nodeMap.Infra, remoteService, c.Output)
		if err != nil {
			return cmdResult, err
		}
		output := c.executeCmdOnGivenNodes(nodeMap.Frontend.CmdInputs, nodeIps, remoteService, nodeMap.Frontend.CmdInputs.InputFilesPrefix, c.Output)
		return output, nil
	case nodeMap.Automate.CmdInputs.NodeType:
		const remoteService string = AUTOMATE
		nodeIps, err := preCmdExecCheck(nodeMap.Automate, c.SshUtil, nodeMap.Infra, remoteService, c.Output)
		if err != nil {
			return cmdResult, err
		}

		output := c.executeCmdOnGivenNodes(nodeMap.Automate.CmdInputs, nodeIps, remoteService, nodeMap.Automate.CmdInputs.InputFilesPrefix, c.Output)
		return output, nil
	case nodeMap.ChefServer.CmdInputs.NodeType:
		const remoteService string = CHEF_SERVER
		nodeIps, err := preCmdExecCheck(nodeMap.ChefServer, c.SshUtil, nodeMap.Infra, remoteService, c.Output)
		if err != nil {
			return cmdResult, err
		}

		output := c.executeCmdOnGivenNodes(nodeMap.ChefServer.CmdInputs, nodeIps, remoteService, nodeMap.ChefServer.CmdInputs.InputFilesPrefix, c.Output)
		return output, nil
	case nodeMap.Postgresql.CmdInputs.NodeType:
		const remoteService string = POSTGRESQL
		nodeIps, err := preCmdExecCheck(nodeMap.Postgresql, c.SshUtil, nodeMap.Infra, remoteService, c.Output)
		if err != nil {
			return cmdResult, err
		}

		output := c.executeCmdOnGivenNodes(nodeMap.Postgresql.CmdInputs, nodeIps, remoteService, nodeMap.Postgresql.CmdInputs.InputFilesPrefix, c.Output)
		return output, nil
	case nodeMap.Opensearch.CmdInputs.NodeType:
		const remoteService string = OPENSEARCH
		nodeIps, err := preCmdExecCheck(nodeMap.Opensearch, c.SshUtil, nodeMap.Infra, remoteService, c.Output)
		if err != nil {
			return cmdResult, err
		}

		output := c.executeCmdOnGivenNodes(nodeMap.Opensearch.CmdInputs, nodeIps, remoteService, nodeMap.Opensearch.CmdInputs.InputFilesPrefix, c.Output)
		return output, nil
	default:
		return cmdResult, errors.New("Missing or Unsupported flag")
	}
}

// executeCmdOnGivenNodes executes given command/commands on all given nodes concurrently.
func (c *remoteCmdExecutor) executeCmdOnGivenNodes(input *CmdInputs, nodeIps []string, remoteService string, inputFilesPrefix string, cliWriter *cli.Writer) map[string][]*CmdResult {
	command := make(map[string]string)
	resultChan := make(chan CmdResult, len(nodeIps))
	ouputJsonResult := map[string][]*CmdResult{}
	originalSSHConfig := c.SshUtil.getSSHConfig()
	inputFiles := input.InputFiles
	outputFiles := input.Outputfiles
	if len(input.MutipleCmdWithArgs) == 0 {
		command["Script"] = input.Cmd
	} else {
		command = input.MutipleCmdWithArgs
	}
	timeout := input.WaitTimeout
	inputFileToOutputFileMap := map[string]string{}
	for _, file := range inputFiles {
		destinationFile := inputFilesPrefix + stringutils.GetFileName(file)
		inputFileToOutputFileMap[file] = destinationFile
	}

	for _, hostIP := range nodeIps {

		newSSHConfig := &SSHConfig{
			sshUser:    originalSSHConfig.sshUser,
			sshPort:    originalSSHConfig.sshPort,
			sshKeyFile: originalSSHConfig.sshKeyFile,
			hostIP:     hostIP,
			timeout:    timeout,
		}
		sshUtil := NewSSHUtil(newSSHConfig)
		if !input.HideSSHConnectionMessage {
			printConnectionMessage(remoteService, hostIP, cliWriter)
		}
		for scriptName, cmdScript := range command {
			go c.executeCmdOnNode(cmdScript, scriptName, inputFileToOutputFileMap, outputFiles, remoteService, input.ErrorCheckEnableInOutput, sshUtil, resultChan)
		}
	}

	for i := 0; i < len(nodeIps)*len(command); i++ {
		cliWriter.StartSpinner()
		result := <-resultChan
		cliWriter.StopSpinner()
		ouputJsonResult[result.HostIP] = append(ouputJsonResult[result.HostIP], &result)
		if !input.SkipPrintOutput {
			if i == 0 {
				cliWriter.Println("=====================================================")
				cliWriter.BufferWriter().Flush()
			}
			printOutput(remoteService, result, outputFiles, c.Output)
			if i < len(nodeIps)-1 {
				cliWriter.Println("=====================================================")
				cliWriter.BufferWriter().Flush()
			}
		}
	}

	status.GlobalResult = ouputJsonResult

	defer close(resultChan)

	if input.SkipPrintOutput {
		return ouputJsonResult
	}
	return map[string][]*CmdResult{}

}

// executeCmdOnNode function will run all remote jobs on a single node
func (c *remoteCmdExecutor) executeCmdOnNode(command, scriptName string, inputFiles map[string]string, outputFiles []string, remoteService string, errorCheckEnableInOutput bool, sshUtil SSHUtil, resultChan chan CmdResult) {
	rc := CmdResult{scriptName, sshUtil.getSSHConfig().hostIP, []string{}, "", nil}
	if len(inputFiles) != 0 {
		for sourceFile, destinationFile := range inputFiles {
			err := sshUtil.copyFileToRemote(sourceFile, destinationFile, false)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}
		}
	}

	output, err := sshUtil.connectAndExecuteCommandOnRemote(command, false)
	if err != nil {
		rc.Output = output
		rc.Error = err
		resultChan <- rc
		return
	}

	if len(outputFiles) != 0 {
		for _, file := range outputFiles {
			outFile := sshUtil.getSSHConfig().hostIP + "_" + stringutils.GetFileName(file)
			destFileName, err := sshUtil.copyFileFromRemote(file, outFile)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}
			rc.OutputFiles = append(rc.OutputFiles, destFileName)
		}
	}

	err = checkIfErrorPresentInOutput(errorCheckEnableInOutput, output)

	if err != nil {
		rc.Error = err
		resultChan <- rc
		return
	}

	rc.Output = output
	resultChan <- rc
}

func checkIfErrorPresentInOutput(errorCheckEnableInOutput bool, output string) error {
	if errorCheckEnableInOutput {
		return checkResultOutputForError(output)
	}
	return nil
}

// preCmdExecCheck will check and execute PreExec function. Also returns nodeips for given remoteservice with error if any.
func preCmdExecCheck(node *Cmd, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) ([]string, error) {
	var nodeIps []string
	var err error
	if (node.PreExec) != nil {
		err = node.PreExec(node.CmdInputs, sshUtil, infra, remoteService, writer)
		if err != nil {
			return nodeIps, err
		}
	}

	for _, ip := range node.CmdInputs.NodeIps {
		nodeIp, err := getNodeIPs(node.CmdInputs.Single, ip, infra, remoteService)
		if err != nil {
			return nodeIps, err
		}
		nodeIps = append(nodeIps, nodeIp...)
	}
	if len(node.CmdInputs.NodeIps) == 0 {
		nodeIps, err = getNodeIPs(node.CmdInputs.Single, "", infra, remoteService)
		if err != nil {
			return nodeIps, err
		}
	}

	if len(nodeIps) == 0 {
		errMsg := fmt.Sprintf("No %s IPs are found", remoteService)
		return nodeIps, errors.New(errMsg)
	}
	return nodeIps, nil
}

// getSshDetails will return the SSH details.
func getSshDetails(infra *AutomateHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// getFrontendIPs will return the Frontend Node Ips.
func getNodeIPs(single bool, ip string, infra *AutomateHAInfraDetails, remoteService string) ([]string, error) {
	nodeIps := []string{}
	switch {
	case remoteService == OPENSEARCH:
		nodeIps = infra.Outputs.OpensearchPrivateIps.Value
	case remoteService == POSTGRESQL:
		nodeIps = infra.Outputs.PostgresqlPrivateIps.Value
	case remoteService == CHEF_SERVER:
		nodeIps = infra.Outputs.ChefServerPrivateIps.Value
	case remoteService == AUTOMATE:
		nodeIps = infra.Outputs.AutomatePrivateIps.Value
	case remoteService == FRONTEND:
		nodeIps = append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	}
	if ip != "" {
		if isValidIP(ip, nodeIps) {
			return []string{ip}, nil
		}
		return []string{}, errors.New("Please Enter Valid Node IP")
	} else if single {
		ip, err := GetSingleIp(nodeIps)
		return []string{ip}, err
	}
	return nodeIps, nil
}

// GetSingleIp returns first ip from array of ips
func GetSingleIp(ips []string) (string, error) {
	if len(ips) == 0 {
		return "", errors.New("No ips found")
	}
	return ips[0], nil
}

// isValidIP will check whether the given ip is in the given remoteservice ips set or not.
func isValidIP(ip string, ips []string) bool {
	if !isValidIPFormat(ip) {
		return false
	}
	for _, clusterIP := range ips {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

func isValidIPFormat(ip string) bool {
	if strings.TrimSpace(ip) == "" {
		return false
	}
	match, _ := regexp.MatchString(IPV4REGEX, ip)
	return match
}

// printOutput of the remote jobs
func printOutput(remoteService string, result CmdResult, outputFiles []string, cliWriter *cli.Writer) {
	var resultFiles []string
	var err error
	if result.Error != nil && len(strings.TrimSpace(result.Output)) != 0 {
		printErrorMessage(remoteService, result.HostIP, cliWriter, result.Output)
	} else if result.Error != nil {
		printErrorMessage(remoteService, result.HostIP, cliWriter, result.Error.Error())
	} else {
		printSuccessMessage(remoteService, result.HostIP, cliWriter, result.Output)
	}
	if len(result.OutputFiles) != 0 {
		resultFiles, err = combineOutputFiles(result.HostIP, outputFiles, result.OutputFiles)
	}
	if err != nil {
		cliWriter.Println("Error while combining the output files")
	}
	if len(resultFiles) != 0 {
		for _, file := range resultFiles {
			cliWriter.Printf("Output file: %s", file+"\n")
		}
	}
}

// printErrorMessage prints the config error message
func printErrorMessage(remoteService string, hostIP string, cliWriter *cli.Writer, err string) {
	cliWriter.Fail("Command failed on " + remoteService + " node : " + hostIP + " with error:\n" + err + "\n")
	cliWriter.BufferWriter().Flush()
}

// printSuccessMessage prints the config success message
func printSuccessMessage(remoteService string, hostIP string, cliWriter *cli.Writer, output string) {
	cliWriter.Printf("Output for Host IP %s : \n%s", hostIP, output+"\n")
	cliWriter.Success("Command is executed on " + remoteService + " node : " + hostIP + "\n")
	cliWriter.BufferWriter().Flush()
}

// combineOutputFiles combines outfile from all node in one file.
func combineOutputFiles(hostIp string, inputOutputFiles, resultOutputFiles []string) ([]string, error) {
	var resultFile []string
	for _, file := range inputOutputFiles {
		for _, outfile := range resultOutputFiles {
			if strings.Contains(outfile, file) {
				result, err := appendChildFileToParentFile(hostIp, file, outfile)
				if err != nil {
					return resultFile, err
				}
				resultFile = append(resultFile, result)

			}
		}
	}

	return resultFile, nil
}

func appendChildFileToParentFile(hostIp, parent, child string) (string, error) {
	f, err := os.OpenFile(parent, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return "", err
	}
	defer f.Close()

	fileUtils := &fileutils.FileSystemUtils{}
	output, err := fileUtils.ReadFile(child)
	if err != nil {
		return "", err
	}

	fileContent := string(output)
	start := fmt.Sprintf("Output for Host IP %s : \n%s\n", hostIp, fileContent)

	_, err = f.WriteString(start)

	if err != nil {
		return "", err
	}
	return parent, nil

}

// checkResultOutputForError checks If the output contains the word "error" then return error
func checkResultOutputForError(output string) error {
	if strings.Contains(strings.ToUpper(strings.TrimSpace(output)), "ERROR") {
		return errors.New(output)
	}
	return nil
}
