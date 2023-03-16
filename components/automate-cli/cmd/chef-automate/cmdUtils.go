package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

type Cmd struct {
	PreExec   func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error
	CmdInputs *CmdInputs
}

type CmdInputs struct {
	Cmd                      string
	Args                     []string
	WaitTimeout              int
	Single                   bool
	NodeIp                   string
	InputFiles               []string
	Outputfiles              []string
	ErrorCheckEnableInOutput bool
	NodeType                 bool
}

type NodeTypeAndCmd struct {
	Frontend   *Cmd
	Automate   *Cmd
	ChefServer *Cmd
	Postgresql *Cmd
	Opensearch *Cmd
	Infra      *AutomteHAInfraDetails
}

type CmdResult struct {
	HostIP      string
	OutputFiles []string
	Output      string
	Error       error
}

type RemoteCmdExecutor interface {
	Execute() error
}

type remoteCmdExecutor struct {
	NodeMap *NodeTypeAndCmd
	SshUtil SSHUtil
}

func NewRemoteCmdExecutor(nodeMap *NodeTypeAndCmd) RemoteCmdExecutor {
	return &remoteCmdExecutor{
		NodeMap: nodeMap,
		SshUtil: NewSSHUtil(&SSHConfig{}),
	}
}

func (c *remoteCmdExecutor) Execute() error {
	timestamp := time.Now().Format("20060102150405")

	sshConfig := getSshDetails(c.NodeMap.Infra)
	c.SshUtil.setSSHConfig(sshConfig)

	switch true {
	case c.NodeMap.Frontend.CmdInputs.NodeType:
		const remoteService string = CONST_FRONTEND
		nodeIps, err := preCmdExecCheck(c.NodeMap.Frontend, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		c.executeCmdOnGivenNodes(c.NodeMap.Frontend.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Automate.CmdInputs.NodeType:
		const remoteService string = CONST_AUTOMATE
		nodeIps, err := preCmdExecCheck(c.NodeMap.Automate, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		c.executeCmdOnGivenNodes(c.NodeMap.Automate.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.ChefServer.CmdInputs.NodeType:
		const remoteService string = CONST_CHEF_SERVER
		nodeIps, err := preCmdExecCheck(c.NodeMap.ChefServer, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		c.executeCmdOnGivenNodes(c.NodeMap.ChefServer.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Postgresql.CmdInputs.NodeType:
		const remoteService string = CONST_POSTGRESQL
		nodeIps, err := preCmdExecCheck(c.NodeMap.Postgresql, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		c.executeCmdOnGivenNodes(c.NodeMap.Postgresql.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Opensearch.CmdInputs.NodeType:
		const remoteService string = CONST_OPENSEARCH
		nodeIps, err := preCmdExecCheck(c.NodeMap.Opensearch, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		c.executeCmdOnGivenNodes(c.NodeMap.Opensearch.CmdInputs, nodeIps, remoteService, timestamp, writer)
	default:
		return errors.New("Missing or Unsupported flag")
	}

	return nil
}

// executeCmdOnGivenNodes executes given command/commands on all given nodes concurrently.
func (c *remoteCmdExecutor) executeCmdOnGivenNodes(input *CmdInputs, nodeIps []string, remoteService string, timestamp string, cliWriter *cli.Writer) {
	resultChan := make(chan CmdResult, len(nodeIps))
	originalSSHConfig := c.SshUtil.getSSHConfig()
	inputFiles := input.InputFiles
	outputFiles := input.Outputfiles
	command := input.Cmd
	timeout := input.WaitTimeout

	inputFileToOutputFileMap := map[string]string{}
	for _, file := range inputFiles {
		destinationFile := remoteService + "_" + timestamp + "_" + file
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

		printConnectionMessage(remoteService, hostIP, cliWriter)

		go c.executeCmdOnNode(command, inputFileToOutputFileMap, outputFiles, remoteService, input.ErrorCheckEnableInOutput, sshUtil, resultChan)
	}

	ouputJsonResult := map[string]*CmdResult{}

	for i := 0; i < len(nodeIps); i++ {
		result := <-resultChan

		ouputJsonResult[result.HostIP] = &result

		if i == 0 {
			writer.StopSpinner()
			cliWriter.Println("=====================================================")
		}

		printOutput(remoteService, result, outputFiles, cliWriter)

		if i < len(nodeIps)-1 {
			cliWriter.Println("=====================================================")
			writer.StartSpinner()
		}
	}
	status.GlobalResult = ouputJsonResult

	close(resultChan)
}

// executeCmdOnNode function will run all remote jobs on a single node
func (c *remoteCmdExecutor) executeCmdOnNode(command string, inputFiles map[string]string, outputFiles []string, remoteService string, errorCheckEnableInOutput bool, sshUtil SSHUtil, resultChan chan CmdResult) {
	rc := CmdResult{sshUtil.getSSHConfig().hostIP, []string{}, "", nil}
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

	output, err := sshUtil.connectAndExecuteCommandOnRemote(command, true)
	if err != nil {
		rc.Error = err
		resultChan <- rc
		return
	}

	if len(outputFiles) != 0 {
		for _, file := range outputFiles {
			outFile := sshUtil.getSSHConfig().hostIP + "_" + file
			destFileName, err := sshUtil.copyFileFromRemote(file, outFile)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}
			writer.Printf("File downloaded %s \n", destFileName)
			rc.OutputFiles = append(rc.OutputFiles, destFileName)
		}
	}

	if errorCheckEnableInOutput {
		err = checkResultOutputForError(output)
	}
	if err != nil {
		rc.Error = err
		resultChan <- rc
		return
	}

	rc.Output = output
	resultChan <- rc
}

// preCmdExecCheck will check and execute PreExec function. Also returns nodeips for given remoteservice with error if any.
func preCmdExecCheck(node *Cmd, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) ([]string, error) {
	var nodeIps []string
	var err error
	if (node.PreExec) != nil {
		err = node.PreExec(node.CmdInputs, sshUtil, infra, remoteService, timestamp, writer)
		if err != nil {
			return nodeIps, err
		}
	}

	nodeIps, err = getNodeIPs(node.CmdInputs.Single, node.CmdInputs.NodeIp, infra, remoteService)

	if err != nil {
		return nodeIps, err
	}
	if len(nodeIps) == 0 {
		errMsg := fmt.Sprintf("No %s IPs are found", remoteService)
		return nodeIps, errors.New(errMsg)
	}
	return nodeIps, nil
}

// getSshDetails will return the SSH details.
func getSshDetails(infra *AutomteHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// getFrontendIPs will return the Frontend Node Ips.
func getNodeIPs(single bool, ip string, infra *AutomteHAInfraDetails, remoteService string) ([]string, error) {
	nodeIps := []string{}
	switch {
	case remoteService == CONST_OPENSEARCH:
		nodeIps = infra.Outputs.OpensearchPrivateIps.Value
	case remoteService == CONST_POSTGRESQL:
		nodeIps = infra.Outputs.PostgresqlPrivateIps.Value
	case remoteService == CONST_CHEF_SERVER:
		nodeIps = infra.Outputs.ChefServerPrivateIps.Value
	case remoteService == CONST_AUTOMATE:
		nodeIps = infra.Outputs.AutomatePrivateIps.Value
	case remoteService == CONST_FRONTEND:
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
	ipv4Regex := `^(((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4})`
	match, _ := regexp.MatchString(ipv4Regex, ip)
	if !match {
		return false
	}
	for _, clusterIP := range ips {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

// printOutput of the remote jobs
func printOutput(remoteService string, result CmdResult, outputFiles []string, cliWriter *cli.Writer) {
	var resultFiles []string
	var err error
	if result.Error != nil {
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
