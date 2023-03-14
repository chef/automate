package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

type Cmd struct {
	PreExec   func(cmdInputs *CmdInputs, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) error
	CmdInputs *CmdInputs
}

type CmdInputs struct {
	Cmd         string
	Args        []string
	Single      bool
	NodeIp      string
	InputFiles  []string
	Outputfiles []string
	ErrorCheck  bool
	NodeType    bool
}

type NodeTypeAndCmd struct {
	Frontend    *Cmd
	Automate    *Cmd
	Chef_server *Cmd
	Postgresql  *Cmd
	Opensearch  *Cmd
	Infra       *AutomteHAInfraDetails
}

type CmdResult struct {
	HostIP      string
	OutputFiles []string
	Output      string
	Error       error
}

type CmdUtil interface {
	RunCommand() error
	CmdExecOnNodes(input *CmdInputs, nodeIps []string, remoteService string, timestamp string, cliWriter *cli.Writer) error
	RemoteJobs(command string, inputFiles map[string]string, outputFiles []string, remoteService string, errorCheck bool, newSSHUtil SSHUtil, resultChan chan CmdResult)
}

type CmdUtilImpl struct {
	NodeMap *NodeTypeAndCmd
	SshUtil SSHUtil
}

func NewCmdUtil(nodeMap *NodeTypeAndCmd) CmdUtil {
	return &CmdUtilImpl{
		NodeMap: nodeMap,
		SshUtil: NewSSHUtil(&SSHConfig{}),
	}
}

func (c *CmdUtilImpl) RunCommand() error {
	var err error
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

		err = c.CmdExecOnNodes(c.NodeMap.Frontend.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Automate.CmdInputs.NodeType:
		const remoteService string = CONST_AUTOMATE
		nodeIps, err := preCmdExecCheck(c.NodeMap.Automate, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		err = c.CmdExecOnNodes(c.NodeMap.Automate.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Chef_server.CmdInputs.NodeType:
		const remoteService string = CONST_CHEF_SERVER
		nodeIps, err := preCmdExecCheck(c.NodeMap.Chef_server, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		err = c.CmdExecOnNodes(c.NodeMap.Chef_server.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Postgresql.CmdInputs.NodeType:
		const remoteService string = CONST_POSTGRESQL
		nodeIps, err := preCmdExecCheck(c.NodeMap.Postgresql, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		err = c.CmdExecOnNodes(c.NodeMap.Postgresql.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Opensearch.CmdInputs.NodeType:
		const remoteService string = CONST_OPENSEARCH
		nodeIps, err := preCmdExecCheck(c.NodeMap.Opensearch, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
		if err != nil {
			return err
		}

		err = c.CmdExecOnNodes(c.NodeMap.Opensearch.CmdInputs, nodeIps, remoteService, timestamp, writer)
	default:
		return errors.New("Missing or Unsupported flag")
	}
	if err != nil {
		return err
	}

	return nil
}

func (c *CmdUtilImpl) CmdExecOnNodes(input *CmdInputs, nodeIps []string, remoteService string, timestamp string, cliWriter *cli.Writer) error {
	resultChan := make(chan CmdResult, len(nodeIps))
	originalSSHConfig := c.SshUtil.getSSHConfig()
	inputFiles := input.InputFiles
	outputFiles := input.Outputfiles
	command := input.Cmd

	inputFileToOutputFileMap := map[string]string{}
	for _, file := range inputFiles {
		configFile := remoteService + "_" + timestamp + "_" + file
		inputFileToOutputFileMap[file] = configFile
	}

	for _, hostIP := range nodeIps {
		newSSHConfig := &SSHConfig{
			sshUser:    originalSSHConfig.sshUser,
			sshPort:    originalSSHConfig.sshPort,
			sshKeyFile: originalSSHConfig.sshKeyFile,
			hostIP:     hostIP,
		}
		newSSHUtil := NewSSHUtil(newSSHConfig)

		printConnectionMessage(remoteService, hostIP, cliWriter)

		go c.RemoteJobs(command, inputFileToOutputFileMap, outputFiles, remoteService, input.ErrorCheck, newSSHUtil, resultChan)
	}

	for i := 0; i < len(nodeIps); i++ {
		result := <-resultChan

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

	close(resultChan)
	return nil
}

// RemoteJobs function will run all remote jobs.
func (c *CmdUtilImpl) RemoteJobs(command string, inputFiles map[string]string, outputFiles []string, remoteService string, errorCheck bool, newSSHUtil SSHUtil, resultChan chan CmdResult) {
	rc := CmdResult{newSSHUtil.getSSHConfig().hostIP, []string{}, "", nil}
	if len(inputFiles) != 0 {
		for inputFile, outputFile := range inputFiles {
			err := newSSHUtil.copyFileToRemote(inputFile, outputFile, false)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}
		}
	}

	output, err := newSSHUtil.connectAndExecuteCommandOnRemote(command, true)
	if err != nil {
		rc.Error = err
		resultChan <- rc
		return
	}

	if len(outputFiles) != 0 {
		for _, file := range outputFiles {
			outFile := newSSHUtil.getSSHConfig().hostIP + "_" + file
			destFileName, err := newSSHUtil.copyFileFromRemote(file, outFile)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}
			writer.Printf("File downloaded %s \n", destFileName)
			rc.OutputFiles = append(rc.OutputFiles, destFileName)
		}
	}

	if errorCheck {
		err = checkResultOutputForError(output)
		if err != nil {
			rc.Error = err
			resultChan <- rc
			return
		}
	}

	rc.Output = output
	resultChan <- rc
}

// preCmdExecCheck will check and execute PreExec function
func preCmdExecCheck(node *Cmd, sshUtil SSHUtil, infra *AutomteHAInfraDetails, remoteService string, timestamp string, writer *cli.Writer) ([]string, error) {
	var nodeIps []string
	var err error
	if (node.PreExec) != nil {
		err = node.PreExec(node.CmdInputs, sshUtil, infra, remoteService, timestamp, writer)
		if err != nil {
			return nodeIps, err
		}
	}

	if remoteService == CONST_FRONTEND {
		nodeIps, err = isFrontendIPs(node.CmdInputs.NodeIp, infra)
	} else if remoteService == CONST_AUTOMATE {
		nodeIps, err = isAutomateIPs(node.CmdInputs.Single, node.CmdInputs.NodeIp, infra)
	} else if remoteService == CONST_CHEF_SERVER {
		nodeIps, err = isChefserverIPs(node.CmdInputs.Single, node.CmdInputs.NodeIp, infra)
	} else if remoteService == CONST_POSTGRESQL {
		nodeIps, err = isPostgresqlIPs(node.CmdInputs.Single, node.CmdInputs.NodeIp, infra)
	} else if remoteService == CONST_OPENSEARCH {
		nodeIps, err = isOpensearchIPs(node.CmdInputs.Single, node.CmdInputs.NodeIp, infra)
	}

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

// isFrontendIPs will return the Frontend Node Ips.
func isFrontendIPs(ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	frontendNodes := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	if ip != "" {
		if isValidIP(ip, frontendNodes) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid frontend IP")
		}
	} else {
		return frontendNodes, nil
	}
}

// IsAutomateIPs will return the Automate Node Ips.
func isAutomateIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if isValidIP(ip, infra.Outputs.AutomatePrivateIps.Value) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid automate IP")
		}
	} else if single {
		return []string{infra.Outputs.AutomatePrivateIps.Value[0]}, nil
	} else {
		return infra.Outputs.AutomatePrivateIps.Value, nil
	}
}

// IsChefserverIPs will return the Chefserver Node Ips.
func isChefserverIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if isValidIP(ip, infra.Outputs.ChefServerPrivateIps.Value) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid chef-server IP")
		}
	} else if single {
		return []string{infra.Outputs.ChefServerPrivateIps.Value[0]}, nil
	} else {
		return infra.Outputs.ChefServerPrivateIps.Value, nil
	}
}

// IsPostgresqlIPs will return the Postgresql Node Ips.
func isPostgresqlIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if isValidIP(ip, infra.Outputs.PostgresqlPrivateIps.Value) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid postgresql IP")
		}
	} else if single {
		return []string{infra.Outputs.PostgresqlPrivateIps.Value[0]}, nil
	} else {
		return infra.Outputs.PostgresqlPrivateIps.Value, nil
	}
}

// IsOpensearchIPs will return the Opensearch Node Ips.
func isOpensearchIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if isValidIP(ip, infra.Outputs.OpensearchPrivateIps.Value) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid opensearch IP")
		}
	} else if single {
		return []string{infra.Outputs.OpensearchPrivateIps.Value[0]}, nil
	} else {
		return infra.Outputs.OpensearchPrivateIps.Value, nil
	}
}

// isValidIP will check whether the given ip is in the given remoteservice ips set or not.
func isValidIP(ip string, ips []string) bool {
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

	output, err := ioutil.ReadFile(child) // nosemgrep
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
