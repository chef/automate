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
	GetSshDetails(infra *AutomteHAInfraDetails) *SSHConfig
	IsValidIP(ip string, ips []string) bool
	IsFrontendIPs(ip string, infra *AutomteHAInfraDetails) ([]string, error)
	IsAutomateIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error)
	IsChefserverIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error)
	IsPostgresqlIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error)
	IsOpensearchIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error)
	CombineOutputFiles(hostIp string, inputOutputFiles, resultOutputFiles []string) ([]string, error)
	PrintErrorMessage(remoteService string, hostIP string, cliWriter *cli.Writer, err string)
	PrintSuccessMessage(remoteService string, hostIP string, cliWriter *cli.Writer, output string)
	CheckOutputForError(output string) error
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

	sshConfig := c.GetSshDetails(c.NodeMap.Infra)

	c.SshUtil.setSSHConfig(sshConfig)

	switch true {
	case c.NodeMap.Frontend.CmdInputs.NodeType:
		const remoteService string = "frontend"

		if (c.NodeMap.Frontend.PreExec) != nil {
			err = c.NodeMap.Frontend.PreExec(c.NodeMap.Frontend.CmdInputs, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
			if err != nil {
				return err
			}
		}

		nodeIps, err := c.IsFrontendIPs(c.NodeMap.Frontend.CmdInputs.NodeIp, c.NodeMap.Infra)
		if err != nil {
			writer.Error(err.Error())
			os.Exit(1)
		}
		if len(nodeIps) == 0 {
			writer.Error("No frontend IPs are found")
			os.Exit(1)
		}

		err = c.CmdExecOnNodes(c.NodeMap.Frontend.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Automate.CmdInputs.NodeType:
		const remoteService string = "automate"

		if (c.NodeMap.Automate.PreExec) != nil {
			err = c.NodeMap.Automate.PreExec(c.NodeMap.Automate.CmdInputs, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
			if err != nil {
				return err
			}
		}

		nodeIps, err := c.IsAutomateIPs(c.NodeMap.Automate.CmdInputs.Single, c.NodeMap.Automate.CmdInputs.NodeIp, c.NodeMap.Infra)
		if err != nil {
			writer.Error(err.Error())
			os.Exit(1)
		}
		if len(nodeIps) == 0 {
			writer.Error("No automate IPs are found")
			os.Exit(1)
		}

		err = c.CmdExecOnNodes(c.NodeMap.Automate.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Chef_server.CmdInputs.NodeType:
		const remoteService string = "chef_server"

		if (c.NodeMap.Chef_server.PreExec) != nil {
			err = c.NodeMap.Chef_server.PreExec(c.NodeMap.Chef_server.CmdInputs, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
			if err != nil {
				return err
			}
		}

		nodeIps, err := c.IsChefserverIPs(c.NodeMap.Chef_server.CmdInputs.Single, c.NodeMap.Chef_server.CmdInputs.NodeIp, c.NodeMap.Infra)
		if err != nil {
			writer.Error(err.Error())
			os.Exit(1)
		}
		if len(nodeIps) == 0 {
			writer.Error("No chef-server IPs are found")
			os.Exit(1)
		}

		err = c.CmdExecOnNodes(c.NodeMap.Chef_server.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Postgresql.CmdInputs.NodeType:
		const remoteService string = "postgresql"
		if (c.NodeMap.Postgresql.PreExec) != nil {
			err = c.NodeMap.Postgresql.PreExec(c.NodeMap.Postgresql.CmdInputs, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
			if err != nil {
				return err
			}
		}

		nodeIps, err := c.IsPostgresqlIPs(c.NodeMap.Postgresql.CmdInputs.Single, c.NodeMap.Postgresql.CmdInputs.NodeIp, c.NodeMap.Infra)
		if err != nil {
			writer.Error(err.Error())
			os.Exit(1)
		}
		if len(nodeIps) == 0 {
			writer.Error("No postgresql IPs are found")
			os.Exit(1)
		}

		err = c.CmdExecOnNodes(c.NodeMap.Postgresql.CmdInputs, nodeIps, remoteService, timestamp, writer)
	case c.NodeMap.Opensearch.CmdInputs.NodeType:
		const remoteService string = "opensearch"

		if (c.NodeMap.Opensearch.PreExec) != nil {
			err = c.NodeMap.Opensearch.PreExec(c.NodeMap.Opensearch.CmdInputs, c.SshUtil, c.NodeMap.Infra, remoteService, timestamp, writer)
			if err != nil {
				return err
			}
		}

		nodeIps, err := c.IsOpensearchIPs(c.NodeMap.Opensearch.CmdInputs.Single, c.NodeMap.Opensearch.CmdInputs.NodeIp, c.NodeMap.Infra)
		if err != nil {
			writer.Error(err.Error())
			os.Exit(1)
		}
		if len(nodeIps) == 0 {
			writer.Error("No opensearch IPs are found")
			os.Exit(1)
		}

		err = c.CmdExecOnNodes(c.NodeMap.Opensearch.CmdInputs, nodeIps, remoteService, timestamp, writer)
	default:
		return errors.New(`Missing or Unsupported flag`)
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

		if result.Error != nil {
			c.PrintErrorMessage(remoteService, result.HostIP, cliWriter, result.Error.Error())
		} else {
			c.PrintSuccessMessage(remoteService, result.HostIP, cliWriter, result.Output)
		}
		if len(result.OutputFiles) != 0 {
			resultFiles, err := c.CombineOutputFiles(result.HostIP, outputFiles, result.OutputFiles)
			if err != nil {
				cliWriter.Println("Error while combining the output files")
			} else {
				for _, file := range resultFiles {
					cliWriter.Printf("Output file: %s", file+"\n")
				}
			}
		}

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
		err = c.CheckOutputForError(output)
		if err != nil {
			rc.Error = err
			resultChan <- rc
			return
		}
	}

	rc.Output = output
	resultChan <- rc
}

// GetSshDetails will return the SSH details.
func (c *CmdUtilImpl) GetSshDetails(infra *AutomteHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// IsValidIP will check whether the given ip is in the given remoteservice ips set or not.
func (c *CmdUtilImpl) IsValidIP(ip string, ips []string) bool {
	for _, clusterIP := range ips {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

// IsFrontendIPs will return the Frontend Node Ips.
func (c *CmdUtilImpl) IsFrontendIPs(ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	frontendNodes := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	if ip != "" {
		if c.IsValidIP(ip, frontendNodes) {
			return []string{ip}, nil
		} else {
			return []string{}, errors.New("Please Enter Valid frontend IP")
		}
	} else {
		return frontendNodes, nil
	}
}

// IsAutomateIPs will return the Automate Node Ips.
func (c *CmdUtilImpl) IsAutomateIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if c.IsValidIP(ip, infra.Outputs.AutomatePrivateIps.Value) {
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
func (c *CmdUtilImpl) IsChefserverIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if c.IsValidIP(ip, infra.Outputs.ChefServerPrivateIps.Value) {
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
func (c *CmdUtilImpl) IsPostgresqlIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if c.IsValidIP(ip, infra.Outputs.PostgresqlPrivateIps.Value) {
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
func (c *CmdUtilImpl) IsOpensearchIPs(single bool, ip string, infra *AutomteHAInfraDetails) ([]string, error) {
	if ip != "" {
		if c.IsValidIP(ip, infra.Outputs.OpensearchPrivateIps.Value) {
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

// CombineOutputFiles combines outfile from all node in one file.
func (c *CmdUtilImpl) CombineOutputFiles(hostIp string, inputOutputFiles, resultOutputFiles []string) ([]string, error) {
	var resultFile []string

	for _, file := range inputOutputFiles {
		for _, outfile := range resultOutputFiles {
			if strings.Contains(outfile, file) {
				f, err := os.OpenFile(file, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
				if err != nil {
					return resultFile, err
				}
				defer f.Close()

				output, err := ioutil.ReadFile(outfile) // nosemgrep
				if err != nil {
					return resultFile, err
				}
				fileContent := string(output)

				start := fmt.Sprintf("Output for Host IP %s : \n%s\n", hostIp, fileContent)

				_, err = f.WriteString(start)

				if err != nil {
					return resultFile, err
				}

				resultFile = append(resultFile, file)
			}
		}
	}
	return resultFile, nil
}

// printErrorMessage prints the config error message
func (c *CmdUtilImpl) PrintErrorMessage(remoteService string, hostIP string, cliWriter *cli.Writer, err string) {
	cliWriter.Fail("Command failed on " + remoteService + " node : " + hostIP + " with error:\n" + err + "\n")
	cliWriter.BufferWriter().Flush()
}

// printSuccessMessage prints the config success message
func (c *CmdUtilImpl) PrintSuccessMessage(remoteService string, hostIP string, cliWriter *cli.Writer, output string) {
	cliWriter.Printf("Output for Host IP %s : \n%s", hostIP, output+"\n")
	cliWriter.Success("Command is executed on " + remoteService + " node : " + hostIP + "\n")
	cliWriter.BufferWriter().Flush()
}

// If the output contains the word "error" then return error
func (c *CmdUtilImpl) CheckOutputForError(output string) error {
	if strings.Contains(strings.ToUpper(strings.TrimSpace(output)), "ERROR") {
		return errors.New(output)
	}
	return nil
}
