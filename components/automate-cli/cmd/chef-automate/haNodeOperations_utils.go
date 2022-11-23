package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

type HAModifyAndDeploy interface {
	validate() error
	modifyConfig() error
	promptUserConfirmation() (bool, error)
	runDeploy() error
}

type MockNodeUtilsImpl struct {
	readConfigfunc                            func(path string) (ExistingInfraConfigToml, error)
	writeFilefunc                             func(tomlbytes []byte, filepath string) error
	executeAutomateClusterCtlCommandAsyncfunc func(command string, args []string, helpDocs string) error
	getHaInfraDetailsfunc                     func() (string, string, string, error)
	connectAndExecuteCommandOnRemotefunc      func(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error)
	genConfigfunc                             func(path string) error
}

func (mnu *MockNodeUtilsImpl) readConfig(path string) (ExistingInfraConfigToml, error) {
	return mnu.readConfigfunc(path)
}
func (mnu *MockNodeUtilsImpl) writeFile(tomlbytes []byte, filepath string) error {
	return mnu.writeFilefunc(tomlbytes, filepath)
}
func (mnu *MockNodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return mnu.executeAutomateClusterCtlCommandAsyncfunc(command, args, helpDocs)
}
func (mnu *MockNodeUtilsImpl) connectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
	return mnu.connectAndExecuteCommandOnRemotefunc(sshUser, sshPort, sshKeyFile, hostIP, remoteCommands)
}
func (mnu *MockNodeUtilsImpl) getHaInfraDetails() (string, string, string, error) {
	return mnu.getHaInfraDetailsfunc()
}
func (mnu *MockNodeUtilsImpl) genConfig(path string) error {
	return mnu.genConfigfunc(path)
}

type NodeUtils interface {
	readConfig(path string) (ExistingInfraConfigToml, error)
	writeFile(tomlbytes []byte, filepath string) error
	executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error
	connectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error)
	getHaInfraDetails() (string, string, string, error)
	genConfig(path string) error
}

type NodeUtilsImpl struct{}

func NewNodeUtils() NodeUtils {
	return &NodeUtilsImpl{}
}

func (nu *NodeUtilsImpl) readConfig(path string) (ExistingInfraConfigToml, error) {
	return readConfig(path)
}
func (nu *NodeUtilsImpl) writeFile(tomlbytes []byte, filepath string) error {
	return writeFile(tomlbytes, filepath)
}
func (nu *NodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return executeAutomateClusterCtlCommandAsync(command, args, helpDocs)
}
func (nu *NodeUtilsImpl) connectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (string, error) {
	sshconfig := &SSHConfig{
		sshUser:    sshUser,
		sshPort:    sshPort,
		sshKeyFile: sshKeyFile,
		hostIP:     hostIP,
	}
	sshUtil := NewSSHUtil(sshconfig)
	return sshUtil.connectAndExecuteCommandOnRemote(remoteCommands, true)
}

func (nu *NodeUtilsImpl) genConfig(path string) error {
	e := newExistingInfa(path)
	return e.generateConfig()
}

func (nu *NodeUtilsImpl) getHaInfraDetails() (string, string, string, error) {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", "", "", err
	}
	sshUser := infra.Outputs.SSHUser.Value
	sskKeyFile := infra.Outputs.SSHKeyFile.Value
	sshPort := infra.Outputs.SSHPort.Value
	return sshUser, sskKeyFile, sshPort, nil
}

func writeFile(tomlbytes []byte, filepath string) error {
	err := ioutil.WriteFile(filepath, tomlbytes, 0600) // nosemgrep
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}
	return nil
}

func trimSliceSpace(slc []string) []string {
	for i := range slc {
		slc[i] = strings.TrimSpace(slc[i])
	}
	return slc
}

func modifyConfigForAddNewNode(instanceCount *string, existingPrivateIPs *[]string, newIps []string, certsIp *[]CertByIP) error {
	if len(newIps) == 0 {
		return nil
	}
	*existingPrivateIPs = append(*existingPrivateIPs, newIps...)
	inc, err := modifyInstanceCount(*instanceCount, len(newIps))
	*instanceCount = inc
	if err != nil {
		return err
	}
	for _, ip := range newIps {
		c := CertByIP{
			IP:         ip,
			PrivateKey: (*certsIp)[len(*certsIp)-1].PrivateKey,
			PublicKey:  (*certsIp)[len(*certsIp)-1].PublicKey,
		}
		*certsIp = append(*certsIp, c)
	}
	return nil
}

func modifyConfigForDeleteNode(instanceCount *string, existingPrivateIPs *[]string, newIps []string, certsIp *[]CertByIP) error {
	if len(newIps) == 0 {
		return nil
	}
	*existingPrivateIPs = difference(*existingPrivateIPs, newIps)
	inc, err := modifyInstanceCount(*instanceCount, -len(newIps))
	*instanceCount = inc
	if err != nil {
		return err
	}
	if len(*certsIp) > 0 {
		for _, ip := range newIps {
			*certsIp = findAndDelete(*certsIp, ip)
		}
	}
	return nil
}

func difference(a, b []string) []string {
	mb := make(map[string]struct{}, len(b))
	for _, x := range b {
		mb[x] = struct{}{}
	}
	var diff []string
	for _, x := range a {
		if _, found := mb[x]; !found {
			diff = append(diff, x)
		}
	}
	return diff
}

func findAndDelete(s []CertByIP, item string) []CertByIP {
	index := 0
	for _, i := range s {
		if i.IP != item {
			s[index] = i
			index++
		}
	}
	return s[:index]
}

func modifyInstanceCount(instanceCount string, additive int) (string, error) {
	i, err := strconv.Atoi(instanceCount)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%v", i+additive), nil
}

func splitIPCSV(automateIp, chefserverIp, opensearchIp, postgresIp string) (automateIpList, chefServerIpList, opensearchIpList, postgresqlIp []string) {
	if automateIp != "" {
		automateIpList = strings.Split(automateIp, ",")
		automateIpList = trimSliceSpace(automateIpList)
	}
	if chefserverIp != "" {
		chefServerIpList = strings.Split(chefserverIp, ",")
		chefServerIpList = trimSliceSpace(chefServerIpList)
	}
	if opensearchIp != "" {
		opensearchIpList = strings.Split(opensearchIp, ",")
		opensearchIpList = trimSliceSpace(opensearchIpList)
	}
	if postgresIp != "" {
		postgresqlIp = strings.Split(postgresIp, ",")
		postgresqlIp = trimSliceSpace(postgresqlIp)
	}
	return
}
