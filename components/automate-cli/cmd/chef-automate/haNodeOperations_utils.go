package main

import (
	"fmt"
	"strconv"
	"strings"
)

type HAModifyAndDeploy interface {
	validate() error
	modifyConfig() error
	promptUserConfirmation() (bool, error)
	runDeploy() error
}

type MockNodeUtilsImpl struct {
	readConfigfunc                            func(path string) (ExistingInfraConfigToml, error)
	executeAutomateClusterCtlCommandAsyncfunc func(command string, args []string, helpDocs string) error
	getHaInfraDetailsfunc                     func() (*SSHConfig, error)
	genConfigfunc                             func(path string) error
}

func (mnu *MockNodeUtilsImpl) readConfig(path string) (ExistingInfraConfigToml, error) {
	return mnu.readConfigfunc(path)
}
func (mnu *MockNodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return mnu.executeAutomateClusterCtlCommandAsyncfunc(command, args, helpDocs)
}
func (mnu *MockNodeUtilsImpl) getHaInfraDetails() (*SSHConfig, error) {
	return mnu.getHaInfraDetailsfunc()
}
func (mnu *MockNodeUtilsImpl) genConfig(path string) error {
	return mnu.genConfigfunc(path)
}

type NodeOpUtils interface {
	readConfig(path string) (ExistingInfraConfigToml, error)
	executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error
	getHaInfraDetails() (*SSHConfig, error)
	genConfig(path string) error
}

type NodeUtilsImpl struct{}

func NewNodeUtils() NodeOpUtils {
	return &NodeUtilsImpl{}
}

func (nu *NodeUtilsImpl) readConfig(path string) (ExistingInfraConfigToml, error) {
	return readConfig(path)
}
func (nu *NodeUtilsImpl) executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	return executeAutomateClusterCtlCommandAsync(command, args, helpDocs)
}

func (nu *NodeUtilsImpl) genConfig(path string) error {
	e := newExistingInfa(path)
	return e.generateConfig()
}

func (nu *NodeUtilsImpl) getHaInfraDetails() (*SSHConfig, error) {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return &SSHConfig{}, err
	}
	sshconfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshconfig, nil
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
