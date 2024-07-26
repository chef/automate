package main

import (
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	chefToml "github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func isStringPresent(arr []string, str string) bool {
	for _, s := range arr {
		if s == str {
			return true
		}
	}
	return false
}

// Defining set of commands which run on particular remoteservice nodes
func getScriptCommands(param *patchFnParameters, scriptCommands string) string {
	if param.remoteService == AUTOMATE || param.remoteService == CHEF_SERVER || param.remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMAND, PATCH, param.remoteService+param.timestamp, DATE_FORMAT)
	} else if param.remoteService == POSTGRESQL || param.remoteService == OPENSEARCH {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG, param.remoteService+param.timestamp, param.remoteService)
	}
	return scriptCommands
}

func getScriptCommandsNoRestart(param *patchFnParameters, scriptCommands string) string {
	if param.remoteService == AUTOMATE || param.remoteService == CHEF_SERVER || param.remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMAND, PATCH, param.remoteService+param.timestamp, DATE_FORMAT)
	} else if param.remoteService == POSTGRESQL || param.remoteService == OPENSEARCH {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG_NO_RESTART, param.remoteService+param.timestamp, param.remoteService)
	}
	return scriptCommands
}

func printCertRotateOutput(cmdResult sshutils.Result, remoteService string, writer *cli.Writer) {
	writer.Printf("\n=======================================================================\n")

	if cmdResult.Error != nil || strings.Contains(cmdResult.Output, "DeploymentServiceCallError") {
		printCertRotateErrorOutput(cmdResult, remoteService, writer)
	} else {
		writer.Printf("Output for Host IP %s : \n%s", cmdResult.HostIP, cmdResult.Output+"\n")
		writer.Success("Command is executed on node : " + cmdResult.HostIP + "\n")
	}
	writer.BufferWriter().Flush()
}

func printCertRotateErrorOutput(cmdResult sshutils.Result, remoteService string, writer *cli.Writer) {
	isOutputError := false
	if strings.Contains(cmdResult.Output, "DeploymentServiceCallError") {
		isOutputError = true
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.HostIP, cmdResult.Output)
	}
	if !isOutputError {
		writer.Failf(CMD_FAILED_MSG, remoteService, cmdResult.HostIP, cmdResult.Error.Error())
	}
}

func uniqueIps(ips []string) []string {
	var uniqueIps []string
	mp := make(map[string]bool)
	for _, ip := range ips {
		mp[ip] = true
	}
	for ip := range mp {
		uniqueIps = append(uniqueIps, ip)
	}
	return uniqueIps
}

func getStatusSummary(infra *AutomateHAInfraDetails) (StatusSummary, error) {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	sshUtil := NewSSHUtil(sshConfig)
	sshConfig.timeout = 30
	sshUtil.setSSHConfig(sshConfig)
	var statusSummaryCmdFlags = StatusSummaryCmdFlags{
		isPostgresql: true,
	}
	remoteCmdExecutor := NewRemoteCmdExecutorWithoutNodeMap(sshUtil, writer)
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &statusSummaryCmdFlags, remoteCmdExecutor)
	err := statusSummary.Prepare()
	if err != nil {
		return nil, err
	}
	return statusSummary, nil
}

func getMaxPGLag(log logger.Logger, statusSummary StatusSummary) (int64, error) {
	lag := statusSummary.GetPGMaxLagAmongFollowers()
	log.Debug("==========================================================")
	log.Debug("Total lag in PostgreSQL follower node is %d \n", lag)
	log.Debug("==========================================================")
	return lag, nil
}

func frontendMaintainenceModeOnOFF(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, onOFFSwitch string, hostIps []string, log logger.Logger, writer *cli.Writer, totalWaitTimeOut time.Duration) {
	sshConfig.Timeout = int(totalWaitTimeOut.Seconds())
	if onOFFSwitch == ON {
		writer.Println("turning ON maintenance mode")
	}
	if onOFFSwitch == OFF {
		writer.Println("turning OFF maintenance mode")
	}
	command := fmt.Sprintf(MAINTENANCE_ON_OFF, onOFFSwitch)
	log.Debug("========================== MAINTENANCE_ON_OFF ========================")
	log.Debug(command)
	log.Debug("========================== MAINTENANCE_ON_OFF ========================")
	excuteResults := sshUtil.ExecuteConcurrently(sshConfig, command, hostIps)
	for _, result := range excuteResults {
		printCertRotateOutput(result, "frontend", writer)
	}
}

func startTrafficOnAutomateNode(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, log logger.Logger, writer *cli.Writer, totalWaitTimeOut time.Duration) {
	hostIps := infra.Outputs.AutomatePrivateIps.Value
	frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtil, OFF, hostIps, log, writer, totalWaitTimeOut)
}

func startTrafficOnChefServerNode(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, log logger.Logger, writer *cli.Writer, totalWaitTimeOut time.Duration) {
	hostIps := infra.Outputs.ChefServerPrivateIps.Value
	frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtil, OFF, hostIps, log, writer, totalWaitTimeOut)
}

func checkLagAndStopTraffic(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtils sshutils.SSHUtil, log logger.Logger, statusSummary StatusSummary, userConsent bool, waitTime time.Duration, totalWaitTimeOut time.Duration, writer *cli.Writer) error {
	fontendIps := infra.Outputs.AutomatePrivateIps.Value
	fontendIps = append(fontendIps, infra.Outputs.ChefServerPrivateIps.Value...)
	lag, err := getMaxPGLag(log, statusSummary)
	if err != nil {
		return err
	}

	if userConsent {
		agree, err := writer.Confirm(fmt.Sprintf(MAINTENANICE_ON_LAG, lag))
		if err != nil {
			return status.Wrap(err, status.InvalidCommandArgsError, errMLSA)
		}
		if !agree {
			return status.New(status.InvalidCommandArgsError, errMLSA)
		}
	}
	frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtils, ON, fontendIps, log, writer, totalWaitTimeOut)

	go func(automateStarted chan bool) {
		isAutomateStarted := <-automateStarted
		if isAutomateStarted {
			startTrafficOnAutomateNode(infra, sshConfig, sshUtils, log, writer, totalWaitTimeOut)
		}
	}(automateStartedChan)

	waitingStart := time.Now()
	time.Sleep(waitTime * time.Second)
	for {
		lag, err := getMaxPGLag(log, statusSummary)
		if err != nil {
			return err
		}
		if lag == 0 {
			break
		} else {
			timeElapsed := time.Since(waitingStart)
			if timeElapsed.Seconds() >= totalWaitTimeOut.Seconds() {
				return status.Wrap(errors.New(""), status.UnhealthyStatusError, fmt.Sprintf("Follower node is still behind the leader by %d bytes\n", lag))
			}
		}
		writer.Println("retrying again...")
		time.Sleep(10 * time.Second)
	}
	return nil
}

func getCertsFromTemplate(clusterCertificateFile string) (*CertificateToml, error) {
	if len(clusterCertificateFile) < 1 {
		return nil, errors.New("Cluster certificate file is required")
	}
	writer.Println("Reading certificates from template file")
	content, err := fileutils.ReadFile(clusterCertificateFile)
	if err != nil {
		writer.Errorln("Error in fetching certificates from template file")
		return nil, err
	}
	certifiacates := &CertificateToml{}
	toml.Decode(string(content), certifiacates)
	return certifiacates, nil
}

func generateCertificateConfig() func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		inf, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		err, certTemplate := populateCertificateConfig(inf)
		if err != nil {
			return err
		}
		return writeCertificateConfigToFile(inf, args, certTemplate, &fileutils.FileSystemUtils{})
	}
}

func writeCertificateConfigToFile(infra *AutomateHAInfraDetails, args []string, certTemplate *CertificateToml, fUtils fileutils.FileUtils) error {
	if len(args) < 1 {
		return errors.Errorf("command need a output file name like cert-config.toml")
	}
	certFileName := args[0]
	config, err := chefToml.Marshal(certTemplate)
	if err != nil {
		return err
	}
	writer.Printf("certificate config file is generated %s, Please update the file with releavent certificate file paths \n", certFileName)
	return fUtils.WriteFile(certFileName, config, 0600)
}

func populateCertificateConfig(infra *AutomateHAInfraDetails) (error, *CertificateToml) {
	certifiacates := &CertificateToml{
		Automate: NodeCertficate{
			IPS: getIPS(infra, AUTOMATE),
		},
		ChefServer: NodeCertficate{
			IPS: getIPS(infra, CHEF_SERVER),
		},
		PostgreSQL: NodeCertficate{
			IPS: getIPS(infra, POSTGRESQL),
		},
		OpenSearch: NodeCertficate{
			AdminPublickey:  "!Replace this with <admin public key>",
			AdminPrivateKey: "!Replace this with <admin private key>",
			IPS:             getIPS(infra, OPENSEARCH),
		},
	}
	return nil, certifiacates
}

func getIPS(infra *AutomateHAInfraDetails, nodeType string) []IP {
	var ips = []IP{}
	if strings.EqualFold(nodeType, AUTOMATE) {
		for _, nodeIp := range infra.Outputs.AutomatePrivateIps.Value {
			ips = append(ips, IP{
				IP: nodeIp,
			})
		}
	} else if strings.EqualFold(nodeType, CHEF_SERVER) {
		for _, nodeIp := range infra.Outputs.ChefServerPrivateIps.Value {
			ips = append(ips, IP{
				IP: nodeIp,
			})
		}
	} else if strings.EqualFold(nodeType, POSTGRESQL) {
		for _, nodeIp := range infra.Outputs.PostgresqlPrivateIps.Value {
			ips = append(ips, IP{
				IP: nodeIp,
			})
		}
	} else if strings.EqualFold(nodeType, OPENSEARCH) {
		for _, nodeIp := range infra.Outputs.OpensearchPrivateIps.Value {
			ips = append(ips, IP{
				IP: nodeIp,
			})
		}
	}
	return ips
}

func patchOSNodeDN(flagsObj *certRotateFlags, patchFnParam *patchFnParameters, c *certRotateFlow, nodesDn string) error {

	peerConfig := fmt.Sprintf(OPENSEARCH_DN_CONFIG_FOR_PEERS, fmt.Sprintf("%v", nodesDn))

	nodeVal := flagsObj.node
	flagsObj.node = ""

	patchFnParam.fileName = "cert-rotate-os-peer.toml"
	patchFnParam.config = peerConfig
	patchFnParam.timestamp = time.Now().Format("20060102150405")
	patchFnParam.skipIpsList = []string{flagsObj.node}
	patchFnParam.concurrent = false
	err := c.patchConfig(patchFnParam, true)
	if err != nil {
		return err
	}

	flagsObj.node = nodeVal
	return nil
}
