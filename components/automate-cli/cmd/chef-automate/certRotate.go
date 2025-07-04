package main

import (
	"bytes"
	"encoding/pem"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	FRONTEND_CONFIG = `
	[[load_balancer.v1.sys.frontend_tls]]
		cert = """%v"""
		key = """%v"""
	[[global.v1.frontend_tls]]
		cert = """%v"""
		key = """%v"""`

	CS_EXTERNAL_AUTOMATE_CERT_CONFIG = `
    [global.v1.external.automate.ssl]
      root_cert = """%v"""`

	POSTGRES_CONFIG = `
	[ssl]
		enable = true
		ssl_key = """%v"""
		ssl_cert = """%v"""
		issuer_cert = """%v"""`

	POSTGRES_CONFIG_IGNORE_ISSUER_CERT = `
	[ssl]
		enable = true
		ssl_key = """%v"""
		ssl_cert = """%v"""`

	POSTGRES_FRONTEND_CONFIG = `
	[global.v1.external.postgresql.ssl]
		enable = true
		root_cert = """%v"""`

	OPENSEARCH_CONFIG = `
	[tls]
		rootCA = """%v"""
		admin_cert = """%v"""
		admin_key = """%v"""
		ssl_cert = """%v"""
		ssl_key = """%v"""
	[plugins.security.authcz]
		admin_dn = "- %v"
	[plugins.security.ssl.transport]
		enforce_hostname_verification = false
		resolve_hostname = false
	[plugins.security]
		nodes_dn = """- %v"""`

	OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA = `
	[tls]
		ssl_cert = """%v"""
		ssl_key = """%v"""
	[plugins.security]
		nodes_dn = """- %v"""`

	OPENSEARCH_FRONTEND_CONFIG = `
	[global.v1.external.opensearch.ssl]
		root_cert = """%v"""
		server_name = "%v"`

	OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT = `
	[global.v1.external.opensearch.ssl]
		server_name = "%v"`

	OPENSEARCH_DN_CONFIG_FOR_PEERS = `
	[plugins]
	[plugins.security]
		nodes_dn = """- %v"""`

	GET_USER_CONFIG = `
	sudo cat /hab/user/automate-ha-%s/config/user.toml`

	COPY_USER_CONFIG = `
	sudo systemctl stop hab-sup.service
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml
	sudo systemctl start hab-sup.service`

	COPY_USER_CONFIG_NO_RESTART = `
	echo "y" | sudo cp /tmp/%s /hab/user/automate-ha-%s/config/user.toml
	`

	IP_V4_REGEX = `(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}`

	ERROR_SELF_MANAGED_DB_CERT_ROTATE = "Certificate rotation for externally configured %s is not supported."
	SKIP_IPS_MSG_CERT_ROTATE          = "The following %s %s will skip during certificate rotation as the following %s have the same certificates as currently provided certificates.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_A2         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Automate root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_PG         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Postgresql root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_OS         = "The following %s %s will skip during root-ca and common name patching as the following %s have same root-ca and common name as currently provided OpenSearch root-ca and common name.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_CN         = "The following %s %s will skip during common name patching as the following %s have same common name as currently provided OpenSearch common name.\n\t %s"
	DEFAULT_TIMEOUT                   = 600
	ERROR_REQUIRED_FLAG_CC            = `Flag certificate-config is mandatory for certificate rotation. Make sure to pass the certificate template with this flag`
	MAINTENANICE_ON_LAG               = `
Follower nodes are behind leader node by %d bytes, to avoid data loss we will put cluster on maintenance mode, do you want to continue :
`
)

type certificates struct {
	privateCert string
	publicCert  string
	rootCA      string
	adminCert   string
	adminKey    string
}

type certRotateFlags struct {
	//SSH
	automate   bool
	chefserver bool
	postgres   bool
	opensearch bool

	// // Certificates Path
	privateCertPath string
	publicCertPath  string
	rootCAPath      string
	adminCertPath   string
	adminKeyPath    string
	cluster         string

	// Node
	node    string
	timeout int
}

type certRotateFlow struct {
	fileUtils   fileutils.FileUtils
	sshUtil     sshutils.SSHUtil
	writer      *cli.Writer
	pullConfigs PullConfigs
	log         logger.Logger
}

type NodeIpHealth struct {
	IP     string
	Health string
}

func NewCertRotateFlow(fileUtils fileutils.FileUtils, sshUtil sshutils.SSHUtil, writer *cli.Writer, pullConfigs PullConfigs, log logger.Logger) *certRotateFlow {
	return &certRotateFlow{
		fileUtils:   fileUtils,
		sshUtil:     sshUtil,
		writer:      writer,
		pullConfigs: pullConfigs,
		log:         log,
	}
}

type patchFnParameters struct {
	sshUtil       SSHUtil
	config        string
	fileName      string
	timestamp     string
	remoteService string
	concurrent    bool
	infra         *AutomateHAInfraDetails
	flagsObj      *certRotateFlags
	skipIpsList   []string
}

func init() {
	flagsObj := certRotateFlags{}

	certRotateCmd := &cobra.Command{
		Use:   "cert-rotate",
		Short: "Chef Automate rotate cert",
		Long:  "Chef Automate CLI command to rotate certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certRotateCmdFunc(&flagsObj),
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithHA,
		},
	}

	certTemplateGenerateCmd := &cobra.Command{
		Use:   "generate-certificate-config",
		Short: "Chef Automate generate certificate config ",
		Long:  "Chef Automate CLI command to generate certificates config, this command should always be executed from AutomateHA Bastion Node",
		RunE:  generateCertificateConfig(),
		Annotations: map[string]string{
			docs.Compatibility: docs.CompatiblewithHA,
		},
	}

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.cluster, "certificate-config", "", "Cluster certificate file")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.cluster, "cc", "", "Cluster certificate file")

	certRotateCmd.PersistentFlags().IntVar(&flagsObj.timeout, "wait-timeout", DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process")

	RootCmd.AddCommand(certRotateCmd)
	certRotateCmd.AddCommand(certTemplateGenerateCmd)
	markGlobalFlagsHiddenExcept(certRotateCmd, "certificate-config", "cc")
}

func certRotateCmdFunc(flagsObj *certRotateFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		level := "info"
		if globalOpts.debug {
			level = "debug"
		}
		log, err := logger.NewLogger("text", level)
		if err != nil {
			return err
		}
		c := NewCertRotateFlow(&fileutils.FileSystemUtils{}, sshutils.NewSSHUtilWithCommandExecutor(sshutils.NewSshClient(), log, command.NewExecExecutor()), writer, NewPullConfigs(&AutomateHAInfraDetails{}, &SSHUtilImpl{}), log)
		return c.certRotate(cmd, args, flagsObj)
	}
}

// certRotate will rotate the certificates of Automate, Chef Infra Server, Postgres and Opensearch.
func (c *certRotateFlow) certRotate(cmd *cobra.Command, args []string, flagsObj *certRotateFlags) error {
	if isA2HARBFileExist() {
		if len(flagsObj.cluster) > 0 {
			infra, err := getAutomateHAInfraDetails()
			if err != nil {
				return err
			}
			sshConfig := c.getSshDetails(infra)
			sshUtil := NewSSHUtil(sshConfig)
			sshConfig.timeout = flagsObj.timeout
			sshUtil.setSSHConfig(sshConfig)
			certShowFlow := NewCertShowImpl(certShowFlags{}, NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer), sshUtil, writer)
			currentCertsInfo, err := certShowFlow.fetchCurrentCerts()
			if err != nil {
				return errors.WithStack(err)
			}
			infra, err = getAutomateHAInfraDetails()
			if err != nil {
				writer.Errorf("error in getting infra details of HA, %s\n", err.Error())
				return err
			}
			statusSummary, err := getStatusSummary(infra)
			if err != nil {
				return err
			}
			err = c.certRotateFromTemplate(flagsObj.cluster, sshUtil, infra, currentCertsInfo, statusSummary, true, 10, flagsObj)
			if err != nil {
				return err
			}
		} else {
			return status.Errorf(status.InvalidCommandArgsError, ERROR_REQUIRED_FLAG_CC)
		}
	} else {
		return fmt.Errorf("cert-rotate command should be executed from Automate HA Bastion Node")
	}

	return nil
}

func (c *certRotateFlow) getSkipIpsListForPgRootCAPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates) ([]string, error) {

	c.pullConfigs.setInfraAndSSHUtil(infra, sshUtil)
	//fetching current config from automate
	automateCurrentConfig, _, err := c.pullConfigs.pullAutomateConfigs(false)
	if err != nil {
		return nil, err
	}

	//get frontend ips to skip root-ca patch
	skipIpsList := c.getFrontendIPsToSkipRootCAPatchingForPg(automateCurrentConfig, certs.rootCA, infra)

	//fetching current config from chefServer
	chefServerCurrentConfig, _, err := c.pullConfigs.pullChefServerConfigs(false)

	if err != nil {
		return nil, err
	}
	newskipIpsList := c.getFrontendIPsToSkipRootCAPatchingForPg(chefServerCurrentConfig, certs.rootCA, infra)
	skipIpsList = append(skipIpsList, newskipIpsList...)

	return skipIpsList, nil
}

func (c *certRotateFlow) getSkipIpsListForOsRootCACNPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates, nodesCn string, flagsObj *certRotateFlags) ([]string, error) {

	c.pullConfigs.setInfraAndSSHUtil(infra, sshUtil)
	//fetching current config from automate
	automateCurrentConfig, _, err := c.pullConfigs.pullAutomateConfigs(false)
	if err != nil {
		return nil, err
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodesCn

	skipIpsList := c.getFrontIpsToSkipRootCAandCNPatchingForOs(automateCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)

	//fetching current config from chefServer
	chefServerCurrentConfig, _, err := c.pullConfigs.pullChefServerConfigs(false)

	if err != nil {
		return nil, err
	}
	newskipIpsList := c.getFrontIpsToSkipRootCAandCNPatchingForOs(chefServerCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)
	skipIpsList = append(skipIpsList, newskipIpsList...)

	return skipIpsList, nil
}

func (c *certRotateFlow) getFrontIpsToSkipRootCAandCNPatchingForOs(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, newCn string, node string, infra *AutomateHAInfraDetails) []string {
	oldRootCA := ""
	oldCn := ""
	skipIpsList := []string{}
	for ip, config := range automatesConfig {
		if config.Global.V1.External.Opensearch != nil && config.Global.V1.External.Opensearch.Ssl != nil {
			if config.Global.V1.External.Opensearch.Ssl.ServerName != nil {
				oldCn = config.Global.V1.External.Opensearch.Ssl.ServerName.Value
				if c.getFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node) {
					if !isStringPresent(skipIpsList, ip) {
						// If not present, append the string to the array
						skipIpsList = append(skipIpsList, ip)
					}
				}
			}
			if config.Global.V1.External.Opensearch.Ssl.RootCert != nil {
				oldRootCA = config.Global.V1.External.Opensearch.Ssl.RootCert.Value
				if c.getFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node) {
					if !isStringPresent(skipIpsList, ip) {
						// If not present, append the string to the array
						skipIpsList = append(skipIpsList, ip)
					}
				}
			}
		}
	}
	return skipIpsList
}

func (c *certRotateFlow) getFrontendIPsToSkipRootCAPatchingForPg(automatesConfig map[string]*deployment.AutomateConfig, newRootCA string, infra *AutomateHAInfraDetails) []string {
	oldRootCA := ""
	skipIpsList := []string{}
	for ip, config := range automatesConfig {
		if config.Global.V1.External.Postgresql.Ssl.RootCert != nil {
			oldRootCA = config.Global.V1.External.Postgresql.Ssl.RootCert.Value
			if oldRootCA == newRootCA {
				skipIpsList = append(skipIpsList, ip)
			}
		}
	}
	return skipIpsList
}

// patchConfig will patch the configurations to required nodes.
func (c *certRotateFlow) patchConfig(param *patchFnParameters, restartService bool) error {
	f, err := os.Create(param.fileName)
	if err != nil {
		return err
	}
	_, err = f.Write([]byte(param.config))
	if err != nil {
		return err
	}
	f.Close()
	var ips []string
	if param.flagsObj.node != "" && param.remoteService != "frontend" {
		isValid := c.validateEachIp(param.remoteService, param.infra, param.flagsObj)
		if !isValid {
			return errors.New(fmt.Sprintf("Please Enter Valid %s IP", param.remoteService))
		}
		ips = append(ips, param.flagsObj.node)
	} else {
		ips = c.getIps(param.remoteService, param.infra)
	}
	if len(ips) == 0 {
		return errors.New(fmt.Sprintf("No %s IPs are found", param.remoteService))
	}

	//collect ips on which need to perform action/cert-rotation
	filteredIps := c.getFilteredIps(ips, param.skipIpsList)
	filteredIpsString := strings.Join(filteredIps, ", ")
	sshConfig := c.getSshDetails(param.infra)
	sshConfig.hostIP = filteredIpsString
	sshConfig.timeout = param.flagsObj.timeout
	configRes := sshutils.SSHConfig{
		SshUser:    sshConfig.sshUser,
		SshPort:    sshConfig.sshPort,
		SshKeyFile: sshConfig.sshKeyFile,
		HostIP:     sshConfig.hostIP,
		Timeout:    sshConfig.timeout,
	}

	var scriptCommands string
	var command string
	if restartService {
		command = getScriptCommands(param, scriptCommands)
	} else {
		command = getScriptCommandsNoRestart(param, scriptCommands)
	}
	if !param.concurrent {
		err = c.copyAndExecute(filteredIps, param.sshUtil, param.timestamp, param.remoteService, param.fileName, command, param.flagsObj)
		if err != nil {
			return err
		}
	} else {
		err = c.copyAndExecuteConcurrentlyToFrontEndNodes(filteredIps, configRes, param.timestamp, param.remoteService, param.fileName, command, param.flagsObj)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *certRotateFlow) copyAndExecute(ips []string, sshUtil SSHUtil, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error {
	var err error
	var tomlFilePath string
	for i := 0; i < len(ips); i++ {
		sshUtil.getSSHConfig().hostIP = ips[i]
		if (flagsObj.postgres || flagsObj.opensearch) && remoteService != "frontend" {
			tomlFilePath, err = c.getMerger(fileName, timestamp, remoteService, GET_USER_CONFIG, sshUtil)
			if err != nil {
				return err
			}
			// Copying the new toml file which includes both old and new configurations (for backend nodes).
			err = sshUtil.copyFileToRemote(tomlFilePath, remoteService+timestamp, false)
		} else {
			// Copying the new toml file which includes new configurations (for frontend nodes).
			err = sshUtil.copyFileToRemote(fileName, remoteService+timestamp, false)
		}
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}

		writer.Printf("Started Applying the Configurations in %s node: %s \n", remoteService, ips[i])
		output, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Print(output + "\n")
	}
	return nil
}

// copyAndExecuteConcurrently will copy the toml file to each required nodes concurrently and then execute the set of commands.
func (c *certRotateFlow) copyAndExecuteConcurrentlyToFrontEndNodes(ips []string, sshConfig sshutils.SSHConfig, timestamp string, remoteService string, fileName string, scriptCommands string, flagsObj *certRotateFlags) error {
	ips = uniqueIps(ips)
	sshConfig.Timeout = flagsObj.timeout
	copyResults := c.sshUtil.CopyFileToRemoteConcurrently(sshConfig, fileName, remoteService+timestamp, TMP_DIR, false, ips)
	isError := false
	for _, result := range copyResults {
		if result.Error != nil {
			c.writer.Errorf("Copying to remote failed on node : %s with error: %v\n", result.HostIP, result.Error)
			isError = true
		}
	}
	if isError {
		return fmt.Errorf("remote copying failed on node")
	}

	writer.Printf("\nStarted Applying the Configurations in %s node: %s \n", remoteService, ips)
	excuteResults := c.sshUtil.ExecuteConcurrently(sshConfig, scriptCommands, ips)
	for _, result := range excuteResults {
		printCertRotateOutput(result, remoteService, writer)
	}
	return nil
}

// validateEachIp validate given ip with the remoteService cluster.
func (c *certRotateFlow) validateEachIp(remoteService string, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) bool {
	ips := c.getIps(remoteService, infra)
	for i := 0; i < len(ips); i++ {
		if ips[i] == flagsObj.node {
			return true
		}
	}
	return false
}

// getSshDetails will return the SSH details.
func (c *certRotateFlow) getSshDetails(infra *AutomateHAInfraDetails) *SSHConfig {
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
	}
	return sshConfig
}

// getIps will return the Ips based on the given remote service.
func (c *certRotateFlow) getIps(remoteService string, infra *AutomateHAInfraDetails) []string {
	if remoteService == AUTOMATE {
		return infra.Outputs.AutomatePrivateIps.Value
	} else if remoteService == CHEF_SERVER {
		return infra.Outputs.ChefServerPrivateIps.Value
	} else if remoteService == POSTGRESQL {
		return infra.Outputs.PostgresqlPrivateIps.Value
	} else if remoteService == OPENSEARCH {
		return infra.Outputs.OpensearchPrivateIps.Value
	} else if remoteService == "frontend" {
		return append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	}
	return []string{}
}

// isIPInCluster will check whether the given ip is in the cluster or not.
func (c *certRotateFlow) isIPInCluster(ip string, infra *AutomateHAInfraDetails) bool {
	cluster := c.getAllIPs(infra)
	for _, clusterIP := range cluster {
		if ip == clusterIP {
			return true
		}
	}
	return false
}

// getAllIps will return all the ips of the cluster.
func (c *certRotateFlow) getAllIPs(infra *AutomateHAInfraDetails) []string {
	ips := append(infra.Outputs.AutomatePrivateIps.Value, infra.Outputs.ChefServerPrivateIps.Value...)
	ips = append(ips, infra.Outputs.PostgresqlPrivateIps.Value...)
	ips = append(ips, infra.Outputs.OpensearchPrivateIps.Value...)
	return ips
}

// getFilteredIps will return ips on which cert-rotation need to perform.
func (c *certRotateFlow) getFilteredIps(serviceIps, skipIpsList []string) []string {
	filteredIps := []string{}
	for _, ip := range serviceIps {
		if !stringutils.SliceContains(skipIpsList, ip) {
			filteredIps = append(filteredIps, ip)
		}
	}
	return filteredIps
}

// compareCurrentCertsWithNewCerts compare current certs and new certs and returns ips to skip cert-rotation.
func (c *certRotateFlow) compareCurrentCertsWithNewCerts(remoteService string, newCerts *certificates, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) []string {
	skipIpsList := []string{}
	isCertsSame := true
	if remoteService == AUTOMATE {
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.AutomateCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == CHEF_SERVER {
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.ChefServerCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == OPENSEARCH {
		isCertsSame = strings.TrimSpace(currentCertsInfo.OpensearchRootCert) == newCerts.rootCA
		isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminCert) == newCerts.adminCert) && isCertsSame
		isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminKey) == newCerts.adminKey) && isCertsSame
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.OpensearchCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == POSTGRESQL {
		isCertsSame = strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newCerts.rootCA
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.PostgresqlCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}
	return skipIpsList
}

// comparePublicCertAndPrivateCert compare new public-cert and private cert with current public-cert and private-cert and returns ips to skip cert-rotation.
func (c *certRotateFlow) comparePublicCertAndPrivateCert(newCerts *certificates, certByIpList []CertByIP, isCertsSame bool, flagsObj *certRotateFlags) []string {
	skipIpsList := []string{}
	for _, currentCerts := range certByIpList {
		isCertsSameTemp := false
		if (strings.TrimSpace(currentCerts.PublicKey) == newCerts.publicCert) && (strings.TrimSpace(currentCerts.PrivateKey) == newCerts.privateCert) && isCertsSame {
			isCertsSameTemp = true
		}

		if isCertsSameTemp {
			if flagsObj.node == currentCerts.IP {
				return []string{flagsObj.node}
			}
			skipIpsList = append(skipIpsList, currentCerts.IP)
		}
	}
	return skipIpsList
}

// getFrontEndIpsForSkippingCnAndRootCaPatching compare new root-ca and new cn with current root-ca and cn and returns ips to skip root-ca patching.
func (c *certRotateFlow) getFrontEndIpsForSkippingCnAndRootCaPatching(newRootCA, newCn, oldCn, oldRootCA, node string) bool {
	isRootCaSame := false

	if strings.TrimSpace(oldRootCA) == newRootCA {
		isRootCaSame = true
	}

	isCnSame := false

	if oldCn == newCn {
		isCnSame = true
	}

	if node == "" && isRootCaSame && isCnSame {
		return true
	}

	if node != "" && isCnSame {
		return true
	}

	return false
}

// skipMessagePrinter print the skip message
func (c *certRotateFlow) skipMessagePrinter(remoteService, skipIpsMsg, nodeFlag string, skipIpsList []string) {
	nodeString := "node"

	if len(skipIpsList) > 1 {
		nodeString = "nodes"
	}

	if len(skipIpsList) != 0 && nodeFlag == "" {
		writer.Warnf(skipIpsMsg, remoteService, nodeString, nodeString, strings.Join(skipIpsList, ", "))
	}

	if len(skipIpsList) != 0 && nodeFlag != "" {
		if stringutils.SliceContains(skipIpsList, nodeFlag) {
			writer.Warnf(skipIpsMsg, remoteService, nodeString, nodeString, strings.Join(skipIpsList, ", "))
		}
	}
}

// getCerts will read the certificate paths, and then return the required certificates.
func (c *certRotateFlow) getCerts(infra *AutomateHAInfraDetails, flagsObj *certRotateFlags) (*certificates, error) {
	privateCertPath := flagsObj.privateCertPath
	publicCertPath := flagsObj.publicCertPath
	rootCaPath := flagsObj.rootCAPath
	adminCertPath := flagsObj.adminCertPath
	adminKeyPath := flagsObj.adminKeyPath

	var rootCA, adminCert, adminKey []byte
	var err error

	const fileAccessErrorMsg string = "failed reading data from the given source"

	if privateCertPath == "" || publicCertPath == "" {
		return nil, errors.New("Please provide both public-cert and private-cert flags")
	}

	privateCert, err := c.getCertFromFile(privateCertPath, infra)
	if err != nil {
		return nil, status.Wrap(
			err,
			status.FileAccessError,
			fileAccessErrorMsg,
		)
	}
	block, _ := pem.Decode(privateCert)
	if block == nil {
		return nil, errors.New("Please provide the valid certificate for private-cert")
	}

	publicCert, err := c.getCertFromFile(publicCertPath, infra)
	if err != nil {
		return nil, status.Wrap(
			err,
			status.FileAccessError,
			fileAccessErrorMsg,
		)
	}
	block, _ = pem.Decode(publicCert)
	if block == nil {
		return nil, errors.New("Please provide the valid certificate for public-cert")
	}

	// Root CA is mandatory for PG and OS nodes. But root CA is ignored when node flag is provided
	if flagsObj.postgres || flagsObj.opensearch {
		if rootCaPath == "" && flagsObj.node == "" {
			return nil, errors.New("Please provide root-ca flag")
		}
		if rootCaPath != "" {
			rootCA, err = c.getCertFromFile(rootCaPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(rootCA)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for root-ca")
			}
		}
	}

	// Admin Cert and Admin Key is mandatory for OS nodes.
	if flagsObj.opensearch {
		if (len(adminCertPath) == 0 || len(adminKeyPath) == 0) && len(flagsObj.node) == 0 {
			return nil, errors.New("Please provide both admin-cert and admin-key flags")
		}
		if adminCertPath != "" && adminKeyPath != "" {
			adminCert, err = c.getCertFromFile(adminCertPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(adminCert)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for admin-cert")
			}

			adminKey, err = c.getCertFromFile(adminKeyPath, infra)
			if err != nil {
				return nil, status.Wrap(
					err,
					status.FileAccessError,
					fileAccessErrorMsg,
				)
			}
			block, _ = pem.Decode(adminKey)
			if block == nil {
				return nil, errors.New("Please provide the valid certificate for admin-key")
			}
		}
	}

	certs := &certificates{
		privateCert: strings.TrimSpace(string(privateCert)),
		publicCert:  strings.TrimSpace(string(publicCert)),
		rootCA:      strings.TrimSpace(string(rootCA)),
		adminCert:   strings.TrimSpace(string(adminCert)),
		adminKey:    strings.TrimSpace(string(adminKey)),
	}
	return certs, nil
}

// getCertFromFile will read the certificate from the given path (local or remote).
func (c *certRotateFlow) getCertFromFile(certPath string, infra *AutomateHAInfraDetails) ([]byte, error) {
	certPath = strings.TrimSpace(certPath)
	// Checking if the given path is remote or local.
	if c.IsRemotePath(certPath) {
		remoteFilePath, _, hostIP, err := c.GetRemoteFileDetails(certPath, infra)
		if err != nil {
			return nil, err
		}
		// Download certificate from remote host.
		sshConfig := c.getSshDetails(infra)
		sshConfig.hostIP = hostIP
		sshUtil := NewSSHUtil(sshConfig)

		out, err := sshUtil.connectAndExecuteCommandOnRemote("sudo cat "+remoteFilePath, true)
		if err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("Unable to read file from remote path: %v", certPath))
		}
		return []byte(strings.TrimSpace(out)), nil
	}
	cp, err := c.fileUtils.ReadFile(certPath)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("Unable to read file from local path: %v", certPath))
	}
	cpstr := strings.TrimSpace(string(cp))
	return []byte(cpstr), nil
}

// GetRemoteFileDetails returns the remote file details from the remotePath.
func (c *certRotateFlow) GetRemoteFileDetails(remotePath string, infra *AutomateHAInfraDetails) (string, string, string, error) {
	// Get Host IP from the given path and validate it.
	hostIP := c.GetIPV4(remotePath)
	if net.ParseIP(hostIP).To4() == nil {
		return "", "", "", errors.New(fmt.Sprintf("%v is not a valid IPv4 address", hostIP))
	}

	// Check if given IP is part cluster.
	if !c.isIPInCluster(hostIP, infra) {
		return "", "", "", errors.New(fmt.Sprintf("%v is not part of the cluster", hostIP))
	}

	// Get the file path and filename from the given remote address.
	certPaths := strings.Split(remotePath, ":")
	if len(certPaths) != 2 || certPaths[1] == "" {
		return "", "", "", errors.New(fmt.Sprintf("Invalid remote path: %v", remotePath))
	}

	// Get the file path and validate it.
	remoteFilePath := filepath.Clean(strings.TrimSpace(certPaths[1]))

	// Get the filename from the file path.
	fileName := filepath.Base(remoteFilePath)
	if fileName == "." || fileName == string(os.PathSeparator) {
		return "", "", "", errors.New(fmt.Sprintf("Invalid remote path: %v", remotePath))
	}

	return remoteFilePath, fileName, hostIP, nil
}

/*
IsRemotePath checks whether given path is valid remote path.
Path should be in this format <IPv4>:<PathToFile>.

Example: 10.1.0.234:/home/ec2-user/certs/public.pem
*/
func (c *certRotateFlow) IsRemotePath(path string) bool {
	pattern := regexp.MustCompile(`^` + IP_V4_REGEX + `:`)
	return pattern.MatchString(path)
}

// GetIPV4 returns ip from given path.
func (c *certRotateFlow) GetIPV4(path string) string {
	pattern := regexp.MustCompile(IP_V4_REGEX)
	return pattern.FindString(path)
}

/*
getMerger will create the new toml file which includes old and new configurations.

If we are working on backend service, then first we have to get the applied configurations
and then merge it with new configurations, then apply that configuration.
Because if we directly apply the new configurations, then the old applied configurations will be gone.
So, we have to retain the old configurations also.
*/
func (c *certRotateFlow) getMerger(fileName string, timestamp string, remoteType string, config string, sshUtil SSHUtil) (string, error) {
	tomlFile := fileName + timestamp
	scriptCommands := fmt.Sprintf(config, remoteType)
	rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		return "", err
	}
	var (
		dest interface{}
		err1 error
	)
	if remoteType == OPENSEARCH {
		dest, err1 = getMergedOpensearchInterface(rawOutput, fileName, remoteType)
	} else {
		dest, err1 = getMergedPostgresqlInterface(rawOutput, fileName, remoteType)
	}
	if err1 != nil {
		return "", err1
	}

	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file
		writer.Bodyf("Failed to create/open the file, \n%v", err)
		return "", err
	}
	var buf bytes.Buffer
	if err := toml.NewEncoder(&buf).Encode(dest); err != nil {
		writer.Bodyf("Failed to encode\n%v", err)
		return "", err
	}
	processed := convertToMultilineIndented(buf.String(), "nodes_dn")
	if _, err := f.WriteString(processed); err != nil {
		writer.Bodyf("Failed to write to file\n%v", err)
		return "", err
	}
	if err := f.Close(); err != nil {
		writer.Bodyf("Failed to close the file\n%v", err)
		return "", err
	}

	return tomlFile, nil
}

func convertToMultilineIndented(tomlStr string, key string) string {
	pattern := fmt.Sprintf(`(?m)^([ \t]*)%s\s*=\s*"((?:[^"\\]|\\.)*)"$`, regexp.QuoteMeta(key))
	re := regexp.MustCompile(pattern)
	return re.ReplaceAllStringFunc(tomlStr, func(line string) string {
		matches := re.FindStringSubmatch(line)
		if len(matches) < 3 {
			return line
		}
		indent := matches[1]
		escaped := matches[2]
		unescaped := strings.ReplaceAll(escaped, `\\`, `\`)
		unescaped = strings.ReplaceAll(unescaped, `\n`, "\n")
		unescaped = strings.ReplaceAll(unescaped, `\`, `\\`)
		lines := strings.Split(unescaped, "\n")
		for i, line := range lines {
			if i == 0 {
				continue
			}
			lines[i] = line
		}
		final := trimFirstAndLastLineSpaces(strings.Join(lines, "\n"))
		return fmt.Sprintf(`%s%s = """%s%s"""`, indent, key, "\n", final)
	})
}

func trimFirstAndLastLineSpaces(s string) string {
	lines := strings.Split(s, "\n")
	if len(lines) == 0 {
		return s
	}
	lines[0] = strings.TrimLeft(lines[0], " ")
	lines[len(lines)-1] = strings.TrimRight(lines[len(lines)-1], " ")
	return strings.Join(lines, "\n")
}
