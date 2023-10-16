package main

import (
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	chefToml "github.com/chef/automate/components/automate-deployment/pkg/toml"
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
		admin_dn = '- %v'
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

	IP_V4_REGEX = `(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}`

	ERROR_SELF_MANAGED_DB_CERT_ROTATE = "Certificate rotation for externally configured %s is not supported."
	SKIP_IPS_MSG_CERT_ROTATE          = "The following %s %s will skip during certificate rotation as the following %s have the same certificates as currently provided certificates.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_A2         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Automate root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_PG         = "The following %s %s will skip during root-ca patching as the following %s have same root-ca as currently provided Postgresql root-ca.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_OS         = "The following %s %s will skip during root-ca and common name patching as the following %s have same root-ca and common name as currently provided OpenSearch root-ca and common name.\n\t %s"
	SKIP_FRONT_END_IPS_MSG_CN         = "The following %s %s will skip during common name patching as the following %s have same common name as currently provided OpenSearch common name.\n\t %s"
	DEFAULT_TIMEOUT                   = 600

	AUTOMATE_HA_CLUSTER_CONFIG = `
	[[load_balancer.v1.sys.frontend_tls]]
		cert = """%v"""
		key = """%v"""
	[[global.v1.frontend_tls]]
		cert = """%v"""
		key = """%v"""
	[global.v1.external.postgresql.ssl]
		enable = true
		root_cert = """%v"""
	[global.v1.external.opensearch.ssl]
		root_cert = """%v"""
		server_name = "%v"
	`
	MAINTENANICE_ON_LAG = `
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

	// Certificates Path
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

	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.automate, AUTOMATE, "a", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.automate, "a2", false, "Automate Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.chefserver, CHEF_SERVER, "c", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.chefserver, "cs", false, "Chef Infra Server Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.postgres, POSTGRESQL, "p", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.postgres, "pg", false, "Postgres Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVarP(&flagsObj.opensearch, OPENSEARCH, "o", false, "OS Certificate Rotation")
	certRotateCmd.PersistentFlags().BoolVar(&flagsObj.opensearch, "os", false, "OS Certificate Rotation")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.privateCertPath, "private-cert", "", "Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.publicCertPath, "public-cert", "", "Public certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.rootCAPath, "root-ca", "", "RootCA certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminCertPath, "admin-cert", "", "Admin certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.adminKeyPath, "admin-key", "", "Admin Private certificate")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.cluster, "certificate-config", "", "Cluster certificate file")
	certRotateCmd.PersistentFlags().StringVar(&flagsObj.cluster, "cc", "", "Cluster certificate file")

	certRotateCmd.PersistentFlags().StringVar(&flagsObj.node, "node", "", "Node Ip address")
	certRotateCmd.PersistentFlags().IntVar(&flagsObj.timeout, "wait-timeout", DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process")

	RootCmd.AddCommand(certRotateCmd)
	RootCmd.AddCommand(certTemplateGenerateCmd)
}

func certRotateCmdFunc(flagsObj *certRotateFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		log, err := logger.NewLogger("text", "info")
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

		if len(flagsObj.cluster) > 0 {
			err = c.certRotateFromTemplate(flagsObj.cluster, sshUtil, infra, currentCertsInfo)
			if err != nil {
				return err
			}
		} else {
			certs, err := c.getCerts(infra, flagsObj)
			if err != nil {
				return err
			}
			// we need to ignore root-ca, adminCert and adminKey in the case of each node
			if certs.rootCA != "" && flagsObj.node != "" {
				writer.Warn("root-ca flag will be ignored when node flag is provided")
			}
			if (certs.adminCert != "" || certs.adminKey != "") && flagsObj.node != "" {
				writer.Warn("admin-cert and admin-key flag will be ignored when node flag is provided")
			}

			if flagsObj.timeout < DEFAULT_TIMEOUT {
				return errors.Errorf("The operation timeout duration for each individual node during the certificate rotation process should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT)
			}

			if flagsObj.automate || flagsObj.chefserver {
				err := c.certRotateFrontend(sshUtil, certs, infra, flagsObj, currentCertsInfo)
				if err != nil {
					return err
				}
			} else if flagsObj.postgres {
				err := c.certRotatePG(sshUtil, certs, infra, flagsObj, currentCertsInfo, false)
				if err != nil {
					return err
				}
			} else if flagsObj.opensearch {
				err := c.certRotateOS(sshUtil, certs, infra, flagsObj, currentCertsInfo, false)
				if err != nil {
					return err
				}
			} else {
				return errors.New("Please Provide service flag")
			}
		}
	} else {
		return fmt.Errorf("cert-rotate command should be executed from Automate HA Bastion Node")
	}

	return nil
}

// certRotateFrontend will rotate the certificates of Automate and Chef Infra Server.
func (c *certRotateFlow) certRotateFrontend(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates) error {
	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if flagsObj.automate {
		remoteService = AUTOMATE
	} else if flagsObj.chefserver {
		remoteService = CHEF_SERVER
	}
	//get ips to exclude
	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	config := fmt.Sprintf(FRONTEND_CONFIG, certs.publicCert, certs.privateCert, certs.publicCert, certs.privateCert)
	concurrent := true
	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	err := c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	return nil
}

// certRotatePG will rotate the certificates of Postgres.
func (c *certRotateFlow) certRotatePG(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, POSTGRESQL)
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := POSTGRESQL

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, certs.privateCert, certs.publicCert)
	} else {
		config = fmt.Sprintf(POSTGRES_CONFIG, certs.privateCert, certs.publicCert, certs.rootCA)
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)
	concurrent := false
	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	// patching on PG
	err := c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	// ignore patching of root-ca when node flag is provided
	if flagsObj.node != "" {
		return nil
	}
	if skipFrontend {
		//Skiping frontend nodes
		return nil
	}
	return c.certRotateFrontendForPG(sshUtil, certs, infra, flagsObj, timestamp)
}

func (c *certRotateFlow) certRotateFrontendForPG(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, timestamp string) error {
	skipIpsList, err := c.getSkipIpsListForPgRootCAPatching(infra, sshUtil, certs)
	if err != nil {
		return err
	}
	c.skipMessagePrinter("frontend", SKIP_FRONT_END_IPS_MSG_PG, "", skipIpsList)

	//Patching root-ca to frontend-nodes for maintaining the connection.
	filenameFe := "pg_fe.toml"
	remoteService := "frontend"
	// Creating and patching the required configurations.
	configFe := fmt.Sprintf(POSTGRES_FRONTEND_CONFIG, certs.rootCA)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        configFe,
		fileName:      filenameFe,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    true,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	// patching frontend
	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	return nil
}

func (c *certRotateFlow) getSkipIpsListForPgRootCAPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates) ([]string, error) {

	c.pullConfigs.setInfraAndSSHUtil(infra, sshUtil)
	//fetching current config from automate
	automateCurrentConfig, err := c.pullConfigs.pullAutomateConfigs()
	if err != nil {
		return nil, err
	}

	//get frontend ips to skip root-ca patch
	skipIpsList := c.getFrontendIPsToSkipRootCAPatchingForPg(automateCurrentConfig, certs.rootCA, infra)

	//fetching current config from chefServer
	chefServerCurrentConfig, err := c.pullConfigs.pullChefServerConfigs()

	if err != nil {
		return nil, err
	}
	newskipIpsList := c.getFrontendIPsToSkipRootCAPatchingForPg(chefServerCurrentConfig, certs.rootCA, infra)
	skipIpsList = append(skipIpsList, newskipIpsList...)

	return skipIpsList, nil
}

// certRotateOS will rotate the certificates of OpenSearch.
func (c *certRotateFlow) certRotateOS(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, currentCertsInfo *certShowCertificates, skipFrontend bool) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, OPENSEARCH)
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := OPENSEARCH

	var adminDn pkix.Name
	var err error
	if flagsObj.node == "" {
		adminDn, err = getDistinguishedNameFromKey(certs.adminCert)
		if err != nil {
			return err
		}
	}
	nodeDn, err := getDistinguishedNameFromKey(certs.publicCert)
	if err != nil {
		return err
	}

	existingNodesDN := currentCertsInfo.OpensearchCertsByIP[0].NodesDn
	nodesDn := ""
	nodesCn := ""

	if flagsObj.node != "" {
		nodesDn = nodesDn + fmt.Sprintf("%v", existingNodesDN) + "\n  - " + fmt.Sprintf("%v", nodeDn) + "\n"
	} else {
		nodesDn = fmt.Sprintf("%v", nodeDn) + nodesDn
		nodesCn = nodeDn.CommonName
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.
	var config string
	if flagsObj.node != "" {
		config = fmt.Sprintf(OPENSEARCH_CONFIG_IGNORE_ADMIN_AND_ROOTCA, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", nodesDn))
	} else {
		config = fmt.Sprintf(OPENSEARCH_CONFIG, certs.rootCA, certs.adminCert, certs.adminKey, certs.publicCert, certs.privateCert, fmt.Sprintf("%v", adminDn), fmt.Sprintf("%v", nodesDn))
	}
	concurrent := false
	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	if flagsObj.node != "" && stringutils.SliceContains(skipIpsList, flagsObj.node) {
		return nil
	}

	if flagsObj.node != "" {

		err := patchOSNodeDN(flagsObj, patchFnParam, c, nodesDn)
		if err != nil {
			return err
		}

	}

	if skipFrontend {
		//Skiping frontend nodes
		return nil
	}
	return c.certRotateFrontendForOS(sshUtil, certs, infra, flagsObj, nodesCn, timestamp)
}

func (c *certRotateFlow) certRotateFrontendForOS(sshUtil SSHUtil, certs *certificates, infra *AutomateHAInfraDetails, flagsObj *certRotateFlags, nodesCn string, timestamp string) error {
	// Patching root-ca to frontend-nodes for maintaining the connection.
	filenameFe := "os_fe.toml"
	remoteService := "frontend"

	skipIpsList, err := c.getSkipIpsListForOsRootCACNPatching(infra, sshUtil, certs, nodesCn, flagsObj)
	if err != nil {
		return err
	}
	skipMessage := ""

	// Creating and patching the required configurations.
	var configFe string
	if flagsObj.node != "" {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG_IGNORE_ROOT_CERT, nodesCn)
		skipMessage = SKIP_FRONT_END_IPS_MSG_CN
	} else {
		configFe = fmt.Sprintf(OPENSEARCH_FRONTEND_CONFIG, certs.rootCA, nodesCn)
		skipMessage = SKIP_FRONT_END_IPS_MSG_OS
	}
	c.skipMessagePrinter(remoteService, skipMessage, "", skipIpsList)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        configFe,
		fileName:      filenameFe,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    true,
		infra:         infra,
		flagsObj:      flagsObj,
		skipIpsList:   skipIpsList,
	}

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	return nil
}

func (c *certRotateFlow) getSkipIpsListForOsRootCACNPatching(infra *AutomateHAInfraDetails, sshUtil SSHUtil, certs *certificates, nodesCn string, flagsObj *certRotateFlags) ([]string, error) {

	c.pullConfigs.setInfraAndSSHUtil(infra, sshUtil)
	//fetching current config from automate
	automateCurrentConfig, err := c.pullConfigs.pullAutomateConfigs()
	if err != nil {
		return nil, err
	}

	// Patching root-ca to frontend-nodes for maintaining the connection.
	cn := nodesCn

	skipIpsList := c.getFrontIpsToSkipRootCAandCNPatchingForOs(automateCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)

	//fetching current config from chefServer
	chefServerCurrentConfig, err := c.pullConfigs.pullChefServerConfigs()

	if err != nil {
		return nil, err
	}
	newskipIpsList := c.getFrontIpsToSkipRootCAandCNPatchingForOs(chefServerCurrentConfig, certs.rootCA, cn, flagsObj.node, infra)
	skipIpsList = append(skipIpsList, newskipIpsList...)

	return skipIpsList, nil
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
	err := c.patchConfig(patchFnParam)
	if err != nil {
		fmt.Println("Error @certRotate.go:612")
		fmt.Println(err)
		return err
	}

	flagsObj.node = nodeVal
	return nil
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

func isStringPresent(arr []string, str string) bool {
	for _, s := range arr {
		if s == str {
			return true
		}
	}
	return false
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
func (c *certRotateFlow) patchConfig(param *patchFnParameters) error {
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
	command := getScriptCommands(param, scriptCommands)
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

// Defining set of commands which run on particular remoteservice nodes
func getScriptCommands(param *patchFnParameters, scriptCommands string) string {
	if param.remoteService == AUTOMATE || param.remoteService == CHEF_SERVER || param.remoteService == "frontend" {
		scriptCommands = fmt.Sprintf(FRONTEND_COMMAND, PATCH, param.remoteService+param.timestamp, DATE_FORMAT)
	} else if param.remoteService == POSTGRESQL || param.remoteService == OPENSEARCH {
		scriptCommands = fmt.Sprintf(COPY_USER_CONFIG, param.remoteService+param.timestamp, param.remoteService)
	}
	return scriptCommands
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

		fmt.Printf("Started Applying the Configurations in %s node: %s", remoteService, ips[i])
		output, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Printf(output + "\n")
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

	fmt.Printf("\nStarted Applying the Configurations in %s node: %s \n", remoteService, ips)
	excuteResults := c.sshUtil.ExecuteConcurrently(sshConfig, scriptCommands, ips)
	for _, result := range excuteResults {
		printCertRotateOutput(result, remoteService, writer)
	}
	return nil
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
		if flagsObj.node == "" {
			isCertsSame = strings.TrimSpace(currentCertsInfo.OpensearchRootCert) == newCerts.rootCA
			isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminCert) == newCerts.adminCert) && isCertsSame
			isCertsSame = (strings.TrimSpace(currentCertsInfo.OpensearchAdminKey) == newCerts.adminKey) && isCertsSame
		}
		skipIpsList = c.comparePublicCertAndPrivateCert(newCerts, currentCertsInfo.OpensearchCertsByIP, isCertsSame, flagsObj)
		return skipIpsList
	}

	if remoteService == POSTGRESQL {
		if flagsObj.node == "" {
			isCertsSame = strings.TrimSpace(currentCertsInfo.PostgresqlRootCert) == newCerts.rootCA
		}
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
		return []byte(out), nil
	}
	return c.fileUtils.ReadFile(certPath)
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
	if err := toml.NewEncoder(f).Encode(dest); err != nil {
		// failed to encode
		writer.Bodyf("Failed to encode\n%v", err)
		return "", err
	}
	if err := f.Close(); err != nil {
		// failed to close the file
		writer.Bodyf("Failed to close the file\n%v", err)
		return "", err
	}

	return tomlFile, nil
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

func getStatusSummary() (StatusSummary, error) {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return nil, err
	}
	var statusSummaryCmdFlags = StatusSummaryCmdFlags{
		isPostgresql: true,
	}
	sshUtil := NewSSHUtil(&SSHConfig{})
	remoteCmdExecutor := NewRemoteCmdExecutorWithoutNodeMap(sshUtil, writer)
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &statusSummaryCmdFlags, remoteCmdExecutor)
	err = statusSummary.Prepare()
	if err != nil {
		return nil, err
	}
	return statusSummary, nil
}

func getPGLeader() (string, string) {
	statusSummary, err := getStatusSummary()
	if err != nil {
		return "", ""
	}
	return statusSummary.GetPGLeaderNode()
}

func getMaxPGLag(log logger.Logger) (int64, error) {
	statusSummary, err := getStatusSummary()
	if err != nil {
		return 0, err
	}
	lag := statusSummary.GetPGMaxLagAmongFollowers()
	log.Debug("==========================================================")
	log.Debug("Total lag in PostgreSQL follower node is %d \n", lag)
	log.Debug("==========================================================")
	return lag, nil
}

func frontendMaintainenceModeOnOFF(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, onOFFSwitch string, hostIps []string, log logger.Logger) error {
	sshConfig.Timeout = 1000
	command := fmt.Sprintf(MAINTENANCE_ON_OFF, onOFFSwitch)
	log.Debug("========================== MAINTENANCE_ON_OFF ========================")
	log.Debug(command)
	log.Debug("========================== MAINTENANCE_ON_OFF ========================")
	excuteResults := sshUtil.ExecuteConcurrently(sshConfig, command, hostIps)
	for _, result := range excuteResults {
		printCertRotateOutput(result, "frontend", writer)
	}
	return nil
}

func startTrafficOnAutomateNode(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, log logger.Logger) error {
	hostIps := infra.Outputs.AutomatePrivateIps.Value
	err := frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtil, OFF, hostIps, log)
	if err != nil {
		return err
	}
	return nil
}

func startTrafficOnChefServerNode(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, log logger.Logger) error {
	hostIps := infra.Outputs.ChefServerPrivateIps.Value
	err := frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtil, OFF, hostIps, log)
	if err != nil {
		return err
	}
	return nil
}

func checkLagAndStopTraffic(infra *AutomateHAInfraDetails, sshConfig sshutils.SSHConfig, sshUtil sshutils.SSHUtil, log logger.Logger) error {
	fontendIps := infra.Outputs.AutomatePrivateIps.Value
	fontendIps = append(fontendIps, infra.Outputs.ChefServerPrivateIps.Value...)
	lag, err := getMaxPGLag(log)
	if err != nil {
		return err
	}
	//////////////////////////////////////////////////////////////////////////
	agree, err := writer.Confirm(fmt.Sprintf(MAINTENANICE_ON_LAG, lag))
	if err != nil {
		return status.Wrap(err, status.InvalidCommandArgsError, errMLSA)
	}
	if !agree {
		return status.New(status.InvalidCommandArgsError, errMLSA)
	}
	err = frontendMaintainenceModeOnOFF(infra, sshConfig, sshUtil, ON, fontendIps, log)
	if err != nil {
		return err
	}
	//////////////////////////////////////////////////////////////////////////

	waitingStart := time.Now()
	time.Sleep(10 * time.Second)
	for {
		lag, err := getMaxPGLag(log)
		if err != nil {
			return err
		}
		if lag == 0 {
			break
		} else {
			timeElapsed := time.Since(waitingStart)
			if timeElapsed.Seconds() >= 60 {
				return status.Wrap(errors.New(""), status.UnhealthyStatusError, fmt.Sprintf("Follower node is still behind the leader by %d bytes\n", lag))
			}
		}
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

func (c *certRotateFlow) certRotateFromTemplate(clusterCertificateFile string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, currentCertsInfo *certShowCertificates) error {
	sshConfig := c.getSshDetails(infra)
	configRes := sshutils.SSHConfig{
		SshUser:    sshConfig.sshUser,
		SshPort:    sshConfig.sshPort,
		SshKeyFile: sshConfig.sshKeyFile,
		HostIP:     sshConfig.hostIP,
		Timeout:    sshConfig.timeout,
	}
	c.log.Debug("==========================================================")
	c.log.Debug("Stopping traffic MAINTENANICE MODE ON")
	c.log.Debug("==========================================================")
	err := checkLagAndStopTraffic(infra, configRes, c.sshUtil, c.log)
	if err != nil {
		return err
	}
	templateCerts, err := getCertsFromTemplate(clusterCertificateFile)
	if err != nil {
		return err
	}
	//fmt.Println(templateCerts)
	if templateCerts != nil {
		// rotating PG certs
		start := time.Now()
		c.log.Debug("Started executing at %s \n", start.String())
		c.writer.Println("Rotating PostgreSQL certificates")
		pgRootCA := templateCerts.PostgreSQL.RootCA
		c.writer.Printf("Fetching PostgreSQL RootCA from template %s \n", pgRootCA)
		for _, pgIp := range templateCerts.PostgreSQL.IPS {
			c.writer.Println("Rotating PostgreSQL follower node certificates")
			err := c.rotatePGNodeCerts(infra, sshUtil, currentCertsInfo, pgRootCA, &pgIp, true)
			if err != nil {
				return err
			}
		}
		timeElapsed := time.Since(start)
		c.log.Debug("Time Elapsed to execute Postgresql certificate rotation since start %f \n", timeElapsed.Seconds())
		// rotating OS certs
		for i, osIp := range templateCerts.OpenSearch.IPS {
			c.writer.Printf("Rotating OpenSearch node %d certificates \n", i)
			err := c.rotateOSNodeCerts(infra, sshUtil, currentCertsInfo, &templateCerts.OpenSearch, &osIp, false)
			if err != nil {
				return err
			}
		}
		timeElapsed = time.Since(start)
		c.log.Debug("Time Elapsed to execute Opensearch certificate rotation since start %f \n", timeElapsed.Seconds())

		// rotate AutomateCerts

		for i, a2Ip := range templateCerts.Automate.IPS {
			c.writer.Printf("Rotating Automate node %d certificates \n", i)
			err := c.rotateAutomateNodeCerts(infra, sshUtil, currentCertsInfo, templateCerts, &a2Ip)
			if err != nil {
				return err
			}
		}

		timeElapsed = time.Since(start)
		c.log.Debug("Time Elapsed to execute Automate certificate rotation since start %f \n", timeElapsed.Seconds())

		c.log.Debug("==========================================================")
		c.log.Debug("Starting traffic on Autoamate nodes MAINTENANICE MODE OFF")
		c.log.Debug("==========================================================")
		err = startTrafficOnAutomateNode(infra, configRes, c.sshUtil, c.log)
		if err != nil {
			return err
		}

		for i, csIp := range templateCerts.ChefServer.IPS {
			c.writer.Printf("Rotating Chef Server node %d certificates \n", i)
			err := c.rotateChefServerNodeCerts(infra, sshUtil, currentCertsInfo, templateCerts, &csIp)
			if err != nil {
				return err
			}
		}

		timeElapsed = time.Since(start)
		c.log.Debug("Time Elapsed to execute ChefServer certificate rotation since start %f \n", timeElapsed.Seconds())

		c.log.Debug("==========================================================")
		c.log.Debug("Starting traffic on chef server nodes MAINTENANICE MODE OFF")
		c.log.Debug("==========================================================")
		err = startTrafficOnChefServerNode(infra, configRes, c.sshUtil, c.log)
		if err != nil {
			return err
		}

	}
	return nil
}

func (c *certRotateFlow) rotatePGNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, pgRootCA string, pgIps *IP, concurrent bool) error {
	start := time.Now()
	c.writer.Printf("Roating PostgreSQL node %s certificate at %s \n", pgIps.IP, start.String())
	if len(pgIps.PrivateKey) == 0 || len(pgIps.Publickey) == 0 {
		c.writer.Printf("Empty certificate for PostgerSQL node %s \n", pgIps.IP)
		return errors.New(fmt.Sprintf("Empty certificate for PostgerSQL node %s \n", pgIps.IP))
	}
	flagsObj := certRotateFlags{
		postgres:        true,
		rootCAPath:      pgRootCA,
		privateCertPath: pgIps.PrivateKey,
		publicCertPath:  pgIps.Publickey,
		node:            pgIps.IP,
		timeout:         1000,
	}
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, POSTGRESQL)
	}
	fileName := "cert-rotate-pg.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := POSTGRESQL

	// Creating and patching the required configurations.
	config := fmt.Sprintf(POSTGRES_CONFIG_IGNORE_ISSUER_CERT, certs.privateCert, certs.publicCert)

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, &flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}

	// patching on PG
	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	timeElapsed := time.Since(start)
	c.log.Debug("Time taken to roate PostgreSQL node %s certificate at %f \n", pgIps.IP, timeElapsed.Seconds())
	return nil
}

func (c *certRotateFlow) rotateOSNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, oss *NodeCertficate, osIp *IP, concurrent bool) error {
	start := time.Now()
	c.writer.Printf("Roating opensearch node %s certificate at %s \n", osIp.IP, start.String())
	if len(osIp.PrivateKey) == 0 || len(osIp.Publickey) == 0 {
		c.writer.Printf("Empty certificate for OpenSearch node %s \n", osIp.IP)
		return errors.New(fmt.Sprintf("Empty certificate for OpenSearch node %s \n", osIp.IP))
	}
	fmt.Printf("Admin cert path : %s", oss.AdminPublickey)
	flagsObj := certRotateFlags{
		opensearch:      true,
		rootCAPath:      oss.RootCA,
		adminKeyPath:    oss.AdminPrivateKey,
		adminCertPath:   oss.AdminPublickey,
		privateCertPath: osIp.PrivateKey,
		publicCertPath:  osIp.Publickey,
		node:            osIp.IP,
		timeout:         1000,
	}
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_DB_CERT_ROTATE, OPENSEARCH)
	}
	fileName := "cert-rotate-os.toml"
	timestamp := time.Now().Format("20060102150405")
	remoteService := OPENSEARCH
	adminPublicCert, err := c.getCertFromFile(oss.AdminPublickey, infra)
	if err != nil {
		return err
	}
	adminPublicCertString := strings.TrimSpace(string(adminPublicCert))
	adminPrivateCert, err := c.getCertFromFile(oss.AdminPrivateKey, infra)
	if err != nil {
		return err
	}
	adminDn, err := getDistinguishedNameFromKey(adminPublicCertString)
	if err != nil {
		c.writer.Printf("Error in decoding admin cert, not able to get adminDn")
		return err
	}
	nodeDn, err := getDistinguishedNameFromKey(certs.publicCert)
	if err != nil {
		c.writer.Printf("Error in decoding node cert, not able to get nodeDn")
		return err
	}
	existingNodesDN := strings.TrimSpace(currentCertsInfo.OpensearchCertsByIP[0].NodesDn)
	if strings.HasSuffix(existingNodesDN, `\n`) {
		i := strings.LastIndex(existingNodesDN, `\n`)
		existingNodesDN = existingNodesDN[:i] + strings.Replace(existingNodesDN[i:], `\n`, "", 1)
	}
	nodesDn := ""
	if strings.EqualFold(existingNodesDN, fmt.Sprintf("%v", nodeDn)) {
		nodesDn = fmt.Sprintf("%v", nodeDn)
	} else {
		nodesDn = fmt.Sprintf("%v\n", existingNodesDN) + "  - " + fmt.Sprintf("%v\n", nodeDn)
	}

	skipIpsList := c.compareCurrentCertsWithNewCerts(remoteService, certs, &flagsObj, currentCertsInfo)
	c.skipMessagePrinter(remoteService, SKIP_IPS_MSG_CERT_ROTATE, flagsObj.node, skipIpsList)

	// Creating and patching the required configurations.

	config := fmt.Sprintf(OPENSEARCH_CONFIG, certs.rootCA, adminPublicCertString, strings.TrimSpace(string(adminPrivateCert)), certs.publicCert, certs.privateCert, fmt.Sprintf("%v", adminDn), fmt.Sprintf("%v", nodesDn))

	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}

	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}

	if flagsObj.node != "" && stringutils.SliceContains(skipIpsList, flagsObj.node) {
		return nil
	}

	if flagsObj.node != "" {

		err := patchOSNodeDN(&flagsObj, patchFnParam, c, nodesDn)
		if err != nil {
			return err
		}

	}
	timeElapsed := time.Since(start)
	c.log.Debug("Time taken to roate opensearch node %s certificate at %f \n", osIp.IP, timeElapsed.Seconds())
	return nil
}

func (c *certRotateFlow) rotateAutomateNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, a2Ip *IP) error {
	if len(a2Ip.PrivateKey) == 0 || len(a2Ip.Publickey) == 0 {
		c.writer.Printf("Empty certificate for Automte node %s \n", a2Ip.IP)
		return errors.New(fmt.Sprintf("Empty certificate for Automte node %s \n", a2Ip.IP))
	}
	flagsObj := certRotateFlags{
		automate:        true,
		rootCAPath:      certToml.Automate.RootCA,
		privateCertPath: a2Ip.PrivateKey,
		publicCertPath:  a2Ip.Publickey,
		node:            a2Ip.IP,
		timeout:         1000,
	}
	return c.rotateClusterFrontendCertificates(infra, sshUtil, flagsObj, currentCertsInfo, certToml)
}

func (c *certRotateFlow) rotateChefServerNodeCerts(infra *AutomateHAInfraDetails, sshUtil SSHUtil, currentCertsInfo *certShowCertificates, certToml *CertificateToml, csIp *IP) error {
	if len(csIp.PrivateKey) == 0 || len(csIp.Publickey) == 0 {
		c.writer.Printf("Empty certificate for Chef Server node %s \n", csIp.IP)
		return errors.New(fmt.Sprintf("Empty certificate for Chef Server node %s \n", csIp.IP))
	}
	flagsObj := certRotateFlags{
		chefserver:      true,
		rootCAPath:      certToml.ChefServer.RootCA,
		privateCertPath: csIp.PrivateKey,
		publicCertPath:  csIp.Publickey,
		node:            csIp.IP,
		timeout:         1000,
	}
	return c.rotateClusterFrontendCertificates(infra, sshUtil, flagsObj, currentCertsInfo, certToml)
}

func (c *certRotateFlow) rotateClusterFrontendCertificates(infra *AutomateHAInfraDetails, sshUtil SSHUtil, flagsObj certRotateFlags, currentCertsInfo *certShowCertificates, certToml *CertificateToml) error {
	certs, err := c.getCerts(infra, &flagsObj)
	if err != nil {
		return err
	}

	fileName := "cert-rotate-fe.toml"
	timestamp := time.Now().Format("20060102150405")
	var remoteService string

	if flagsObj.automate {
		remoteService = AUTOMATE
	} else if flagsObj.chefserver {
		remoteService = CHEF_SERVER
	}
	//get ips to exclude
	skipIpsList := []string{}

	nodeDn := pkix.Name{}
	if len(certToml.OpenSearch.IPS) > 0 {
		opensearchFlagsObj := certRotateFlags{
			opensearch:      true,
			rootCAPath:      certToml.OpenSearch.RootCA,
			adminKeyPath:    certToml.OpenSearch.AdminPrivateKey,
			adminCertPath:   certToml.OpenSearch.AdminPublickey,
			privateCertPath: certToml.OpenSearch.IPS[0].PrivateKey,
			publicCertPath:  certToml.OpenSearch.IPS[0].Publickey,
			node:            certToml.OpenSearch.IPS[0].IP,
			timeout:         1000,
		}
		opensearchCerts, err := c.getCerts(infra, &opensearchFlagsObj)
		nodeDn, err = getDistinguishedNameFromKey(opensearchCerts.publicCert)
		if err != nil {
			return err
		}
	}
	opensearchRootCA, err := c.getCertFromFile(certToml.OpenSearch.RootCA, infra)
	if err != nil {
		return err
	}

	postgreSQLRootCA, err := c.getCertFromFile(certToml.PostgreSQL.RootCA, infra)
	if err != nil {
		return err
	}

	// Creating and patching the required configurations.
	config := fmt.Sprintf(AUTOMATE_HA_CLUSTER_CONFIG, certs.publicCert, certs.privateCert, certs.publicCert, certs.privateCert, string(postgreSQLRootCA), string(opensearchRootCA), nodeDn.CommonName)
	concurrent := true
	patchFnParam := &patchFnParameters{
		sshUtil:       sshUtil,
		config:        config,
		fileName:      fileName,
		timestamp:     timestamp,
		remoteService: remoteService,
		concurrent:    concurrent,
		infra:         infra,
		flagsObj:      &flagsObj,
		skipIpsList:   skipIpsList,
	}
	//fmt.Println(patchFnParam)
	err = c.patchConfig(patchFnParam)
	if err != nil {
		return err
	}
	return nil
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
	config, err := chefToml.Marshal(certTemplate)
	if err != nil {
		return err
	}
	writer.Printf("certificate config file is generate %s, Please update the file with releavent certificate file paths \n", fileName)
	return fUtils.WriteFile(fileName, config, 0600)
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
