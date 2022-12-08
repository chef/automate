package main

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/spf13/cobra"
)

const (
	CONST_AUTOMATE    = "automate"
	CONST_CHEF_SERVER = "chef-server"
	CONST_POSTGRESQL  = "postgresql"
	CONST_OPENSEARCH  = "opensearch"
)

type certShowFlags struct {
	node string
}

type certShowImpl struct {
	flags     certShowFlags
	nodeUtils NodeOpUtils
	sshUtil   SSHUtil
	writer    *cli.Writer
}

type certShowCertificates struct {
	//Automate Cets
	AutomateRootCert  string
	AutomateCertsByIP []CertByIP

	//ChefServer Cets
	ChefServerCertsByIP []CertByIP

	//Postgresql Cets
	PostgresqlRootCert  string
	PostgresqlCertsByIP []CertByIP

	//Opensearch Cets
	OpensearchRootCert  string
	OpensearchAdminKey  string
	OpensearchAdminCert string
	OpensearchCertsByIP []CertByIP
}

func init() {
	flagsObj := certShowFlags{}

	certCmd := &cobra.Command{
		Use:   "cert COMMAND",
		Short: "Chef Automate Certificate Management",
		Long:  "Chef Automate certificate management, this command should always be executed from AutomateHA Bastion Node.",
	}

	certShowCmd := &cobra.Command{
		Use:   "show",
		Short: "Chef Automate Certificates Show",
		Long:  "Chef Automate CLI command to show certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowCmdFunc(&flagsObj),
	}

	certShowAutomateCmd := &cobra.Command{
		Use:   "automate",
		Short: "Chef Automate Certificates Show Automate",
		Long:  "Chef Automate CLI command to show Automate certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowAutomateCmdFunc(&flagsObj),
	}
	certShowAutomateCmd.Flags().StringVarP(&flagsObj.node, "node", "n", "", "Chef Automate Node")
	certShowCmd.AddCommand(certShowAutomateCmd)

	certShowChefServerCmd := &cobra.Command{
		Use:   "chef-server",
		Short: "Chef Automate Certificates Show Chef-Server",
		Long:  "Chef Automate CLI command to show Chef Server certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowChefServerCmdFunc(&flagsObj),
	}
	certShowChefServerCmd.Flags().StringVarP(&flagsObj.node, "node", "n", "", "Chef Server Node")
	certShowCmd.AddCommand(certShowChefServerCmd)

	certShowPostgresqlCmd := &cobra.Command{
		Use:   "postgresql",
		Short: "Chef Automate Certificates Show Postgresql",
		Long:  "Chef Automate CLI command to show Postgresql certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowPostgresqlCmdFunc(&flagsObj),
	}
	certShowPostgresqlCmd.Flags().StringVarP(&flagsObj.node, "node", "n", "", "Postgresql Node")
	certShowCmd.AddCommand(certShowPostgresqlCmd)

	certShowOpensearchCmd := &cobra.Command{
		Use:   "opensearch",
		Short: "Chef Automate Certificates Show Opensearch",
		Long:  "Chef Automate CLI command to show Opensearch certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowOpensearchCmdFunc(&flagsObj),
	}
	certShowOpensearchCmd.Flags().StringVarP(&flagsObj.node, "node", "n", "", "Opensearch Node")
	certShowCmd.AddCommand(certShowOpensearchCmd)

	certCmd.AddCommand(certShowCmd)
	RootCmd.AddCommand(certCmd)
}

func NewCertShowImpl(flags certShowFlags, nodeUtils NodeOpUtils, sshUtil SSHUtil, writer *cli.Writer) certShowImpl {
	return certShowImpl{
		flags:     flags,
		nodeUtils: nodeUtils,
		sshUtil:   sshUtil,
		writer:    writer,
	}
}

func certShowCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args, "")
	}
}
func certShowAutomateCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args, CONST_AUTOMATE)
	}
}
func certShowChefServerCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args, CONST_CHEF_SERVER)
	}
}
func certShowPostgresqlCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args, CONST_POSTGRESQL)
	}
}
func certShowOpensearchCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args, CONST_OPENSEARCH)
	}
}

func (c *certShowImpl) certShow(cmd *cobra.Command, args []string, remoteService string) error {
	if !isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, AUTOMATE_HA_INVALID_BASTION)
	}
	config, err := c.getHAConfig()
	if err != nil {
		return err
	}
	certInfo := c.getCerts(config)

	switch remoteService {
	case CONST_AUTOMATE:
		if err := c.validateNode(certInfo.AutomateCertsByIP, CONST_AUTOMATE); err != nil {
			return err
		}
		c.printAutomateCertificates(certInfo)
	case CONST_CHEF_SERVER:
		if err := c.validateNode(certInfo.ChefServerCertsByIP, CONST_CHEF_SERVER); err != nil {
			return err
		}
		c.printChefServerCertificates(certInfo)
	case CONST_POSTGRESQL:
		if isManagedServicesOn() {
			return status.New(status.InvalidCommandArgsError, "This command is not supported in Managed Services")
		}
		if err := c.validateNode(certInfo.PostgresqlCertsByIP, CONST_POSTGRESQL); err != nil {
			return err
		}
		c.printPostgresqlCertificates(certInfo)
	case CONST_OPENSEARCH:
		if isManagedServicesOn() {
			return status.New(status.InvalidCommandArgsError, "This command is not supported in Managed Services")
		}
		if err := c.validateNode(certInfo.OpensearchCertsByIP, CONST_OPENSEARCH); err != nil {
			return err
		}
		c.printOpensearchCertificates(certInfo)
	default:
		c.printCertificates(certInfo)
	}

	return nil
}

func (c *certShowImpl) getHAConfig() (*ExistingInfraConfigToml, error) {
	infra, cfg, err := c.nodeUtils.getHaInfraDetails()
	if err != nil {
		return nil, err
	}
	c.sshUtil.setSSHConfig(cfg)
	configPuller := NewPullConfigs(infra, c.sshUtil)
	return configPuller.generateConfig()
}

func (c *certShowImpl) getCerts(config *ExistingInfraConfigToml) certShowCertificates {
	certInfo := certShowCertificates{}

	certInfo.AutomateRootCert = config.Automate.Config.RootCA
	certInfo.AutomateCertsByIP = config.Automate.Config.CertsByIP

	certInfo.ChefServerCertsByIP = config.ChefServer.Config.CertsByIP

	if !isManagedServicesOn() {
		certInfo.PostgresqlRootCert = config.Postgresql.Config.RootCA
		certInfo.PostgresqlCertsByIP = config.Postgresql.Config.CertsByIP

		certInfo.OpensearchRootCert = config.Opensearch.Config.RootCA
		certInfo.OpensearchAdminKey = config.Opensearch.Config.AdminKey
		certInfo.OpensearchAdminCert = config.Opensearch.Config.AdminCert
		certInfo.OpensearchCertsByIP = config.Opensearch.Config.CertsByIP
	}

	return certInfo
}

func (c *certShowImpl) printCertificates(certInfo certShowCertificates) {
	c.printAutomateCertificates(certInfo)
	c.printChefServerCertificates(certInfo)

	if !isManagedServicesOn() {
		c.printPostgresqlCertificates(certInfo)
		c.printOpensearchCertificates(certInfo)
	}
}

func (c *certShowImpl) printAutomateCertificates(certInfo certShowCertificates) {
	c.writer.Title("Automate Certificates")
	c.writer.HR()
	c.writer.Println("========================Automate Root CA========================")
	if len(strings.TrimSpace(certInfo.AutomateRootCert)) == 0 {
		c.writer.Println("No root certificate found")
	} else {
		c.writer.Println(certInfo.AutomateRootCert)
	}
	for _, certs := range certInfo.AutomateCertsByIP {
		c.printPublicAndPrivateKeys(certs, "Automate")
	}
}

func (c *certShowImpl) printChefServerCertificates(certInfo certShowCertificates) {
	c.writer.Title("Chef Server Certificates")
	c.writer.HR()
	for _, certs := range certInfo.ChefServerCertsByIP {
		c.printPublicAndPrivateKeys(certs, "Chef Server")
	}
}

func (c *certShowImpl) printPostgresqlCertificates(certInfo certShowCertificates) {
	c.writer.Title("Postgresql Certificates")
	c.writer.HR()
	c.writer.Println("=======================Postgresql Root CA=======================")
	if len(strings.TrimSpace(certInfo.PostgresqlRootCert)) == 0 {
		c.writer.Println("No root certificate found")
	} else {
		c.writer.Println(certInfo.PostgresqlRootCert)
	}
	for _, certs := range certInfo.PostgresqlCertsByIP {
		c.printPublicAndPrivateKeys(certs, "Postgresql")
	}
}

func (c *certShowImpl) printOpensearchCertificates(certInfo certShowCertificates) {
	c.writer.Title("Opensearch Certificates")
	c.writer.HR()
	c.writer.Println("=======================Opensearch Root CA=======================")
	if len(strings.TrimSpace(certInfo.OpensearchRootCert)) == 0 {
		c.writer.Println("No root certificate found")
	} else {
		c.writer.Println(certInfo.OpensearchRootCert)
	}
	c.writer.Println("\n======================Opensearch Admin Key======================")
	if len(strings.TrimSpace(certInfo.OpensearchAdminKey)) == 0 {
		c.writer.Println("No admin key found")
	} else {
		c.writer.Println(certInfo.OpensearchAdminKey)
	}
	c.writer.Println("\n======================Opensearch Admin Cert======================")
	if len(strings.TrimSpace(certInfo.OpensearchAdminCert)) == 0 {
		c.writer.Println("No admin certificate found")
	} else {
		c.writer.Println(certInfo.OpensearchAdminCert)
	}
	for _, certs := range certInfo.OpensearchCertsByIP {
		c.printPublicAndPrivateKeys(certs, "Opensearch")
	}
}

func (c *certShowImpl) printPublicAndPrivateKeys(certs CertByIP, remoteService string) {
	if c.flags.node != "" && c.flags.node != certs.IP {
		return
	}
	c.writer.Println(fmt.Sprintf("\n%s Certificates for %s\n", remoteService, certs.IP))
	c.writer.Println(fmt.Sprintf("=======================%s Public Key=======================", remoteService))
	if len(strings.TrimSpace(certs.PublicKey)) == 0 {
		c.writer.Println("No public key found")
	} else {
		c.writer.Println(certs.PublicKey)
	}
	c.writer.Println(fmt.Sprintf("\n======================%s Private Key======================", remoteService))
	if len(strings.TrimSpace(certs.PrivateKey)) == 0 {
		c.writer.Println("No private key found")
	} else {
		c.writer.Println(certs.PrivateKey)
	}
}

func (c *certShowImpl) validateNode(certs []CertByIP, remoteService string) error {
	if c.flags.node != "" {
		for _, cert := range certs {
			if cert.IP == c.flags.node {
				return nil
			}
		}
		return status.New(status.InvalidCommandArgsError, fmt.Sprintf("Node %s does not exist in the %s cluster.", c.flags.node, remoteService))
	}
	return nil
}
