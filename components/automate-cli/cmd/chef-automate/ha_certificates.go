package main

import (
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
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
		Use:   "show COMMAND",
		Short: "Chef Automate Certificates Show",
		Long:  "Chef Automate CLI command to show certificates, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowCmdFunc(&flagsObj),
	}

	/*
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
	*/

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
		return cs.certShow(cmd, args)
	}
}

func (c *certShowImpl) certShow(cmd *cobra.Command, args []string) error {
	if !isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	config, err := c.getHAConfig()
	if err != nil {
		return err
	}
	certInfo := c.getCerts(config)
	c.printCertificates(certInfo)
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
	c.writer.Println(certInfo.AutomateRootCert)
	for _, certs := range certInfo.AutomateCertsByIP {
		c.writer.Println(fmt.Sprintf("Automate Certificates for %s", certs.IP))
		c.writer.Println("========================Automate Public Key========================")
		c.writer.Println(certs.PublicKey)
		c.writer.Println("========================Automate Private Key========================")
		c.writer.Println(certs.PrivateKey)
	}
}

func (c *certShowImpl) printChefServerCertificates(certInfo certShowCertificates) {
	c.writer.Title("Chef Server Certificates")
	c.writer.HR()
	for _, certs := range certInfo.ChefServerCertsByIP {
		c.writer.Println(fmt.Sprintf("Chef Server Certificates for %s", certs.IP))
		c.writer.Println("========================Chef Server Public Key========================")
		c.writer.Println(certs.PublicKey)
		c.writer.Println("========================Chef Server Private Key========================")
		c.writer.Println(certs.PrivateKey)
	}
}

func (c *certShowImpl) printPostgresqlCertificates(certInfo certShowCertificates) {
	c.writer.Title("Postgresql Certificates")
	c.writer.HR()
	c.writer.Println("========================Postgresql Root CA========================")
	c.writer.Println(certInfo.PostgresqlRootCert)
	for _, certs := range certInfo.PostgresqlCertsByIP {
		c.writer.Println(fmt.Sprintf("Postgresql Certificates for %s", certs.IP))
		c.writer.Println("========================Postgresql Public Key========================")
		c.writer.Println(certs.PublicKey)
		c.writer.Println("========================Postgresql Private Key========================")
		c.writer.Println(certs.PrivateKey)
	}
}

func (c *certShowImpl) printOpensearchCertificates(certInfo certShowCertificates) {
	c.writer.Title("Opensearch Certificates")
	c.writer.HR()
	c.writer.Println("========================Opensearch Root CA========================")
	c.writer.Println(certInfo.OpensearchRootCert)
	c.writer.Println("========================Opensearch Admin Key========================")
	c.writer.Println(certInfo.OpensearchAdminKey)
	c.writer.Println("========================Opensearch Admin Cert========================")
	c.writer.Println(certInfo.OpensearchAdminCert)
	for _, certs := range certInfo.OpensearchCertsByIP {
		c.writer.Println(fmt.Sprintf("Opensearch Certificates for %s", certs.IP))
		c.writer.Println("========================Opensearch Public Key========================")
		c.writer.Println(certs.PublicKey)
		c.writer.Println("========================Opensearch Private Key========================")
		c.writer.Println(certs.PrivateKey)
	}
}
