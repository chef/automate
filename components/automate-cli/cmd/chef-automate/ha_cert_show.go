package main

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/stringutils"
	"github.com/spf13/cobra"
)

type certShowFlags struct {
	automate   bool
	chefserver bool
	postgresql bool
	opensearch bool

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
		Long:  "Chef Automate CLI command to show all certificates on HA cluster, this command should always be executed from AutomateHA Bastion Node",
		RunE:  certShowCmdFunc(&flagsObj),
	}

	certShowCmd.PersistentFlags().BoolVarP(&flagsObj.automate, CONST_AUTOMATE, "a", false, "Show Automate Certificates")
	certShowCmd.PersistentFlags().BoolVar(&flagsObj.automate, "a2", false, "Show Automate Certificates")

	certShowCmd.PersistentFlags().BoolVarP(&flagsObj.chefserver, CONST_CHEF_SERVER, "c", false, "Show Chef Server Certificates")
	certShowCmd.PersistentFlags().BoolVar(&flagsObj.chefserver, "cs", false, "Show Chef Server Certificates")

	certShowCmd.PersistentFlags().BoolVarP(&flagsObj.postgresql, CONST_POSTGRESQL, "p", false, "Show Postgres Certificates")
	certShowCmd.PersistentFlags().BoolVar(&flagsObj.postgresql, "pg", false, "Show Postgres Certificates")

	certShowCmd.PersistentFlags().BoolVarP(&flagsObj.opensearch, CONST_OPENSEARCH, "o", false, "Show Opensearch Certificates")
	certShowCmd.PersistentFlags().BoolVar(&flagsObj.opensearch, "os", false, "Show Opensearch Certificates")

	certShowCmd.PersistentFlags().StringVarP(&flagsObj.node, "node", "n", "", "Service cluster's node IP address to show certificates, if not provided then all nodes certificates will be shown")

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

// certShowCmdFunc is the main function for the cert show command
func certShowCmdFunc(flagsObj *certShowFlags) func(cmd *cobra.Command, args []string) error {
	return func(cmd *cobra.Command, args []string) error {
		cs := NewCertShowImpl(*flagsObj, NewNodeUtils(), NewSSHUtil(&SSHConfig{}), writer)
		return cs.certShow(cmd, args)
	}
}

// certShow is the return function for all cert show commands
func (c *certShowImpl) certShow(cmd *cobra.Command, args []string) error {
	if !c.nodeUtils.isA2HARBFileExist() {
		return status.New(status.InvalidCommandArgsError, AUTOMATE_HA_INVALID_BASTION)
	}

	remoteService := c.getRemoteService()

	if remoteService == "" && len(strings.TrimSpace(c.flags.node)) > 0 {
		return status.New(status.InvalidCommandArgsError, "Node flag can only be used with service flags like --automate, --chef_server, --postgresql or --opensearch")
	}

	config, err := c.nodeUtils.pullAndUpdateConfig(&c.sshUtil, nil)
	if err != nil {
		return err
	}
	certInfo := c.getCerts(config)

	return c.validateAndPrintCertificates(remoteService, certInfo)
}

func (c *certShowImpl) validateAndPrintCertificates(remoteService string, certInfo certShowCertificates) error {
	switch remoteService {
	case CONST_AUTOMATE:
		if err := c.validateNode(certInfo.AutomateCertsByIP, CONST_AUTOMATE); err != nil {
			return err
		}
		c.printAutomateAndCSCertificates(certInfo.AutomateRootCert, certInfo.AutomateCertsByIP, stringutils.Title(CONST_AUTOMATE))
	case CONST_CHEF_SERVER:
		if err := c.validateNode(certInfo.ChefServerCertsByIP, CONST_CHEF_SERVER); err != nil {
			return err
		}
		c.printAutomateAndCSCertificates("", certInfo.ChefServerCertsByIP, stringutils.TitleSplit(CONST_CHEF_SERVER, "_"))
	case CONST_POSTGRESQL:
		if c.nodeUtils.isManagedServicesOn() {
			return status.New(status.InvalidCommandArgsError, "This command is not supported in Managed Services")
		}
		if err := c.validateNode(certInfo.PostgresqlCertsByIP, CONST_POSTGRESQL); err != nil {
			return err
		}
		c.printPostgresqlAndOSCertificates(certInfo.PostgresqlRootCert, "", "", certInfo.PostgresqlCertsByIP, stringutils.Title(CONST_POSTGRESQL))
	case CONST_OPENSEARCH:
		if c.nodeUtils.isManagedServicesOn() {
			return status.New(status.InvalidCommandArgsError, "This command is not supported in Managed Services")
		}
		if err := c.validateNode(certInfo.OpensearchCertsByIP, CONST_OPENSEARCH); err != nil {
			return err
		}
		c.printPostgresqlAndOSCertificates(certInfo.OpensearchRootCert, certInfo.OpensearchAdminKey, certInfo.OpensearchAdminCert, certInfo.OpensearchCertsByIP, stringutils.Title(CONST_OPENSEARCH))
	default:
		c.printAllCertificates(certInfo)
	}
	return nil
}

// getCerts returns the certificates from the config
func (c *certShowImpl) getCerts(config *ExistingInfraConfigToml) certShowCertificates {
	certInfo := certShowCertificates{}

	certInfo.AutomateRootCert = config.Automate.Config.RootCA
	certInfo.AutomateCertsByIP = config.Automate.Config.CertsByIP

	certInfo.ChefServerCertsByIP = config.ChefServer.Config.CertsByIP

	if !c.nodeUtils.isManagedServicesOn() {
		certInfo.PostgresqlRootCert = config.Postgresql.Config.RootCA
		certInfo.PostgresqlCertsByIP = config.Postgresql.Config.CertsByIP

		certInfo.OpensearchRootCert = config.Opensearch.Config.RootCA
		certInfo.OpensearchAdminKey = config.Opensearch.Config.AdminKey
		certInfo.OpensearchAdminCert = config.Opensearch.Config.AdminCert
		certInfo.OpensearchCertsByIP = config.Opensearch.Config.CertsByIP
	}

	return certInfo
}

// printAllCertificates prints all certificates
func (c *certShowImpl) printAllCertificates(certInfo certShowCertificates) {
	c.printAutomateAndCSCertificates(certInfo.AutomateRootCert, certInfo.AutomateCertsByIP, stringutils.Title(CONST_AUTOMATE))
	c.printAutomateAndCSCertificates("", certInfo.ChefServerCertsByIP, stringutils.TitleSplit(CONST_CHEF_SERVER, "_"))

	if !c.nodeUtils.isManagedServicesOn() {
		c.printPostgresqlAndOSCertificates(certInfo.PostgresqlRootCert, "", "", certInfo.PostgresqlCertsByIP, stringutils.Title(CONST_POSTGRESQL))
		c.printPostgresqlAndOSCertificates(certInfo.OpensearchRootCert, certInfo.OpensearchAdminKey, certInfo.OpensearchAdminCert, certInfo.OpensearchCertsByIP, stringutils.Title(CONST_OPENSEARCH))
	}
}

// printAutomateAndCSCertificates prints automate or chef server certificates
func (c *certShowImpl) printAutomateAndCSCertificates(rootCA string, certsByIP []CertByIP, remoteServiceName string) {
	c.writer.Title(fmt.Sprintf("%s Certificates", remoteServiceName))
	c.writer.HR()

	if remoteServiceName == stringutils.Title(CONST_AUTOMATE) {
		c.writer.Println("========================Automate Root CA========================")
		if len(strings.TrimSpace(rootCA)) == 0 {
			c.writer.Println("No Automate root certificate found")
		} else {
			c.writer.Println(rootCA)
		}
	}

	if len(certsByIP) == 0 {
		c.writer.Println(fmt.Sprintf("No public and private key found for %s", remoteServiceName))
		return
	}

	if c.flags.node == "" && c.isCommonCerts(certsByIP) {
		c.writer.Println(fmt.Sprintf("\n%s certificates are common across all nodes.\n", remoteServiceName))
		c.printPublicAndPrivateKeys(certsByIP[0], remoteServiceName, false)
		return
	}

	for _, certs := range certsByIP {
		c.printPublicAndPrivateKeys(certs, remoteServiceName, true)
	}

}

// printPostgresqlAndOSCertificates prints the postgresql and opensearch certificates
func (c *certShowImpl) printPostgresqlAndOSCertificates(rootCA, adminKey, adminCert string, certsByIP []CertByIP, remoteServiceName string) {
	c.writer.Title(fmt.Sprintf("%s Certificates", remoteServiceName))
	c.writer.HR()

	c.writer.Println(fmt.Sprintf("=======================%s Root CA=======================", remoteServiceName))
	if len(strings.TrimSpace(rootCA)) == 0 {
		c.writer.Println(fmt.Sprintf("No %s root certificate found", remoteServiceName))
	} else {
		c.writer.Println(rootCA)
	}

	if remoteServiceName == stringutils.Title(CONST_OPENSEARCH) {
		c.writer.Println("\n======================Opensearch Admin Key======================")
		if len(strings.TrimSpace(adminKey)) == 0 {
			c.writer.Println("No admin key found")
		} else {
			c.writer.Println(adminKey)
		}
		c.writer.Println("\n======================Opensearch Admin Cert======================")
		if len(strings.TrimSpace(adminCert)) == 0 {
			c.writer.Println("No admin certificate found")
		} else {
			c.writer.Println(adminCert)
		}
	}

	if len(certsByIP) == 0 {
		c.writer.Println(fmt.Sprintf("No public and private key found for %s", remoteServiceName))
		return
	}

	if c.flags.node == "" && c.isCommonCerts(certsByIP) {
		c.writer.Println(fmt.Sprintf("\n%s certificates are common across all nodes.\n", remoteServiceName))
		c.printPublicAndPrivateKeys(certsByIP[0], remoteServiceName, false)
		return
	}

	for _, certs := range certsByIP {
		c.printPublicAndPrivateKeys(certs, remoteServiceName, true)
	}

}

// printPublicAndPrivateKeys prints the public and private keys for a given service
func (c *certShowImpl) printPublicAndPrivateKeys(certs CertByIP, remoteService string, printNode bool) {
	if c.flags.node != "" && c.flags.node != certs.IP {
		return
	}

	if printNode {
		c.writer.Println(fmt.Sprintf("\n%s Certificates for %s\n", remoteService, certs.IP))
	}

	c.writer.Println(fmt.Sprintf("=======================%s Public Key=======================", remoteService))
	if len(strings.TrimSpace(certs.PublicKey)) == 0 {
		c.writer.Println(fmt.Sprintf("No %s public key found", remoteService))
	} else {
		c.writer.Println(certs.PublicKey)
	}

	c.writer.Println(fmt.Sprintf("\n======================%s Private Key======================", remoteService))
	if len(strings.TrimSpace(certs.PrivateKey)) == 0 {
		c.writer.Println(fmt.Sprintf("No %s private key found", remoteService))
	} else {
		c.writer.Println(certs.PrivateKey)
	}
}

// validateNode validates if the node exists in the cluster
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

// isCertSame checks if the certificate is the same as the one in the config
func (c *certShowImpl) isCertSame(certA string, CertB string) bool {
	if len(strings.TrimSpace(certA)) == 0 {
		return true
	}
	certA = strings.TrimSpace(certA)
	CertB = strings.TrimSpace(CertB)
	return certA == CertB
}

// isCommonCerts compares the certificates of all nodes in the cluster
func (c *certShowImpl) isCommonCerts(certs []CertByIP) bool {
	for _, cert := range certs {
		if c.isCertSame(cert.PublicKey, certs[0].PublicKey) && c.isCertSame(cert.PrivateKey, certs[0].PrivateKey) {
			continue
		} else {
			return false
		}
	}
	return true
}

// getRemoteService returns the remote service name based on the flags
func (c *certShowImpl) getRemoteService() string {
	if c.flags.automate {
		return CONST_AUTOMATE
	} else if c.flags.chefserver {
		return CONST_CHEF_SERVER
	} else if c.flags.postgresql {
		return CONST_POSTGRESQL
	} else if c.flags.opensearch {
		return CONST_OPENSEARCH
	} else {
		return ""
	}
}
