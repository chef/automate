package main

import (
	"container/list"
	"encoding/json"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/stringutils"
	ptoml "github.com/pelletier/go-toml"
)

type existingInfra struct {
	config     ExistingInfraConfigToml
	configPath string
}

type keydetails struct {
	key      string
	certtype string
	svc      string
}

func newExistingInfa(configPath string) *existingInfra {
	return &existingInfra{
		configPath: configPath,
	}
}

func (e *existingInfra) doDeployWork(args []string) error {
	if !deployCmdFlags.skipVerify {
		_, configFile := getConfigFileFromArgs(args)
		err := executeConfigVerifyAndPromptConfirmationOnError(configFile)
		if err != nil {
			return err
		}
	}
	err := bootstrapEnv(e, deployCmdFlags.airgap, false, DEPLOY)
	if err != nil {
		return err
	}
	err = executeSecretsInitCommand(e.config.Architecture.ConfigInitials.SecretsKeyFile)
	if err != nil {
		return err
	}
	return executeDeployment(args)
}

func (e *existingInfra) doProvisionJob(args []string) error {
	return nil
}

func (e *existingInfra) generateConfig(state string) error {
	templateBytes, err := ioutil.ReadFile(e.getConfigPath())
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	e.config = ExistingInfraConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &e.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	errList := e.validateConfigFields()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid")
	}
	err = e.addDNTocertConfig()
	if err != nil {
		return err
	}
	return writeHAConfigFiles(existingNodesA2harbTemplate, e.config, state)
}

func (e *existingInfra) addDNTocertConfig() error {
	//If CustomCertsEnabled for OpenSearch is enabled, then get admin_dn and nodes_dn from the certs
	if e.config.Opensearch.Config.EnableCustomCerts {
		//If AdminCert is given then get the admin_dn from the cert
		if len(strings.TrimSpace(e.config.Opensearch.Config.AdminCert)) > 0 {
			admin_dn, err := getDistinguishedNameFromKey(e.config.Opensearch.Config.AdminCert)
			if err != nil {
				return err
			}
			e.config.Opensearch.Config.AdminDn = fmt.Sprintf("%v", admin_dn)
		}
		//If PublicKey is given then get the nodes_dn from the cert
		if len(strings.TrimSpace(e.config.Opensearch.Config.PublicKey)) > 0 {
			nodes_dn, err := getDistinguishedNameFromKey(e.config.Opensearch.Config.PublicKey)
			if err != nil {
				return err
			}
			e.config.Opensearch.Config.NodesDn = fmt.Sprintf("%v", nodes_dn)
		}

		NodesDn := ""

		//Set the admin_dn and nodes_dn in the config for all IP addresses
		for i := 0; i < len(e.config.Opensearch.Config.CertsByIP); i++ {
			//If PublicKey is given then get the node_dn from the cert
			publicKey := e.config.Opensearch.Config.CertsByIP[i].PublicKey
			if len(strings.TrimSpace(publicKey)) > 0 {
				nodeDn, err := getDistinguishedNameFromKey(publicKey)
				if err != nil {
					return err
				}
				if NodesDn == "" {
					NodesDn = NodesDn + fmt.Sprintf("%v", nodeDn) + "\\n  "
				} else {
					NodesDn = NodesDn + fmt.Sprintf("- %v", nodeDn) + "\\n  "
				}
			}
		}

		for i := 0; i < len(e.config.Opensearch.Config.CertsByIP); i++ {
			//If PublicKey is given then set the nodes_dn from the cert
			publicKey := e.config.Opensearch.Config.CertsByIP[i].PublicKey
			if len(strings.TrimSpace(publicKey)) > 0 {
				e.config.Opensearch.Config.CertsByIP[i].NodesDn = strings.TrimSpace(NodesDn)
			}
		}
	}
	return nil
}

func (e *existingInfra) getConfigPath() string {
	return e.configPath
}

func (e *existingInfra) validateConfigFields() *list.List {
	errorList := list.New()
	if len(e.config.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(e.config.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(e.config.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(e.config.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(e.config.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(e.config.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if len(e.config.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(e.config.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}

	if len(e.config.ExistingInfra.Config.AutomatePrivateIps) < 1 {
		errorList.PushBack("Invalid or empty automate_private_ips")
	}

	if len(e.config.ExistingInfra.Config.ChefServerPrivateIps) < 1 {
		errorList.PushBack("Invalid or empty chef_server_private_ips")
	}
	if (e.config.ExternalDB.Database.Type != "aws") && (e.config.ExternalDB.Database.Type != "self-managed") {
		if len(e.config.Opensearch.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty open-search instance_count")
		}
		if len(e.config.Postgresql.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty postgres-sql instance_count")
		}

		if len(e.config.ExistingInfra.Config.OpensearchPrivateIps) < 1 {
			errorList.PushBack("Invalid or empty opensearch_private_ips")
		}

		if len(e.config.ExistingInfra.Config.PostgresqlPrivateIps) < 1 {
			errorList.PushBack("Invalid or empty postgresql_private_ips")
		}
	}

	if len(e.config.Architecture.ConfigInitials.BackupConfig) > 0 {
		if e.config.Architecture.ConfigInitials.BackupConfig == "object_storage" {
			//fmt.Println("file -> ", e.config.ObjectStorage.Config.GoogleServiceAccountFile)
			// Bucket check is comman in AWS and GCP
			if len(e.config.ObjectStorage.Config.BucketName) < 1 {
				errorList.PushBack("Invalid or empty bucket_name")
			}
			// if gcpJsonFile file exist then it the gcp flow other wise aws flow
			gcpJsonFile := checkGoogleServiceAccountJson(e.config.ObjectStorage.Config.GoogleServiceAccountFile, errorList)
			if gcpJsonFile != nil {
				fmt.Println("no gcp config file ")
			} else {
				//fmt.Println("config -> ", c)
				//fmt.Println("--------------")
				if len(e.config.ObjectStorage.Config.AccessKey) < 1 {
					errorList.PushBack("Invalid or empty access_key")
				}
				if len(e.config.ObjectStorage.Config.SecretKey) < 1 {
					errorList.PushBack("Invalid or empty secret_key")
				}
				if len(e.config.ObjectStorage.Config.Endpoint) < 1 {
					errorList.PushBack("Invalid or empty endpoint")
				}
			}

		} else if e.config.Architecture.ConfigInitials.BackupConfig == "file_system" {
			if len(e.config.Architecture.ConfigInitials.BackupMount) < 1 {
				errorList.PushBack("Invalid or empty access_key")
			}
		} else {
			errorList.PushBack("Invalid or empty backup_config")
		}
	}

	errorList.PushBackList(e.validateIPs())
	errorList.PushBackList(e.validateCerts())
	errorList.PushBackList(e.validateExternalDbFields())
	return errorList
}

func (e *existingInfra) validateExternalDbFields() *list.List {
	errorList := list.New()
	if e.config.ExternalDB.Database.Type == "aws" {
		if len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchDomainName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchInstanceURL)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchSuperUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.AWS.AwsOsSnapshotRoleArn)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeyId)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeySecret)) < 1 {
			errorList.PushBack("Opensearch Domain Name and/or Instance URL and/or SuperUser Name and/or SuperUser Password and/or Snapshot Role Arn and/or OsUser AccessKey Id and/or OsUser AccessKey Secret are missing.")
		}
		if len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword)) < 1 {
			errorList.PushBack("PostgreQL Instance URL and/or SuperUser Name and/or SuperUser Password and/or DBUserName and/or DBUserPassword are missing ")
		}
	}
	if e.config.ExternalDB.Database.Type == "self-managed" {
		if len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchDomainName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchInstanceURL)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchSuperUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.Opensearch.OpensearchRootCert)) < 1 {
			errorList.PushBack("Opensearch Domain Name and/or Instance URL and/or SuperUser Name and/or SuperUser Password and/or Root Cert  are missing.")
		}
		if len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword)) < 1 ||
			len(strings.TrimSpace(e.config.ExternalDB.Database.PostgreSQL.PostgreSQLRootCert)) < 1 {
			errorList.PushBack("PostgreQL Instance URL and/or SuperUser Name and/or SuperUser Password and/or DBUserName and/or DBUserPassword and/or Root Cert are missing ")
		}
	}
	return errorList
}

/*
	func validateGoogleServiceAccountJson(config *GoogleServiceAccountJSON, errorList *list.List) {
		if config != nil {
			if len(strings.TrimSpace(config.Type)) < 1 ||
				len(strings.TrimSpace(config.ProjectID)) < 1 ||
				len(strings.TrimSpace(config.PrivateKeyID)) < 1 ||
				len(strings.TrimSpace(config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(config.ClientEmail)) < 1 ||
				len(strings.TrimSpace(config.ClientID)) < 1 ||
				len(strings.TrimSpace(config.AuthURI)) < 1 ||
				len(strings.TrimSpace(config.TokenURI)) < 1 ||
				len(strings.TrimSpace(config.AuthProviderX509CertURL)) < 1 ||
				len(strings.TrimSpace(config.ClientX509CertURL)) < 1 ||
				len(strings.TrimSpace(config.UniverseDomain)) < 1 {
				errorList.PushBack("some field are missing the service account json ")
			}
		}
	}
*/
func extractIPsFromCertsByIP(certsByIp []CertByIP) []string {
	ips := []string{}
	for _, el := range certsByIp {
		ips = append(ips, el.IP)
	}
	return ips
}

func (e *existingInfra) validateCerts() *list.List {

	errorList := list.New()

	// If automate root_ca is provided, check that it is valid
	if len(strings.TrimSpace(e.config.Automate.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: e.config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
		}))
	}

	// If chefserver root_ca is provided, check that it is valid
	if len(strings.TrimSpace(e.config.ChefServer.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: e.config.ChefServer.Config.RootCA, certtype: "root_ca", svc: "chefserver"},
		}))
	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if e.config.Automate.Config.EnableCustomCerts {
		if len(e.config.Automate.Config.CertsByIP) > 0 {
			if !stringutils.SubSlice(e.config.ExistingInfra.Config.AutomatePrivateIps, extractIPsFromCertsByIP(e.config.Automate.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some automate private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(e.config.ExistingInfra.Config.AutomatePrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range e.config.Automate.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for Automate requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "automate cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "automate cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(e.config.Automate.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(e.config.Automate.Config.PublicKey)) < 1 {
				errorList.PushBack("Automate public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.Automate.Config.PrivateKey, certtype: "private_key", svc: "automate"},
				{key: e.config.Automate.Config.PublicKey, certtype: "public_key", svc: "automate"},
			}))
		}

	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if e.config.ChefServer.Config.EnableCustomCerts {
		if len(e.config.ChefServer.Config.CertsByIP) > 0 {
			if !stringutils.SubSlice(e.config.ExistingInfra.Config.ChefServerPrivateIps, extractIPsFromCertsByIP(e.config.ChefServer.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some ChefServer private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(e.config.ExistingInfra.Config.ChefServerPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range e.config.ChefServer.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for chef_server requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "chef-server cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "chef-server cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(e.config.ChefServer.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(e.config.ChefServer.Config.PublicKey)) < 1 {
				errorList.PushBack("ChefServer public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.ChefServer.Config.PrivateKey, certtype: "private_key", svc: "chef-server"},
				{key: e.config.ChefServer.Config.PublicKey, certtype: "public_key", svc: "chef-server"},
			}))
		}
	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if e.config.Postgresql.Config.EnableCustomCerts {
		if len(e.config.Postgresql.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(e.config.Postgresql.Config.RootCA)) < 1 {
				errorList.PushBack("Postgresql root_ca is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
			}))
			if !stringutils.SubSlice(e.config.ExistingInfra.Config.PostgresqlPrivateIps, extractIPsFromCertsByIP(e.config.Postgresql.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Postgresql private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(e.config.ExistingInfra.Config.PostgresqlPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range e.config.Postgresql.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for postgresql requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "postgresql cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "postgresql cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(e.config.Postgresql.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(e.config.Postgresql.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(e.config.Postgresql.Config.PublicKey)) < 1 {
				errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
				{key: e.config.Postgresql.Config.PrivateKey, certtype: "private_key", svc: "postgresql"},
				{key: e.config.Postgresql.Config.PublicKey, certtype: "public_key", svc: "postgresql"},
			}))
		}
	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if e.config.Opensearch.Config.EnableCustomCerts {
		if len(e.config.Opensearch.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(e.config.Opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.AdminCert)) < 1 {
				errorList.PushBack("Opensearch root_ca, admin_key or admin_cert is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: e.config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: e.config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
			}))
			if !stringutils.SubSlice(e.config.ExistingInfra.Config.OpensearchPrivateIps, extractIPsFromCertsByIP(e.config.Opensearch.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Opensearch private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(e.config.ExistingInfra.Config.OpensearchPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range e.config.Opensearch.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for opensearch requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "opensearch cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "opensearch cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(e.config.Opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.AdminCert)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(e.config.Opensearch.Config.PublicKey)) < 1 {
				errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: e.config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: e.config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: e.config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
				{key: e.config.Opensearch.Config.PrivateKey, certtype: "private_key", svc: "opensearch"},
				{key: e.config.Opensearch.Config.PublicKey, certtype: "public_key", svc: "opensearch"},
			}))
		}
	}
	return errorList
}

func (e *existingInfra) validateIPs() *list.List {
	const notValidErrorString = "is not valid"
	errorList := list.New()

	for _, element := range e.config.ExistingInfra.Config.AutomatePrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Automate private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range e.config.ExistingInfra.Config.ChefServerPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("chef server private Ip " + element + notValidErrorString)
		}
	}

	if (e.config.ExternalDB.Database.Type != "aws") && (e.config.ExternalDB.Database.Type != "self-managed") {
		for _, element := range e.config.ExistingInfra.Config.OpensearchPrivateIps {
			if checkIPAddress(element) != nil {
				errorList.PushBack("open search private Ip " + element + notValidErrorString)
			}
		}

		for _, element := range e.config.ExistingInfra.Config.PostgresqlPrivateIps {
			if checkIPAddress(element) != nil {
				errorList.PushBack("Postgresql private Ip " + element + notValidErrorString)
			}
		}
	}

	if e.config.Automate.Config.EnableCustomCerts {
		for _, element := range e.config.Automate.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Automate IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}
	if e.config.ChefServer.Config.EnableCustomCerts {
		for _, element := range e.config.ChefServer.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("ChefServer IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	if e.config.Opensearch.Config.EnableCustomCerts {
		for _, element := range e.config.Opensearch.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Opensearch IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	if e.config.Postgresql.Config.EnableCustomCerts {
		for _, element := range e.config.Postgresql.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Postgresql IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	return errorList
}

func checkCertValid(keys []keydetails) *list.List {
	errorList := list.New()
	for _, el := range keys {
		block, _ := pem.Decode([]byte(el.key))
		if block == nil {
			errorList.PushBack("Invalid format. Failed to decode " + el.certtype + " for " + el.svc)
		}
	}
	return errorList
}

const AUTOMATE_HA_WORKSPACE_GOOGLE_SERVICE_FILE = "/hab/a2_deploy_workspace/googleServiceAccount.json"

func checkGoogleServiceAccountJson(filePath string, errorList *list.List) *GoogleServiceAccount {
	if len(strings.TrimSpace(filePath)) < 1 {
		return nil
	}
	// Open the JSON file
	//fmt.Println(filePath)
	file, err := os.Open(filePath)
	if err != nil {
		errorList.PushBack("Error opening file")
		return nil
	}
	defer file.Close()

	// Decode the JSON data
	var serviceAccount GoogleServiceAccount
	decoder := json.NewDecoder(file)
	err = decoder.Decode(&serviceAccount)
	if err != nil {
		fmt.Println("Error decoding JSON:", err.Error())
		errorList.PushBack(err.Error())
		return nil
	}
	fmt.Println("json", serviceAccount)
	// Validate the required fields
	if len(strings.TrimSpace(serviceAccount.Type)) < 1 {
		errorList.PushBack("Invalid type")
	}

	if len(strings.TrimSpace(serviceAccount.ProjectID)) < 1 {
		errorList.PushBack("Invalid project_id")
	}

	if len(strings.TrimSpace(serviceAccount.PrivateKeyID)) < 1 {
		errorList.PushBack("Invalid private_key_id")
	}

	if len(strings.TrimSpace(serviceAccount.PrivateKey)) < 1 {
		errorList.PushBack("Invalid private_key")
	}

	if len(strings.TrimSpace(serviceAccount.ClientEmail)) < 1 {
		errorList.PushBack("Invalid client_email")
	}

	if len(strings.TrimSpace(serviceAccount.ClientID)) < 1 {
		errorList.PushBack("Invalid client_id")
	}

	if len(strings.TrimSpace(serviceAccount.AuthURI)) < 1 {
		errorList.PushBack("Invalid auth_url")
	}

	if len(strings.TrimSpace(serviceAccount.TokenURI)) < 1 {
		errorList.PushBack("Invalid token_uri")
	}

	if len(strings.TrimSpace(serviceAccount.AuthProviderX509CertURL)) < 1 {
		errorList.PushBack("Invalid auth_provider_x509_cert_url")
	}

	if len(strings.TrimSpace(serviceAccount.ClientX509CertURL)) < 1 {
		errorList.PushBack("Invalid client_x509_cert_url")
	}

	if len(strings.TrimSpace(serviceAccount.UniverseDomain)) < 1 {
		errorList.PushBack("Invalid universe_domain")
	}

	// Add more validation checks as needed

	fmt.Println("Service Account JSON is valid")

	writeGoogleserviceJsonFile(AUTOMATE_HA_WORKSPACE_GOOGLE_SERVICE_FILE, serviceAccount, errorList)
	return &serviceAccount
}

func writeGoogleserviceJsonFile(filePath string, serviceAccount GoogleServiceAccount, errorList *list.List) {

	// Check if the file already exists
	if _, err := os.Stat(filePath); err == nil {
		// Remove the existing file
		if err := os.Remove(filePath); err != nil {
			errorList.PushBack("Error removing existing file")
			return
		}
	}

	// Create the JSON file
	file, err := os.Create(filePath)
	if err != nil {
		errorList.PushBack("Error while creating service account file")
		return
	}
	defer file.Close()

	// Encode and write the ServiceAccount struct as JSON
	encoder := json.NewEncoder(file)
	err = encoder.Encode(serviceAccount)
	if err != nil {
		errorList.PushBack("Error encoding service account JSON")
		return
	}

	fmt.Printf("Service Account JSON written to %s\n", filePath)

}
