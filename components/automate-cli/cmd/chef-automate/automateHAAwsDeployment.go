package main

import (
	"container/list"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/components/local-user-service/password"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/stringutils"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

const (
	DEPLOY       = "deploy"
	PROVISION    = "provision"
	TOKEN_URL    = "http://169.254.169.254/latest/api/token"
	METADATA_URL = "http://169.254.169.254/latest/meta-data/iam/info"
)

type awsDeployment struct {
	config     AwsConfigToml
	configPath string
	AWSConfigIp
	httpRequestClient httputils.HTTPClient
}

func newAwsDeployemnt(configPath string) *awsDeployment {
	log := logger.NewLogrusStandardLogger()
	return &awsDeployment{
		configPath:        configPath,
		httpRequestClient: httputils.NewClient(log),
	}
}

func (a *awsDeployment) doDeployWork(args []string) error {
	if isA2HARBFileExist() {
		if !deployCmdFlags.skipVerify {
			_, configFile := getConfigFileFromArgs(args)
			err := executeConfigVerifyAndPromptConfirmationOnError(configFile)
			if err != nil {
				return err
			}
		}
		err := a.generateConfig(DEPLOY)
		if err != nil {
			return status.Annotate(err, status.DeployError)
		}
		err = executeDeployment(args)
		if err != nil {
			return err
		}
		sharedConfigToml, err := getAwsHAConfig()
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to fetch HA config")
		}
		archBytes, err := os.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) // nosemgrep
		if err != nil {
			writer.Errorf("%s", err.Error())
			return err
		}
		var arch = strings.Trim(string(archBytes), "\n")
		sharedConfigToml.Architecture.ConfigInitials.Architecture = arch
		shardConfig, err := toml.Marshal(sharedConfigToml)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to marshal config to file")
		}
		err = os.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"), shardConfig, 0644) // nosemgrep
		if err != nil {
			return status.Wrap(err, status.ConfigError, "unable to write config toml to file")
		}
		return nil
	} else {
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
}

func (a *awsDeployment) doProvisionJob(args []string) error {
	writer.Print("AWS Provision")
	err := bootstrapEnv(a, deployCmdFlags.airgap, deployCmdFlags.saas, PROVISION)
	if err != nil {
		return err
	}
	err = executeSecretsInitCommand(a.config.Architecture.ConfigInitials.SecretsKeyFile)
	if err != nil {
		return err
	}
	writer.Printf("provisioning infra for automate HA \n\n\n\n")
	args = args[1:]
	args = append(args, "-y")
	if isA2HARBFileExist() {
		return executeAutomateClusterCtlCommandAsync("provision", args, provisionInfraHelpDocs, true)
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}

func (a *awsDeployment) generateConfig(state string) error {
	templateBytes, err := os.ReadFile(a.getConfigPath())
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	a.config = AwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &a.config)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) {
		archBytes, err := os.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) // nosemgrep
		if err != nil {
			writer.Errorf("%s", err.Error())
			return err
		}
		var arch = strings.Trim(string(archBytes), "\n")
		a.config.Architecture.ConfigInitials.Architecture = arch
		if state == DEPLOY {
			err = a.getAwsHAIp()
			if err != nil {
				return status.Wrap(err, status.IpAccessError, "Error in fetching node IPs in aws mode")
			}
		}
	}
	errList := a.validateConfigFields()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "config is invalid")
	}

	err = a.addDNTocertConfig()
	if err != nil {
		return err
	}
	return writeHAConfigFiles(awsA2harbTemplate, a.config, state)
}

func (a *awsDeployment) addDNTocertConfig() error {
	//If CustomCertsEnabled for OpenSearch is enabled, then get admin_dn and nodes_dn from the certs
	if a.config.Opensearch.Config.EnableCustomCerts {
		//If AdminCert is given then get the admin_dn from the cert
		if len(strings.TrimSpace(a.config.Opensearch.Config.AdminCert)) > 0 {
			admin_dn, err := getDistinguishedNameFromKey(a.config.Opensearch.Config.AdminCert)
			if err != nil {
				return err
			}
			a.config.Opensearch.Config.AdminDn = fmt.Sprintf("%v", admin_dn)
		}
		//If PublicKey is given then get the nodes_dn from the cert
		if len(strings.TrimSpace(a.config.Opensearch.Config.PublicKey)) > 0 {
			nodes_dn, err := getDistinguishedNameFromKey(a.config.Opensearch.Config.PublicKey)
			if err != nil {
				return err
			}
			a.config.Opensearch.Config.NodesDn = fmt.Sprintf("%v", nodes_dn)
		}

		NodesDn := ""

		//Set the admin_dn and nodes_dn in the config for all IP addresses
		for i := 0; i < len(a.config.Opensearch.Config.CertsByIP); i++ {
			//If PublicKey is given then get the node_dn from the cert
			publicKey := a.config.Opensearch.Config.CertsByIP[i].PublicKey
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

		for i := 0; i < len(a.config.Opensearch.Config.CertsByIP); i++ {
			//If PublicKey is given then set the nodes_dn from the cert
			publicKey := a.config.Opensearch.Config.CertsByIP[i].PublicKey
			if len(strings.TrimSpace(publicKey)) > 0 {
				a.config.Opensearch.Config.CertsByIP[i].NodesDn = strings.TrimSpace(NodesDn)
			}
		}
	}
	return nil
}

func (a *awsDeployment) getDistinguishedNameFromKey(publicKey string) (string, error) {
	block, _ := pem.Decode([]byte(publicKey))
	if block == nil {
		return "", status.New(status.ConfigError, "failed to decode certificate PEM")
	}
	cert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return "", status.Wrap(err, status.ConfigError, "failed to parse certificate PEM")
	}
	return fmt.Sprintf("%v", cert.Subject), nil
}

func (a *awsDeployment) getConfigPath() string {
	return a.configPath
}

func (a *awsDeployment) validateConfigFields() *list.List {
	errorList := list.New()
	if len(a.config.Architecture.ConfigInitials.SecretsKeyFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_key_file")
	}
	if len(a.config.Architecture.ConfigInitials.SecretsStoreFile) < 1 {
		errorList.PushBack("Invalid or empty secrets_store_file")
	}
	if len(a.config.Architecture.ConfigInitials.Architecture) < 1 {
		errorList.PushBack("Invalid or empty Architecture")
	}
	if len(a.config.Architecture.ConfigInitials.WorkspacePath) < 1 {
		errorList.PushBack("Invalid or empty workspace_path")
	}
	if len(a.config.Architecture.ConfigInitials.SSHUser) < 1 {
		errorList.PushBack("Invalid or empty ssh_user")
	}
	if len(a.config.Architecture.ConfigInitials.SSHKeyFile) < 1 {
		errorList.PushBack("Invalid or empty ssh_key_file")
	}
	if a.config.Architecture.ConfigInitials.BackupConfig == "efs" && len(a.config.Architecture.ConfigInitials.BackupMount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	}
	if a.config.Architecture.ConfigInitials.BackupConfig == "s3" && len(strings.TrimSpace(a.config.Architecture.ConfigInitials.S3BucketName)) < 1 {
		errorList.PushBack("Invalid or empty s3_bucketName")
	}
	if len(a.config.Automate.Config.AdminPassword) > 0 {
		val, err := password.NewValidator()
		if err != nil {
			errorList.PushBack(err.Error())
		}
		passvalErr := val.Validate(a.config.Automate.Config.AdminPassword)
		if passvalErr != nil {
			errorList.PushBack(passvalErr.Error())
		}
	}
	if len(a.config.Automate.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty automate instance_count")
	}
	if len(a.config.ChefServer.Config.InstanceCount) < 1 {
		errorList.PushBack("Invalid or empty chef-server instance_count")
	}
	if !a.config.Aws.Config.SetupManagedServices {
		if len(a.config.Opensearch.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty open-search instance_count")
		}
		if len(a.config.Postgresql.Config.InstanceCount) < 1 {
			errorList.PushBack("Invalid or empty postgres-sql instance_count")
		}
	}
	errorList.PushBackList(a.validateEnvFields())
	errorList.PushBackList(a.validateCerts())
	return errorList
}

func (a *awsDeployment) validateEnvFields() *list.List {
	errorList := list.New()
	if len(a.config.Aws.Config.Profile) < 1 {
		err := a.isIamRolePresent()
		if err != nil {
			errorList.PushBack("Invalid local AWS Profile name or Bastion IAM role, Please Check your local AWS Profile name or Bastion IAM Role is configured properly")
		}
	}
	if len(a.config.Aws.Config.Region) < 1 {
		errorList.PushBack("Invalid or empty aws region")
	}
	if len(a.config.Aws.Config.SSHKeyPairName) < 1 {
		errorList.PushBack("Invalid or empty aws ssh_key_pair_name")
	}
	if len(a.config.Aws.Config.LBAccessLogs) < 1 {
		errorList.PushBack("Invalid or empty aws lb_access_logs")
	}
	if len(a.config.Aws.Config.AutomateServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_server_instance_type")
	}
	if len(a.config.Aws.Config.ChefServerInstanceType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_instance_type")
	}
	if len(a.config.Aws.Config.AutomateLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws automate_lb_certificate_arn")
	}
	if len(a.config.Aws.Config.ChefServerLbCertificateArn) < 1 {
		errorList.PushBack("Invalid or empty aws chef_server_lb_certificate_arn")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_iops")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_size")
	}
	if len(a.config.Aws.Config.AutomateEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws automate_ebs_volume_type")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeIops) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_iops")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeSize) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_size")
	}
	if len(a.config.Aws.Config.ChefEbsVolumeType) < 1 {
		errorList.PushBack("Invalid or empty aws chef_ebs_volume_type")
	}
	if !a.config.Aws.Config.SetupManagedServices {
		if len(a.config.Aws.Config.OpensearchServerInstanceType) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_server_instance_type")
		}
		if len(a.config.Aws.Config.PostgresqlServerInstanceType) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_server_instance_type")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeIops) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_iops")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeSize) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_size")
		}
		if len(a.config.Aws.Config.OpensearchEbsVolumeType) < 1 {
			errorList.PushBack("Invalid or empty aws opensearch_ebs_volume_type")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeIops) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_iops")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeSize) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_size")
		}
		if len(a.config.Aws.Config.PostgresqlEbsVolumeType) < 1 {
			errorList.PushBack("Invalid or empty aws postgresql_ebs_volume_type")
		}
	}
	return errorList
}

func (a *awsDeployment) validateCerts() *list.List {
	errorList := list.New()

	// If automate root_ca is provided, check that it is valid
	if len(strings.TrimSpace(a.config.Automate.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
		}))
	}

	// If chefserver root_ca is provided, check that it is valid
	if len(strings.TrimSpace(a.config.ChefServer.Config.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: a.config.ChefServer.Config.RootCA, certtype: "root_ca", svc: "chefserver"},
		}))
	}
	if a.config.Automate.Config.EnableCustomCerts {
		if len(a.config.Automate.Config.CertsByIP) > 0 {

			if !stringutils.SubSlice(a.configAutomateIpList, extractIPsFromCertsByIP(a.config.Automate.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some automate private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(a.configAutomateIpList, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range a.config.Automate.Config.CertsByIP {
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
			if len(strings.TrimSpace(a.config.Automate.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(a.config.Automate.Config.PublicKey)) < 1 {
				errorList.PushBack("Automate public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
			}

			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.Automate.Config.PrivateKey, certtype: "private_key", svc: "automate"},
				{key: a.config.Automate.Config.PublicKey, certtype: "public_key", svc: "automate"},
			}))
		}
	}

	if a.config.ChefServer.Config.EnableCustomCerts {
		if len(a.config.ChefServer.Config.CertsByIP) > 0 {
			if !stringutils.SubSlice(a.configChefServerIpList, extractIPsFromCertsByIP(a.config.ChefServer.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some ChefServer private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(a.configChefServerIpList, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range a.config.ChefServer.Config.CertsByIP {
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
			if len(strings.TrimSpace(a.config.ChefServer.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(a.config.ChefServer.Config.PublicKey)) < 1 {
				errorList.PushBack("ChefServer root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.ChefServer.Config.PrivateKey, certtype: "private_key", svc: "chef-server"},
				{key: a.config.ChefServer.Config.PublicKey, certtype: "public_key", svc: "chef-server"},
			}))
		}
	}

	if a.config.Postgresql.Config.EnableCustomCerts {
		if len(a.config.Postgresql.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(a.config.Postgresql.Config.RootCA)) < 1 {
				errorList.PushBack("Postgresql root_ca is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
			}))
			if !stringutils.SubSlice(a.configPostgresqlIpList, extractIPsFromCertsByIP(a.config.Postgresql.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Postgresql private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(a.configPostgresqlIpList, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range a.config.Postgresql.Config.CertsByIP {
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
			if len(strings.TrimSpace(a.config.Postgresql.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(a.config.Postgresql.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(a.config.Postgresql.Config.PublicKey)) < 1 {
				errorList.PushBack("PostgreSQL root_ca, public_key and private_key are mandatory fields, check if any of them are missing. Otherwise set enable_custom_certs to false.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.Postgresql.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
				{key: a.config.Postgresql.Config.PrivateKey, certtype: "private_key", svc: "postgresql"},
				{key: a.config.Postgresql.Config.PublicKey, certtype: "public_key", svc: "postgresql"},
			}))
		}
	}

	if a.config.Opensearch.Config.EnableCustomCerts {
		if len(a.config.Opensearch.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(a.config.Opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.AdminCert)) < 1 {
				errorList.PushBack("Opensearch root_ca, admin_key or admin_cert is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: a.config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: a.config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
			}))
			if !stringutils.SubSlice(a.configOpensearchIpList, extractIPsFromCertsByIP(a.config.Opensearch.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Opensearch private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(a.configOpensearchIpList, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range a.config.Opensearch.Config.CertsByIP {
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
			if len(strings.TrimSpace(a.config.Opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.AdminCert)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(a.config.Opensearch.Config.PublicKey)) < 1 {
				errorList.PushBack("Opensearch root_ca, admin_key, admin_cert, public_key and private_key are mandatory fields, check if any of them are missing. Otherwise set enable_custom_certs to false.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a.config.Opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: a.config.Opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: a.config.Opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
				{key: a.config.Opensearch.Config.PrivateKey, certtype: "private_key", svc: "opensearch"},
				{key: a.config.Opensearch.Config.PublicKey, certtype: "public_key", svc: "opensearch"},
			}))
		}
	}
	return errorList
}

func (a *awsDeployment) getAwsHAIp() error {
	nodeUtils := NewNodeUtils(NewRemoteCmdExecutorWithoutNodeMap(NewSSHUtil(&SSHConfig{}), writer), command.NewExecExecutor(), writer)
	ConfigIp, err := nodeUtils.getAWSConfigIp()
	if err != nil {
		return err
	}
	a.AWSConfigIp = *ConfigIp
	return nil
}

func (a *awsDeployment) isIamRolePresent() error {
	_, tokenResponseBody, err := a.httpRequestClient.MakeRequestWithHeaders(http.MethodPut, TOKEN_URL, nil, "X-aws-ec2-metadata-token-ttl-seconds", "21600")
	if err != nil {
		return fmt.Errorf("error while getting the token value: %v", err)
	}

	token := string(tokenResponseBody)

	resp, _, err := a.httpRequestClient.MakeRequestWithHeaders(http.MethodGet, METADATA_URL, nil, "X-aws-ec2-metadata-token", token)
	if err != nil {
		return fmt.Errorf("error while getting the response for IAM role: %v", err)
	}
	if resp.StatusCode != 200 {
		return errors.New("Please check if Bastion has attached an IAM Role to it")
	}
	return nil
}
