package genconfig

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/pmt"
	"github.com/chef/automate/lib/toml"
)

const (
	AWS_MACHINE_TYPE_REGEX         = "^((a1|c1|c3|c4|c5|c5a|c5ad|c5d|c5n|c6a|c6g|c6gd|c6gn|c6i|c6id|c7g|cc2|d2|d3|d3en|dl1|f1|g2|g3|g3s|g4ad|g4dn|g5|g5g|h1|i2|i3|i3en|i4i|im4gn|inf1|is4gen|m1|m2|m3|m4|m5|m5a|m5ad|m5d|m5dn|m5n|m5zn|m6a|m6g|m6gd|m6i|m6id|mac1|mac2|p2|p3|p3dn|p4d|r3|r4|r5|r5a|r5ad|r5b|r5d|r5dn|r5n|r6a|r6g|r6gd|r6i|r6id|t1|t2|t3|t3a|t4g|trn1|u-12tb1|u-3tb1|u-6tb1|u-9tb1|vt1|x1|x1e|x2gd|x2idn|x2iedn|x2iezn|z1d)\\.(10xlarge|112xlarge|12xlarge|16xlarge|18xlarge|24xlarge|2xlarge|32xlarge|3xlarge|48xlarge|4xlarge|56xlarge|6xlarge|8xlarge|9xlarge|large|medium|metal|micro|nano|small|xlarge))$"
	AWS_VOL_TYPE_REGEX             = "^(gp2|gp3|io2|io1|st1|sc1)$"
	IP_REGEX                       = "^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.?\\b){4}$"
	IP_REGEX_SAMPLE                = "10.0.0.0"
	URL_OPTIONAL_PORT_REGEX        = "^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z0-9][a-zA-Z0-9-]{0,61}[a-zA-Z0-9](:{1}[0-9]{1,5})?$"
	URL_OPTIONAL_PORT_REGEX_SAMPLE = "myopensearch.com or 10.0.82.0:9200"
	FQDN_REGEX                     = "^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z0-9][a-zA-Z0-9-]{0,61}[a-zA-Z0-9]$"
	FQDN_REGEX_SAMPLE              = "mydomain.chef.io"
	URL_REQUIRED_PORT_REGEX        = "^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z0-9][a-zA-Z0-9-]{0,61}[a-zA-Z0-9]:{1}[0-9]{1,5}$"
	URL_REQUIRED_PORT_REGEX_SAMPLE = "mydomain.chef.io:5432"
	LINUX_USER_REGEX               = "^[a-z_]([a-z0-9_-]{0,31}|[a-z0-9_-]{0,30})$"
	DIR_PATH_REGEX                 = "^\\/$|(\\/[a-zA-Z_0-9-]+)+$"
	BUCKET_NAME_REGEX              = "^[a-zA-Z0-9_-]+$"
	AWS_ACCESS_KEY_ID_REGEX        = "^[A-Z0-9]{20}$"
	AWS_ACCESS_KEY_SECRET_REGEX    = "^[A-Za-z0-9/+=]{40}$"
	ACCESS_KEY_ID_REGEX            = "^[A-Za-z0-9/+=]+$"
	ACCESS_KEY_SECRET_REGEX        = "^[A-Za-z0-9/+=]+$"
	ENDPOINT_URL                   = "^((http|https)://)[-a-zA-Z0-9@:%._\\+~#?&//=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%._\\+~#?&//=]*)$"
	AUTOMATE_ADMIN_PASSWORD_REGEX  = "^.{8,35}$"
	OPENSEARCH_NODETYPE            = "OpenSearch"
	POSTGRESQL_NODETYPE            = "Postgresql"
	AUTOMATE_NODETYPE              = "Automate"
	CHEF_INFRA_SERVER_NODETYPE     = "Chef Infra Server"
	PUB_CERT                       = "Public Cert"
	PRI_KEY                        = "Private Key"
	ADMIN_CERT                     = "Admin Cert"
	ADMIN_KEY                      = "Admin Key"
	ROOTCA                         = "Root CA"
	TOKEN_URLS                     = "http://169.254.169.254/latest/api/token"
	METADATA_URLS                  = "http://169.254.169.254/latest/meta-data/iam/info"
)

type AwsHaProvisionConfig struct {
	Prompt               pmt.Prompt `toml:"-"`
	Config               *config.HaDeployConfig
	HasCustomCerts       bool                `toml:"-"`
	FileUtils            fileutils.FileUtils `toml:"-"`
	CreateOSSnapShotCred bool
	httpRequestClient    httputils.HTTPClient
}

func AwsHaProvisionConfigFactory(p pmt.Prompt) *AwsHaProvisionConfig {
	log := logger.NewLogrusStandardLogger()
	return &AwsHaProvisionConfig{
		Prompt:            p,
		Config:            &config.HaDeployConfig{},
		FileUtils:         fileutils.NewFileSystemUtils(),
		httpRequestClient: httputils.NewClient(log),
	}
}

func (c *AwsHaProvisionConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c.Config)
}

func (c *AwsHaProvisionConfig) Prompts() (err error) {
	err = c.PromptAwsArchitecture()
	if err != nil {
		return
	}

	err = c.PromptAwsConfig()
	if err != nil {
		return
	}

	err = c.PromptAutomate()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServer()
	if err != nil {
		return
	}

	err = c.PromptDatabases()
	if err != nil {
		return
	}

	err = c.PromptBackup()
	if err != nil {
		return
	}

	err = c.Config.Verify()
	if err != nil {
		fmt.Println("ERRORs in Validation:\n", err)
		return nil
	}

	return
}

func (c *AwsHaProvisionConfig) PromptAwsConfig() (err error) {
	err = c.PromptProfile()
	if err != nil {
		return
	}

	err = c.PromptRegion()
	if err != nil {
		return
	}

	err = c.PromptVpcId()
	if err != nil {
		return
	}

	err = c.PromptCidr()
	if err != nil {
		return
	}

	err = c.PromptSshKeyPairName()
	if err != nil {
		return
	}

	err = c.PromptAmi()
	if err != nil {
		return
	}

	err = c.PromptDeleteOnTermination()
	if err != nil {
		return
	}

	err = c.PromptLoadBalancerAccessLogs()
	if err != nil {
		return
	}

	return
}

func (c *AwsHaProvisionConfig) PromptCidr() (err error) {
	isCidrBlockAddr, err := c.HasCidrBlockAddr()
	if err != nil {
		return
	}

	if !isCidrBlockAddr {
		err1 := c.PromptPrivateSubnet()
		if err1 != nil {
			return err1
		}
		err1 = c.PromptPublicSubnet()
		if err1 != nil {
			return err1
		}
	}
	return
}

func (c *AwsHaProvisionConfig) PromptProfile() (err error) {
	useIAMRole, err := c.useIAMRole()
	if err != nil {
		return
	}

	if !useIAMRole {
		profile, err := c.Prompt.InputStringDefault("AWS Profile", "default")
		if err != nil {
			return err
		}
		c.Config.InitAws().InitConfigAwsSettings().Profile = profile
	}
	return
}

func (c *AwsHaProvisionConfig) PromptRegion() (err error) {
	awsRegions := AwsRegionsImpFactory(c.Prompt)
	awsRegion, err := awsRegions.Choose("AWS Region")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().Region = awsRegion
	return
}

func (c *AwsHaProvisionConfig) PromptVpcId() (err error) {
	vpcId, err := c.Prompt.InputStringRequired("AWS VPC ID")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AwsVpcID = vpcId
	return
}

func (c *AwsHaProvisionConfig) HasCidrBlockAddr() (isCiderBlock bool, err error) {
	isCiderBlock, err = c.Prompt.Confirm("Do you want to use AWS CIDR Block", "yes", "no")
	if err != nil {
		return
	}
	if isCiderBlock {
		err = c.PromptCidrBlockAddr()
		if err != nil {
			return
		}
	}
	return
}

func (c *AwsHaProvisionConfig) useIAMRole() (isIAMUserAvailable bool, err error) {

	_, tokenResponseBody, err := c.httpRequestClient.MakeRequestWithHeaders(http.MethodPut, TOKEN_URLS, nil, "X-aws-ec2-metadata-token-ttl-seconds", "21600")
	if err != nil {
		return
	}

	token := string(tokenResponseBody)
	fmt.Println("TOKEN: ", token)

	resp, dataByte, err := c.httpRequestClient.MakeRequestWithHeaders(http.MethodGet, METADATA_URLS, nil, "X-aws-ec2-metadata-token", token)
	if err != nil {
		return
	}

	if resp.StatusCode != 200 {
		isIAMUserAvailable = false
		return
	}

	iamRole := getIAMRoleName(dataByte)

	fmt.Println("iamRole: ", iamRole)

	isIAMUserAvailable, err = c.Prompt.Confirm("Found an IAM role ("+iamRole+") attached to this machine. Do you want to continue with IAM role", "yes", "no")
	return
}

func getIAMRoleName(respByte []byte) (IAMRoleName string) {

	var result map[string]interface{}
	err := json.Unmarshal(respByte, &result)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	instanceProfileArn, ok := result["InstanceProfileArn"].(string)
	if !ok {
		fmt.Println("InstanceProfileArn not found or not a string")
		return
	}
	IAMRoleName = strings.Split(instanceProfileArn, "/")[1]
	return
}

func (c *AwsHaProvisionConfig) PromptCidrBlockAddr() (err error) {
	ciderBlockAddr, err := c.Prompt.InputStringRegex("AWS CIDR Block Address", IP_REGEX, IP_REGEX_SAMPLE)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AwsCidrBlockAddr = ciderBlockAddr
	return
}

func (c *AwsHaProvisionConfig) PromptPrivateSubnet() (err error) {
	privateSubnet1, err := c.Prompt.InputStringRequired("AWS Private Subnet 1 [Eg: subnet-e556d512]")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PrivateCustomSubnets = []string{privateSubnet1}

	privateSubnet2, err := c.Prompt.InputStringRequired("AWS Private Subnet 2 [Eg: subnet-e556d514]")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PrivateCustomSubnets = append(c.Config.InitAws().InitConfigAwsSettings().PrivateCustomSubnets, privateSubnet2)

	privateSubnet3, err := c.Prompt.InputStringRequired("AWS Private Subnet 3 [Eg: subnet-e556d513]")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PrivateCustomSubnets = append(c.Config.InitAws().InitConfigAwsSettings().PrivateCustomSubnets, privateSubnet3)

	return
}

func (c *AwsHaProvisionConfig) PromptPublicSubnet() (err error) {
	hasPublicSubnets, err := c.Prompt.Confirm("Do you have Public Subnets", "yes", "no")
	if err != nil {
		return
	}
	if hasPublicSubnets {
		publicSubnet1, err1 := c.Prompt.InputStringRequired("AWS Public Subnet 1:")
		if err1 != nil {
			return err1
		}
		c.Config.InitAws().InitConfigAwsSettings().PublicCustomSubnets = []string{publicSubnet1}

		publicSubnet2, err1 := c.Prompt.InputStringRequired("AWS Public Subnet 2:")
		if err1 != nil {
			return err1
		}
		c.Config.InitAws().InitConfigAwsSettings().PublicCustomSubnets = append(c.Config.InitAws().InitConfigAwsSettings().PublicCustomSubnets, publicSubnet2)

		publicSubnet3, err1 := c.Prompt.InputStringRequired("AWS Public Subnet 3:")
		if err1 != nil {
			return err1
		}
		c.Config.InitAws().InitConfigAwsSettings().PublicCustomSubnets = append(c.Config.InitAws().InitConfigAwsSettings().PublicCustomSubnets, publicSubnet3)
	}
	return
}

func (c *AwsHaProvisionConfig) PromptSshKeyPairName() (err error) {
	sshKeyPairName, err := c.Prompt.InputStringRequired("AWS SSH Key Pair Name")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().SSHKeyPairName = sshKeyPairName
	return
}

func (c *AwsHaProvisionConfig) PromptAmi() (err error) {
	amiId, err := c.Prompt.InputStringRequired("AWS AMI ID")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AmiID = amiId
	return
}

func (c *AwsHaProvisionConfig) PromptDeleteOnTermination() (err error) {
	noDelOnTerm, err := c.Prompt.Confirm("Delete on termination should be", "off", "on")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().DeleteOnTermination = !noDelOnTerm
	return
}

func (c *AwsHaProvisionConfig) PromptDatabases() (err error) {
	isAwsManagedDb, err := c.PromptIsAwsManaged()
	if err != nil {
		return
	}

	if isAwsManagedDb {
		err := c.PromptAwsManaged()
		if err != nil {
			return err
		}
		c.SetDefaultValuesForDBNodes()
	} else {
		err := c.PromptChefManaged()
		if err != nil {
			return err
		}
	}

	return
}

func (c *AwsHaProvisionConfig) PromptIsAwsManaged() (isAwsManagedDb bool, err error) {
	isAwsManagedDb, err = c.Prompt.Confirm("Do you want to use AWS Managed Databases", "yes", "no")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().SetupManagedServices = isAwsManagedDb
	return
}

func (c *AwsHaProvisionConfig) PromptAwsManaged() (err error) {
	err = c.PromptAwsManagedOpenSearch()
	if err != nil {
		return
	}

	err = c.PromptAwsManagedPostgresql()
	if err != nil {
		return
	}

	return
}

func (c *AwsHaProvisionConfig) PromptAwsManagedPostgresql() (err error) {
	err = c.PromptPgUrl()
	if err != nil {
		return
	}

	err = c.PromptPgSuperUserName()
	if err != nil {
		return
	}

	err = c.PromptPgSuperUserPassword()
	if err != nil {
		return
	}

	err = c.PromptPgDbUserName()
	if err != nil {
		return
	}

	err = c.PromptPgDbUserPassword()
	if err != nil {
		return
	}

	err = c.PromptPgCert()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) PromptPgUrl() (err error) {
	pgUrl, err := c.Prompt.InputStringRegex("AWS Managed RDS PostgreSQL URL:<port>", URL_REQUIRED_PORT_REGEX, URL_REQUIRED_PORT_REGEX_SAMPLE)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsInstanceURL = pgUrl
	return
}

func (c *AwsHaProvisionConfig) PromptPgSuperUserName() (err error) {
	user, err := c.Prompt.InputStringRequired("AWS Managed RDS PostgreSQL Super User Name")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsSuperuserUsername = user
	return
}

func (c *AwsHaProvisionConfig) PromptPgSuperUserPassword() (err error) {
	pass, err := c.Prompt.InputPassword("AWS Managed RDS PostgreSQL Super User Password")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsSuperuserPassword = pass
	return
}

func (c *AwsHaProvisionConfig) PromptPgDbUserName() (err error) {
	user, err := c.Prompt.InputStringRequired("AWS Managed RDS PostgreSQL DB User Name")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsDbuserUsername = user
	return
}

func (c *AwsHaProvisionConfig) PromptPgDbUserPassword() (err error) {
	pass, err := c.Prompt.InputPassword("AWS Managed RDS PostgreSQL DB User Password")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsDbuserPassword = pass
	return
}

func (c *AwsHaProvisionConfig) PromptPgCert() (err error) {
	awsDefaultCert, err := c.Prompt.Confirm("Do you want to use Default AWS Cert to connect with AWS Managed RDS PosgreSQL URL", "yes", "no")
	if err != nil {
		return
	}
	if awsDefaultCert {
		c.Config.InitAws().InitConfigAwsSettings().ManagedRdsCertificate = ""
		return
	}
	certFilePath, err := c.Prompt.InputExistingFilePath("AWS Managed RDS PostgreSQL Certificate File Path")
	if err != nil {
		return
	}
	certFile, err := c.FileUtils.ReadFile(certFilePath)
	if err != nil {
		return
	}
	cert := strings.TrimSpace(string(certFile))
	c.Config.InitAws().InitConfigAwsSettings().ManagedRdsCertificate = cert
	return
}

func (c *AwsHaProvisionConfig) PromptAwsManagedOpenSearch() (err error) {
	err = c.PromptOsDomainName()
	if err != nil {
		return
	}

	err = c.PromptOsDomainUrl()
	if err != nil {
		return
	}

	err = c.PromptOsUserName()
	if err != nil {
		return
	}

	err = c.PromptOsUserPassword()
	if err != nil {
		return
	}

	err = c.PromptOsCert()
	if err != nil {
		return
	}

	err = c.CreateOSSnapShotCredentials()
	if err != nil {
		return
	}

	err = c.PromptOsSnapshotRoleArn()
	if err != nil {
		return
	}

	err = c.PromptOsSnapshotUserAccessKeyId()
	if err != nil {
		return
	}

	err = c.PromptOsSnapshotUserAccessKeySecret()
	if err != nil {
		return
	}

	return
}

func (c *AwsHaProvisionConfig) PromptOsDomainName() (err error) {
	osDomainName, err := c.Prompt.InputStringRequired("AWS Managed OpenSearch Domain Name")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchDomainName = osDomainName
	return
}

func (c *AwsHaProvisionConfig) PromptOsDomainUrl() (err error) {
	osDomainUrl, err := c.Prompt.InputStringRegex("AWS Managed OpenSearch Domain URL", URL_OPTIONAL_PORT_REGEX, URL_OPTIONAL_PORT_REGEX_SAMPLE)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchDomainURL = osDomainUrl
	return
}

func (c *AwsHaProvisionConfig) PromptOsUserName() (err error) {
	user, err := c.Prompt.InputStringRequired("AWS Managed OpenSearch User Name")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchUsername = user
	return
}

func (c *AwsHaProvisionConfig) PromptOsUserPassword() (err error) {
	pass, err := c.Prompt.InputPassword("AWS Managed OpenSearch User Password")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchUserPassword = pass
	return
}

func (c *AwsHaProvisionConfig) PromptOsCert() (err error) {
	awsDefaultCert, err := c.Prompt.Confirm("Do you want to use Default AWS Cert to connect with AWS Managed OpenSearch Domain URL", "yes", "no")
	if err != nil {
		return
	}
	if awsDefaultCert {
		c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchCertificate = ""
		return
	}
	certFilePath, err := c.Prompt.InputExistingFilePath("AWS Managed OpenSearch Certificate File Path")
	if err != nil {
		return
	}
	certFile, err := c.FileUtils.ReadFile(certFilePath)
	if err != nil {
		return
	}
	cert := strings.TrimSpace(string(certFile))
	c.Config.InitAws().InitConfigAwsSettings().ManagedOpensearchCertificate = cert
	return
}

func (c *AwsHaProvisionConfig) CreateOSSnapShotCredentials() (err error) {
	CreateOSSnapShotCred, err := c.Prompt.Confirm("Create new snapshot role ARN and user credential", "yes", "no")
	if err != nil {
		return
	}
	c.CreateOSSnapShotCred = CreateOSSnapShotCred
	return
}

func (c *AwsHaProvisionConfig) PromptOsSnapshotRoleArn() (err error) {
	if c.CreateOSSnapShotCred {
		c.Config.InitAws().InitConfigAwsSettings().AwsOsSnapshotRoleArn = ""
	} else {
		arn, err := c.Prompt.InputStringRequired("AWS Managed OpenSearch Snapshot Role ARN")
		if err != nil {
			return err
		}
		c.Config.InitAws().InitConfigAwsSettings().AwsOsSnapshotRoleArn = arn
	}
	return
}

func (c *AwsHaProvisionConfig) PromptOsSnapshotUserAccessKeyId() (err error) {
	if c.CreateOSSnapShotCred {
		c.Config.InitAws().InitConfigAwsSettings().OsSnapshotUserAccessKeyID = ""
	} else {
		keyId, err := c.Prompt.InputStringRequired("AWS Managed OpenSearch Snapshot User Access Key ID")
		if err != nil {
			return err
		}
		c.Config.InitAws().InitConfigAwsSettings().OsSnapshotUserAccessKeyID = keyId
	}
	return
}

func (c *AwsHaProvisionConfig) PromptOsSnapshotUserAccessKeySecret() (err error) {
	if c.CreateOSSnapShotCred {
		c.Config.InitAws().InitConfigAwsSettings().OsSnapshotUserAccessKeySecret = ""
	} else {
		secret, err := c.Prompt.InputStringRequired("AWS Managed OpenSearch Snapshot User Access Key Secret")
		if err != nil {
			return err
		}
		c.Config.InitAws().InitConfigAwsSettings().OsSnapshotUserAccessKeySecret = secret
	}
	return
}

func (c *AwsHaProvisionConfig) PromptChefManaged() (err error) {
	err = c.PromptPgChefManaged()
	if err != nil {
		return
	}

	err = c.PromptOsChefManaged()
	if err != nil {
		return
	}

	return
}

func (c *AwsHaProvisionConfig) PromptAutomate() (err error) {
	err = c.PromptAutomateFqdn()
	if err != nil {
		return
	}

	err = c.PromptAutomateLoadBalancerCertArn()
	if err != nil {
		return
	}

	err = c.PromptAutomateFqdnRootCa()
	if err != nil {
		return
	}

	c.DefaultAutomateConfigValues()

	err = c.PromptAutomateAdminPassword()
	if err != nil {
		return
	}

	err = c.PromptAutomateNodes()
	if err != nil {
		return
	}

	err = c.PromptAutomateInstanceType()
	if err != nil {
		return
	}

	err = c.PromptAutomateVolSize()
	if err != nil {
		return
	}

	err = c.PromptAutomateVolType()
	if err != nil {
		return
	}

	err = c.PromptAutomateVolIops()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServer() (err error) {
	err = c.PromptChefInfraServerFqdn()
	if err != nil {
		return
	}

	err = c.PromptChefInfrServerLoadBalancerCertArn()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerFqdnRootCa()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerNodes()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerInstanceType()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerVolSize()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerVolType()
	if err != nil {
		return
	}

	err = c.PromptChefInfraServerVolIops()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) PromptPgChefManaged() (err error) {
	err = c.PromptPostgresqlNodes()
	if err != nil {
		return
	}

	err = c.PromptPostgresqlInstanceType()
	if err != nil {
		return
	}

	err = c.PromptPostgresqlVolSize()
	if err != nil {
		return
	}

	err = c.PromptPostgresqlVolType()
	if err != nil {
		return
	}

	err = c.PromptPostgresqlVolIops()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) PromptOsChefManaged() (err error) {
	err = c.PromptOpenSearchNodes()
	if err != nil {
		return
	}

	err = c.PromptOpenSearchInstanceType()
	if err != nil {
		return
	}

	err = c.PromptOpenSearchVolSize()
	if err != nil {
		return
	}

	err = c.PromptOpenSearchVolType()
	if err != nil {
		return
	}

	err = c.PromptOpenSearchVolIops()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) DefaultAutomateConfigValues() {
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.ConfigFile = "configs/automate.toml"
}

func (c *AwsHaProvisionConfig) PromptAutomateFqdn() (err error) {
	automateFqdn, err := c.Prompt.InputStringRegex("Automate FQDN", FQDN_REGEX, FQDN_REGEX_SAMPLE)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.Fqdn = automateFqdn
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateFqdnRootCa() (err error) {
	automateFqdnRootCaFilePath, err := c.Prompt.InputExistingFilePath("Automate FQDN Root CA File Path")
	if err != nil {
		return
	}

	automateFqdnRootCaFile, err := c.FileUtils.ReadFile(automateFqdnRootCaFilePath)
	if err != nil {
		return
	}

	automateFqdnRootCa := string(automateFqdnRootCaFile)
	automateFqdnRootCa = strings.TrimSpace(automateFqdnRootCa)

	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.FqdnRootCA = automateFqdnRootCa
	return
}

func (c *AwsHaProvisionConfig) PromptHaveCustomCerts(nodeType string) (customCerts bool, err error) {
	if c.HasCustomCerts {
		customCerts, err = c.Prompt.Confirm("Do you have custom certs for "+nodeType+" Nodes", "yes", "no")
		if err != nil {
			return
		}
	}
	return
}

func (c *AwsHaProvisionConfig) PromptCert(ip, nodeType, certName string) (certVal string, err error) {
	msgCert := "Provide " + certName + " file path for " + nodeType
	if ip != "" {
		msgCert += " Node on IP: " + ip
	} else {
		msgCert += " Service"
	}
	certFilePath, err := c.Prompt.InputExistingFilePath(msgCert)
	if err != nil {
		return
	}
	certFile, err := c.FileUtils.ReadFile(certFilePath)
	if err != nil {
		return
	}
	certVal = string(certFile)
	certVal = strings.TrimSpace(certVal)
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateNodes() (err error) {
	automateNodeCount, err := c.Prompt.InputIntRange("No. of Automate Nodes", 1, 100)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.InstanceCount = fmt.Sprint(automateNodeCount)

	hasCustomCerts, err := c.PromptHaveCustomCerts("Automate")
	if err != nil {
		return err
	}

	c.Config.InitAutomate().InitConfig().EnableCustomCerts = hasCustomCerts

	if hasCustomCerts {
		priKey, err := c.PromptCert("", AUTOMATE_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}
		c.Config.Automate.Config.PrivateKey = priKey

		pubCert, err := c.PromptCert("", AUTOMATE_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}
		c.Config.Automate.Config.PublicKey = pubCert
	}
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateAdminPassword() (err error) {
	adminPass, err := c.Prompt.InputPasswordRegex("Automate Dashboard Admin User Password", AUTOMATE_ADMIN_PASSWORD_REGEX)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig().AdminPassword = adminPass
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateInstanceType() (err error) {
	instanceType, err := c.Prompt.InputStringRegexDefault("AWS Instance type for Automate", AWS_MACHINE_TYPE_REGEX, "m5.large")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AutomateServerInstanceType = instanceType
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerFqdn() (err error) {
	chefInfraServerFqdn, err := c.Prompt.InputStringRegex("Chef Infra Server FQDN", FQDN_REGEX, FQDN_REGEX_SAMPLE)
	if err != nil {
		return
	}
	c.Config.InitChefServer().InitConfig()
	c.Config.ChefServer.Config.ChefServerFqdn = chefInfraServerFqdn
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerFqdnRootCa() (err error) {
	chefInfraServerFqdnRootCaFilePath, err := c.Prompt.InputExistingFilePath("Chef Infra Server FQDN Root CA File Path")
	if err != nil {
		return
	}

	chefInfraServerFqdnRootCaFile, err := c.FileUtils.ReadFile(chefInfraServerFqdnRootCaFilePath)
	if err != nil {
		return
	}

	chefInfraServerFqdnRootCa := string(chefInfraServerFqdnRootCaFile)

	chefInfraServerFqdnRootCa = strings.TrimSpace(chefInfraServerFqdnRootCa)

	c.Config.InitChefServer().InitConfig()
	c.Config.ChefServer.Config.FqdnRootCA = chefInfraServerFqdnRootCa
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerNodes() (err error) {
	chefServerNodeCount, err := c.Prompt.InputIntRange("No. of Chef Infra Server Nodes", 1, 100)
	if err != nil {
		return
	}
	c.Config.InitChefServer().InitConfig()
	c.Config.ChefServer.Config.InstanceCount = fmt.Sprint(chefServerNodeCount)

	hasCustomCerts, err := c.PromptHaveCustomCerts("Chef Infra Server")
	if err != nil {
		return err
	}

	c.Config.InitChefServer().InitConfig().EnableCustomCerts = hasCustomCerts

	if hasCustomCerts {
		priKey, err := c.PromptCert("", CHEF_INFRA_SERVER_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}
		c.Config.ChefServer.Config.PrivateKey = priKey

		pubCert, err := c.PromptCert("", CHEF_INFRA_SERVER_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}
		c.Config.ChefServer.Config.PublicKey = pubCert
	}

	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerInstanceType() (err error) {
	instanceType, err := c.Prompt.InputStringRegexDefault("AWS Instance type for Chef Infra Server", AWS_MACHINE_TYPE_REGEX, "m5.large")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ChefServerInstanceType = instanceType
	return
}

func (c *AwsHaProvisionConfig) PromptPostgresqlInstanceType() (err error) {
	instanceType, err := c.Prompt.InputStringRegexDefault("AWS Instance type for PostgreSQL", AWS_MACHINE_TYPE_REGEX, "m5.large")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PostgresqlServerInstanceType = instanceType
	return
}

func (c *AwsHaProvisionConfig) PromptOpenSearchInstanceType() (err error) {
	instanceType, err := c.Prompt.InputStringRegexDefault("AWS Instance type for OpenSearch", AWS_MACHINE_TYPE_REGEX, "m5.large")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().OpensearchServerInstanceType = instanceType
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateVolSize() (err error) {
	size, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume Size for Automate", 200, 200, 16384)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AutomateEbsVolumeSize = fmt.Sprint(size)
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerVolSize() (err error) {
	size, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume Size for Chef Infra Server", 200, 200, 16384)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ChefEbsVolumeSize = fmt.Sprint(size)
	return
}

func (c *AwsHaProvisionConfig) PromptPostgresqlNodes() (err error) {
	postgresqlNodeCount, err := c.Prompt.InputIntRange("No. of Postgresql Nodes", 3, 100)
	if err != nil {
		return
	}
	c.Config.InitPostgresql().InitConfig()
	c.Config.Postgresql.Config.InstanceCount = fmt.Sprint(postgresqlNodeCount)

	hasCustomCerts, err := c.PromptHaveCustomCerts("Postgresql")
	if err != nil {
		return err
	}

	c.Config.InitPostgresql().InitConfig().EnableCustomCerts = hasCustomCerts

	if hasCustomCerts {
		priKey, err := c.PromptCert("", POSTGRESQL_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}
		c.Config.Postgresql.Config.PrivateKey = priKey

		pubCert, err := c.PromptCert("", POSTGRESQL_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}
		c.Config.Postgresql.Config.PublicKey = pubCert

		rootCa, err := c.PromptCert("", POSTGRESQL_NODETYPE, ROOTCA)
		if err != nil {
			return err
		}
		c.Config.Postgresql.Config.RootCA = rootCa
	}
	return
}

func (c *AwsHaProvisionConfig) PromptPostgresqlVolSize() (err error) {
	size, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume Size for PostgreSQL", 200, 200, 16384)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PostgresqlEbsVolumeSize = fmt.Sprint(size)
	return
}

func (c *AwsHaProvisionConfig) PromptOpenSearchNodes() (err error) {
	opensearchNodeCount, err := c.Prompt.InputIntRange("No. of OpenSearch Nodes", 3, 100)
	if err != nil {
		return
	}
	c.Config.InitOpenSearch().InitConfig()
	c.Config.Opensearch.Config.InstanceCount = fmt.Sprint(opensearchNodeCount)

	hasCustomCerts, err := c.PromptHaveCustomCerts("OpenSearch")
	if err != nil {
		return err
	}

	c.Config.InitOpenSearch().InitConfig().EnableCustomCerts = hasCustomCerts

	if hasCustomCerts {
		priKey, err := c.PromptCert("", OPENSEARCH_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}
		c.Config.Opensearch.Config.PrivateKey = priKey

		pubCert, err := c.PromptCert("", OPENSEARCH_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}
		c.Config.Opensearch.Config.PublicKey = pubCert

		rootCa, err := c.PromptCert("", OPENSEARCH_NODETYPE, ROOTCA)
		if err != nil {
			return err
		}
		c.Config.Opensearch.Config.RootCA = rootCa
		adminCert, err := c.PromptCert("", OPENSEARCH_NODETYPE, ADMIN_CERT)
		if err != nil {
			return err
		}
		c.Config.Opensearch.Config.AdminCert = adminCert
		adminKey, err := c.PromptCert("", OPENSEARCH_NODETYPE, ADMIN_KEY)
		if err != nil {
			return err
		}
		c.Config.Opensearch.Config.AdminKey = adminKey
	}
	return
}

func (c *AwsHaProvisionConfig) PromptOpenSearchVolSize() (err error) {
	size, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume Size for OpenSearch", 200, 200, 16384)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().OpensearchEbsVolumeSize = fmt.Sprint(size)
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateVolIops() (err error) {
	iops, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume IOPS for Automate", 100, 100, 16000)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AutomateEbsVolumeIops = fmt.Sprint(iops)
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerVolIops() (err error) {
	iops, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume IOPS for Chef Infra Server", 100, 100, 16000)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ChefEbsVolumeIops = fmt.Sprint(iops)
	return
}

func (c *AwsHaProvisionConfig) PromptPostgresqlVolIops() (err error) {
	iops, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume IOPS for PostgreSQL", 100, 100, 16000)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PostgresqlEbsVolumeIops = fmt.Sprint(iops)
	return
}

func (c *AwsHaProvisionConfig) PromptOpenSearchVolIops() (err error) {
	iops, err := c.Prompt.InputIntDefaultRange("AWS EBS Volume IOPS for OpenSearch", 100, 100, 16000)
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().OpensearchEbsVolumeIops = fmt.Sprint(iops)
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateVolType() (err error) {
	volType, err := c.Prompt.InputStringRegexDefault("AWS EBS Volume Type for Automate", AWS_VOL_TYPE_REGEX, "gp3")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AutomateEbsVolumeType = volType
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfraServerVolType() (err error) {
	volType, err := c.Prompt.InputStringRegexDefault("AWS EBS Volume IOPS for Chef Infra Server", AWS_VOL_TYPE_REGEX, "gp3")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ChefEbsVolumeType = volType
	return
}

func (c *AwsHaProvisionConfig) PromptPostgresqlVolType() (err error) {
	volType, err := c.Prompt.InputStringRegexDefault("AWS EBS Volume IOPS for PostgreSQL", AWS_VOL_TYPE_REGEX, "gp3")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().PostgresqlEbsVolumeType = volType
	return
}

func (c *AwsHaProvisionConfig) PromptOpenSearchVolType() (err error) {
	volType, err := c.Prompt.InputStringRegexDefault("AWS EBS Volume IOPS for OpenSearch", AWS_VOL_TYPE_REGEX, "gp3")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().OpensearchEbsVolumeType = volType
	return
}

func (c *AwsHaProvisionConfig) PromptAutomateLoadBalancerCertArn() (err error) {
	certArn, err := c.Prompt.InputStringRequired("AWS Load Balancer Cert ARN for Automate")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().AutomateLbCertificateArn = certArn
	return
}

func (c *AwsHaProvisionConfig) PromptChefInfrServerLoadBalancerCertArn() (err error) {
	certArn, err := c.Prompt.InputStringRequired("AWS Load Balancer Cert ARN for Chef Infra Server")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().ChefServerLbCertificateArn = certArn
	return
}

func (c *AwsHaProvisionConfig) PromptLoadBalancerAccessLogs() (err error) {
	enableAccessLogs, err := c.Prompt.Confirm("Do you want to Enable Access Logs on AWS Load Balancer", "yes", "no")
	if err != nil {
		return
	}
	c.Config.InitAws().InitConfigAwsSettings().LbAccessLogs = fmt.Sprint(enableAccessLogs)
	return
}

func (c *AwsHaProvisionConfig) DefaultAwsValues() {
	c.Config.InitArchitecture().InitAws()
	c.Config.Architecture.Aws.SecretsKeyFile = "/hab/a2_deploy_workspace/secrets.key"
	c.Config.Architecture.Aws.SecretsStoreFile = "/hab/a2_deploy_workspace/secrets.json"
	c.Config.Architecture.Aws.Architecture = "aws"
	c.Config.Architecture.Aws.WorkspacePath = "/hab/a2_deploy_workspace"
}

func (c *AwsHaProvisionConfig) PromptAwsArchitecture() (err error) {
	c.DefaultAwsValues()

	err = c.PromptSsh()
	if err != nil {
		return
	}

	err = c.PromptCustomCerts()
	if err != nil {
		return
	}

	return
}

func (c *AwsHaProvisionConfig) PromptSsh() (err error) {
	sshUser, err := c.PromptSshUser()
	if err != nil {
		return
	}

	err = c.PromptSshGroup(sshUser)
	if err != nil {
		return
	}

	err = c.PromptSshPort()
	if err != nil {
		return
	}

	err = c.PromptSshKey()
	if err != nil {
		return
	}
	return
}

func (c *AwsHaProvisionConfig) PromptSshUser() (sshUser string, err error) {
	sshUser, err = c.Prompt.InputStringRegex("SSH User Name", LINUX_USER_REGEX)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitAws()
	c.Config.Architecture.Aws.SSHUser = sshUser
	return
}

func (c *AwsHaProvisionConfig) PromptSshGroup(sshUser string) (err error) {
	sshGroup, err := c.Prompt.InputStringRegexDefault("SSH Group", LINUX_USER_REGEX, sshUser)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitAws()
	c.Config.Architecture.Aws.SSHGroupName = sshGroup
	return
}

func (c *AwsHaProvisionConfig) PromptSshPort() (err error) {
	sshPort, err := c.Prompt.InputIntDefaultRange("SSH Port", 22, 0, 65535)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitAws()
	c.Config.Architecture.Aws.SSHPort = fmt.Sprint(sshPort)
	return
}

func (c *AwsHaProvisionConfig) PromptSshKey() (err error) {
	sshKeyFile, err := c.Prompt.InputExistingFilePath("SSH Private Key File Path [*.pem downloaded for SSH Key Pair from AWS]")
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitAws()
	c.Config.Architecture.Aws.SSHKeyFile = sshKeyFile
	return
}

func (c *AwsHaProvisionConfig) PromptCustomCerts() (err error) {
	noCustomCerts, err := c.Prompt.Confirm("Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, OpenSearch", "no", "yes")
	if err != nil {
		return
	}
	c.HasCustomCerts = !noCustomCerts
	c.Config.InitAutomate().InitConfig().EnableCustomCerts = c.HasCustomCerts
	c.Config.InitChefServer().InitConfig().EnableCustomCerts = c.HasCustomCerts
	c.Config.InitPostgresql().InitConfig().EnableCustomCerts = c.HasCustomCerts
	c.Config.InitOpenSearch().InitConfig().EnableCustomCerts = c.HasCustomCerts
	return
}

func (c *AwsHaProvisionConfig) PromptBackup() (err error) {
	isBackupNeeded, err := c.Prompt.Confirm("Backup need to be configured during deployment", "yes", "no")
	if isBackupNeeded {
		c.Config.InitArchitecture().InitAws().BackupMount = "/mnt/automate_backups"
		backupConfig := "s3"

		if !c.Config.InitAws().InitConfigAwsSettings().SetupManagedServices {
			_, backupOption, err1 := c.Prompt.Select("Which backup option will you use", "AWS S3", "EFS")
			if err1 != nil {
				return err1
			}
			switch backupOption {
			case "AWS S3":
				backupConfig = "s3"
			case "EFS":
				backupConfig = "efs"
			}
		}

		c.Config.InitArchitecture().InitAws().BackupConfig = backupConfig

		if backupConfig == "s3" {

			s3Bucket, err1 := c.Prompt.InputStringRequired("AWS S3 Bucket Name")
			if err1 != nil {
				return err1
			}
			c.Config.InitArchitecture().InitAws().S3BucketName = s3Bucket
		}

	}
	return
}

func (c *AwsHaProvisionConfig) SetDefaultValuesForDBNodes() {
	c.Config.InitOpenSearch().InitConfig()
	c.Config.Opensearch.Config.InstanceCount = "0"
	c.Config.Opensearch.Config.EnableCustomCerts = false

	c.Config.InitPostgresql().InitConfig()
	c.Config.Postgresql.Config.InstanceCount = "0"
	c.Config.Postgresql.Config.EnableCustomCerts = false
}
