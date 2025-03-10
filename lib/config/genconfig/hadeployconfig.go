package genconfig

import (
	"fmt"
	"strings"

	"github.com/chef/automate/lib/config"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/chef/automate/lib/toml"
)

const (
	PROMPT_NODE_IP                = "%v. Node IP address"
	AWS_S3                        = "AWS S3"
	MINIO                         = "Minio"
	OBJECT_STORE                  = "Object Store"
	GOOGLE_BUCKET                 = "Google Cloud Storage Bucket"
	FILE_SYSTEM                   = "File System"
	NFS                           = "NFS"
	EFS                           = "EFS"
	ERR_MSG_BUCKET                = "Please provide valid Bucket Name, like: automate-backup"
	ERR_MSG_AWS_ACCESS_KEY_ID     = "Please provide valid AWS Access Key ID of 20 Capital Letters or Numbers"
	ERR_MSG_AWS_ACCESS_KEY_SECRET = "Please provide valid AWS Access Key Secret of 40 alpha numeric characters"
	ERR_MSG_ACCESS_KEY            = "Please provide valid Access Key ID with Alpha numeric characters"
	ERR_MSG_IP                    = "Please provide valid IP Address, like: 192.168.1.1"
	ERR_MSG_BUCKET_ENDPOINT       = "Please provide a valid Bucket Endpoint URL, like: https://my-object-store.example.com or https://35.2.44.4:9200"
	ERR_MSG_DIR_LOC               = "Please provide a valid Directory Location on local linux filesystem, like: /mnt/automate_backups"
)

type HaDeployConfigGen struct {
	Prompt         pmt.Prompt `toml:"-"`
	Config         *config.HaDeployConfig
	HasCustomCerts bool                `toml:"-"`
	FileUtils      fileutils.FileUtils `toml:"-"`
}

func HaDeployConfigFactory(p pmt.Prompt) *HaDeployConfigGen {
	return &HaDeployConfigGen{
		Prompt:    p,
		Config:    &config.HaDeployConfig{},
		FileUtils: fileutils.NewFileSystemUtils(),
	}
}

func (c *HaDeployConfigGen) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c.Config)
}

func (c *HaDeployConfigGen) Prompts() (err error) {
	c.DefaultExistingInfraValues()

	err = c.PromptSsh()
	if err != nil {
		return
	}

	err = c.PromptCustomCerts()
	if err != nil {
		return
	}

	err = c.PromptAutomate()
	if err != nil {
		return
	}

	err = c.PromptChefServer()
	if err != nil {
		return
	}

	isExternalDb, err := c.PromptExternalDb()
	if err != nil {
		return
	}
	if isExternalDb {
		c.SetDefaultValuesForDBNodes()
	} else {
		err = c.PromptOpenSearch()
		if err != nil {
			return
		}

		err = c.PromptPostgresql()
		if err != nil {
			return
		}
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

func (c *HaDeployConfigGen) PromptCustomCerts() (err error) {
	noCustomCerts, err := c.Prompt.Confirm("Use custom certs for one or all service like Automate, Chef Infra Server, PostgreSQL, OpenSearch", "no", "yes")
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

func (c *HaDeployConfigGen) SetDefaultValuesForDBNodes() {
	c.Config.InitOpenSearch().InitConfig()
	c.Config.Opensearch.Config.InstanceCount = "0"
	c.Config.Opensearch.Config.EnableCustomCerts = false

	c.Config.InitPostgresql().InitConfig()
	c.Config.Postgresql.Config.InstanceCount = "0"
	c.Config.Postgresql.Config.EnableCustomCerts = false
}

func (c *HaDeployConfigGen) PromptExternalDb() (hasExternalDb bool, err error) {
	hasExternalDb, err = c.Prompt.Confirm("Use External Databases for PostgresQL and OpenSearch", "yes", "no")
	if err != nil {
		return
	}
	if !hasExternalDb {
		return
	}

	err = c.PromptExternalDbType()
	if err != nil {
		return
	}

	err = c.PromptExternalOpenSearch()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresql()
	if err != nil {
		return
	}
	return
}

func (c *HaDeployConfigGen) PromptExternalDbType() (err error) {
	isAWSManaged, err := c.Prompt.Confirm("Type of External DB", "AWS Managed", "Self Managed")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase()
	if isAWSManaged {
		c.Config.External.Database.Type = "aws"
	} else {
		c.Config.External.Database.Type = "self-managed"
	}
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearch() (err error) {
	err = c.PromptExternalOpenSearchDomainName()
	if err != nil {
		return
	}

	err = c.PromptExternalOpenSearchDomainUrl()
	if err != nil {
		return
	}

	err = c.PromptExternalOpenSearchUser()
	if err != nil {
		return
	}

	err = c.PromptExternalOpenSearchPassword()
	if err != nil {
		return
	}

	err = c.PromptExternalOpenSearchRootCert()
	if err != nil {
		return
	}

	if c.Config.External.Database.Type == "aws" {
		err = c.PromptExternalOpenSearchAwsSnapshotArn()
		if err != nil {
			return
		}

		err = c.PromptExternalOpenSearchAwsSnapshotUserAccessKey()
		if err != nil {
			return
		}

		err = c.PromptExternalOpenSearchAwsSnapshotUserAccessSecret()
		if err != nil {
			return
		}
	}

	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresql() (err error) {
	err = c.PromptExternalPostgresqlInstanceUrl()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresqlSuperUser()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresqlSuperUserPassword()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresqlDbUser()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresqlDbUserPassword()
	if err != nil {
		return
	}

	err = c.PromptExternalPostgresqlRootCert()
	if err != nil {
		return
	}
	return
}

func (c *HaDeployConfigGen) GetExternalPgType() (dbType string) {
	c.Config.InitExternal().InitDatabase()
	if c.Config.External.Database.Type == "aws" {
		dbType = "AWS RDS Posgresql"
	} else {
		dbType = "Self Managed Postgresql"
	}
	return
}

func (c *HaDeployConfigGen) GetExternalOsType() (dbType string) {
	c.Config.InitExternal().InitDatabase()
	if c.Config.External.Database.Type == "aws" {
		dbType = "AWS OpenSearch"
	} else {
		dbType = "Self Managed OpenSearch"
	}
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlInstanceUrl() (err error) {
	pgUrl, err := c.Prompt.InputStringRegexErrMsg(c.GetExternalPgType()+" [Eg: pgdomain.com:5432]", URL_REQUIRED_PORT_REGEX, ERR_MSG_PG_URL)
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.InstanceURL = pgUrl
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlSuperUser() (err error) {
	user, err := c.Prompt.InputStringRequired(c.GetExternalPgType() + " Super Username")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.SuperuserUsername = user
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlSuperUserPassword() (err error) {
	pass, err := c.Prompt.InputPassword(c.GetExternalPgType() + " Super User Password")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.SuperuserPassword = pass
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlDbUser() (err error) {
	user, err := c.Prompt.InputStringRequired(c.GetExternalPgType() + " Db Username")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.DbuserUsername = user
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlDbUserPassword() (err error) {
	pass, err := c.Prompt.InputPassword(c.GetExternalPgType() + " DB User Password")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.DbuserPassword = pass
	return
}

func (c *HaDeployConfigGen) PromptExternalPostgresqlRootCert() (err error) {
	if c.Config.External.Database.Type == "aws" {
		awsDefaultCert := false
		awsDefaultCert, err = c.Prompt.Confirm("Use Default AWS Cert to connect with AWS Managed RDS PostgreSQL URL", "yes", "no")
		if err != nil {
			return
		}
		if awsDefaultCert {
			c.Config.External.Database.PostgreSQL.PostgresqlRootCert = ""
			return
		}
	}
	rootCertFilePath, err := c.Prompt.InputExistingFilePath(c.GetExternalPgType() + " Root Cert File Path")
	if err != nil {
		return
	}

	rootCertFile, err := c.FileUtils.ReadFile(rootCertFilePath)
	if err != nil {
		return
	}

	rootCert := string(rootCertFile)
	rootCert = strings.TrimSpace(rootCert)

	c.Config.InitExternal().InitDatabase().InitPostgresql()
	c.Config.External.Database.PostgreSQL.PostgresqlRootCert = rootCert
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchDomainName() (err error) {
	domainName, err := c.Prompt.InputStringRequired(c.GetExternalOsType() + " Domain Name [Eg: chef-opensearch]")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch()
	c.Config.External.Database.OpenSearch.OpensearchDomainName = domainName
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchDomainUrl() (err error) {
	domainUrl, err := c.Prompt.InputStringRegexErrMsg(c.GetExternalOsType()+" Domain URL [Eg: chef-opensearch.myosdomain.com]", URL_OPTIONAL_PORT_REGEX, ERR_MSG_OS_URL)
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch()
	c.Config.External.Database.OpenSearch.OpensearchDomainURL = domainUrl
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchUser() (err error) {
	user, err := c.Prompt.InputStringRequired(c.GetExternalOsType() + " Username [Eg: admin]")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch()
	c.Config.External.Database.OpenSearch.OpensearchUsername = user
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchPassword() (err error) {
	pass, err := c.Prompt.InputPassword(c.GetExternalOsType() + " User Password")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch()
	c.Config.External.Database.OpenSearch.OpensearchUserPassword = pass
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchRootCert() (err error) {
	if c.Config.External.Database.Type == "aws" {
		awsDefaultCert := false
		awsDefaultCert, err = c.Prompt.Confirm("Use Default AWS Cert to connect with AWS Managed OpenSearch Domain URL", "yes", "no")
		if err != nil {
			return
		}
		if awsDefaultCert {
			c.Config.External.Database.OpenSearch.OpensearchRootCert = ""
			return
		}
	}
	rootCertFilePath, err := c.Prompt.InputExistingFilePath(c.GetExternalOsType() + " Root Cert File Path")
	if err != nil {
		return
	}

	rootCertFile, err := c.FileUtils.ReadFile(rootCertFilePath)
	if err != nil {
		return
	}

	rootCert := string(rootCertFile)
	rootCert = strings.TrimSpace(rootCert)

	c.Config.InitExternal().InitDatabase().InitOpenSearch()
	c.Config.External.Database.OpenSearch.OpensearchRootCert = rootCert
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchAwsSnapshotArn() (err error) {
	arn, err := c.Prompt.InputStringRequired(c.GetExternalOsType() + " Snapshot ARN")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch().InitOpenSearchAws()
	c.Config.External.Database.OpenSearch.Aws.AwsOsSnapshotRoleArn = arn
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchAwsSnapshotUserAccessKey() (err error) {
	key, err := c.Prompt.InputStringRequired(c.GetExternalOsType() + " Snapshot User Access Key")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch().InitOpenSearchAws()
	c.Config.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeyID = key
	return
}

func (c *HaDeployConfigGen) PromptExternalOpenSearchAwsSnapshotUserAccessSecret() (err error) {
	secret, err := c.Prompt.InputStringRequired(c.GetExternalOsType() + " Snapshot User Access Secret")
	if err != nil {
		return
	}
	c.Config.InitExternal().InitDatabase().InitOpenSearch().InitOpenSearchAws()
	c.Config.External.Database.OpenSearch.Aws.OsSnapshotUserAccessKeySecret = secret
	return
}

func (c *HaDeployConfigGen) DefaultAutomateConfigValues() {
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.ConfigFile = "configs/automate.toml"
}

func (c *HaDeployConfigGen) PromptAutomate() (err error) {
	err = c.PromptAutomateFqdn()
	if err != nil {
		return
	}

	err = c.PromptAutomateFqdnRootCa()
	if err != nil {
		return
	}

	err = c.PromptAutomateAdminPassword()
	if err != nil {
		return
	}

	c.DefaultAutomateConfigValues()

	err = c.PromptAutomateNodes()
	if err != nil {
		return
	}

	return
}

func (c *HaDeployConfigGen) PromptAutomateAdminPassword() (err error) {
	adminPass, err := c.Prompt.InputPasswordRegexErrMsg("Automate Dashboard Admin User Password", AUTOMATE_ADMIN_PASSWORD_REGEX,
		ERR_MSG_AUTOMATE_PASS)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig().AdminPassword = adminPass
	return
}

func (c *HaDeployConfigGen) PromptChefServer() (err error) {
	err = c.PromptChefInfraServerFqdn()
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
	return
}

func (c *HaDeployConfigGen) PromptOpenSearch() (err error) {
	err = c.PromptOpenSearchNodes()
	if err != nil {
		return
	}
	return
}

func (c *HaDeployConfigGen) PromptPostgresql() (err error) {
	err = c.PromptPostgresqlNodes()
	if err != nil {
		return
	}
	return
}

func (c *HaDeployConfigGen) PromptChefInfraServerNodes() (err error) {
	chefServerNodeCount, err := c.Prompt.InputIntRange("No. of Chef Infra Server Nodes", 1, 100)
	if err != nil {
		return
	}
	c.Config.InitChefServer().InitConfig()
	c.Config.ChefServer.Config.InstanceCount = fmt.Sprint(chefServerNodeCount)

	hasCustomCerts, hasCustomCertsPerNode, err := c.PromptHaveCustomCerts(CHEF_INFRA_SERVER_NODETYPE)
	if err != nil {
		return err
	}

	c.Config.ChefServer.Config.EnableCustomCerts = hasCustomCerts

	err = c.PromptChefInfraServerPriPubCerts(hasCustomCerts, hasCustomCertsPerNode)
	if err != nil {
		return err
	}

	c.Config.InitExistingInfra().InitConfig()
	for i := 0; i < chefServerNodeCount; i++ {
		err = c.PromptChefInfraServerNodeIpCerts(i, hasCustomCerts, hasCustomCertsPerNode)
		if err != nil {
			return err
		}
	}
	return
}

func (c *HaDeployConfigGen) PromptChefInfraServerPriPubCerts(hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	if hasCustomCerts && !hasCustomCertsPerNode {
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
	if hasCustomCerts && hasCustomCertsPerNode {
		c.Config.ChefServer.Config.InitCertsByIP()
	}
	return
}

func (c *HaDeployConfigGen) PromptChefInfraServerNodeIpCerts(i int, hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	ip, err := c.PromptNodeIp(fmt.Sprintf(PROMPT_NODE_IP, i+1))
	if err != nil {
		return err
	}
	c.Config.ExistingInfra.Config.ChefServerPrivateIps = append(c.Config.ExistingInfra.Config.ChefServerPrivateIps, ip)
	if hasCustomCerts && hasCustomCertsPerNode {
		priKey, err := c.PromptCert(ip, CHEF_INFRA_SERVER_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}

		pubCert, err := c.PromptCert(ip, CHEF_INFRA_SERVER_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}

		nodeCert := config.CertByIP{
			IP:         ip,
			PrivateKey: priKey,
			PublicKey:  pubCert,
		}
		*c.Config.ChefServer.Config.CertsByIP = append(*c.Config.ChefServer.Config.CertsByIP, nodeCert)
	}
	return
}

func (c *HaDeployConfigGen) PromptOpenSearchNodes() (err error) {
	opensearchNodeCount, err := c.Prompt.InputIntRange("No. of OpenSearch Nodes", 3, 100)
	if err != nil {
		return
	}
	c.Config.InitOpenSearch().InitConfig()
	c.Config.Opensearch.Config.InstanceCount = fmt.Sprint(opensearchNodeCount)

	hasCustomCerts, hasCustomCertsPerNode, err := c.PromptHaveCustomCerts(OPENSEARCH_NODETYPE)
	if err != nil {
		return err
	}

	c.Config.Opensearch.Config.EnableCustomCerts = hasCustomCerts

	err = c.PromptOpenSearchPubPriCerts(hasCustomCerts, hasCustomCertsPerNode)
	if err != nil {
		return err
	}

	c.Config.InitExistingInfra().InitConfig()
	for i := 0; i < opensearchNodeCount; i++ {
		err := c.PromptOpenSearchNodeIpCerts(i, hasCustomCerts, hasCustomCertsPerNode)
		if err != nil {
			return err
		}
	}
	return
}

func (c *HaDeployConfigGen) PromptOpenSearchNodeIpCerts(i int, hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	ip, err1 := c.PromptNodeIp(fmt.Sprintf(PROMPT_NODE_IP, i+1))
	if err1 != nil {
		return err1
	}
	c.Config.ExistingInfra.Config.OpensearchPrivateIps = append(c.Config.ExistingInfra.Config.OpensearchPrivateIps, ip)
	if hasCustomCerts && hasCustomCertsPerNode {
		priKey, err := c.PromptCert(ip, OPENSEARCH_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}

		pubCert, err := c.PromptCert(ip, OPENSEARCH_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}

		nodeCert := config.CertByIP{
			IP:         ip,
			PrivateKey: priKey,
			PublicKey:  pubCert,
		}
		*c.Config.Opensearch.Config.CertsByIP = append(*c.Config.Opensearch.Config.CertsByIP, nodeCert)
	}
	return
}

func (haconfiggen *HaDeployConfigGen) PromptOpenSearchPubPriCerts(hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	if hasCustomCerts && !hasCustomCertsPerNode {
		priKey, err := haconfiggen.PromptCert("", OPENSEARCH_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}
		haconfiggen.Config.InitOpenSearch().InitConfig().PrivateKey = priKey

		pubCert, err := haconfiggen.PromptCert("", OPENSEARCH_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}
		haconfiggen.Config.InitOpenSearch().InitConfig().PublicKey = pubCert
	}

	if hasCustomCerts {
		rootCa, err := haconfiggen.PromptCert("", OPENSEARCH_NODETYPE, ROOTCA)
		if err != nil {
			return err
		}
		haconfiggen.Config.Opensearch.Config.RootCA = rootCa
		adminCert, err := haconfiggen.PromptCert("", OPENSEARCH_NODETYPE, ADMIN_CERT)
		if err != nil {
			return err
		}
		haconfiggen.Config.Opensearch.Config.AdminCert = adminCert
		adminKey, err := haconfiggen.PromptCert("", OPENSEARCH_NODETYPE, ADMIN_KEY)
		if err != nil {
			return err
		}
		haconfiggen.Config.Opensearch.Config.AdminKey = adminKey
	}

	if hasCustomCerts && hasCustomCertsPerNode {
		haconfiggen.Config.Opensearch.Config.InitCertsByIP()
	}
	return
}

func (c *HaDeployConfigGen) PromptPostgresqlNodes() (err error) {
	postgresqlNodeCount, err := c.Prompt.InputIntRange("No. of Postgresql Nodes", 3, 100)
	if err != nil {
		return
	}
	c.Config.InitPostgresql().InitConfig()
	c.Config.Postgresql.Config.InstanceCount = fmt.Sprint(postgresqlNodeCount)

	hasCustomCerts, hasCustomCertsPerNode, err := c.PromptHaveCustomCerts(POSTGRESQL_NODETYPE)
	if err != nil {
		return err
	}

	c.Config.Postgresql.Config.EnableCustomCerts = hasCustomCerts

	err = c.PromptPostgresqlPubPriCerts(hasCustomCerts, hasCustomCertsPerNode)
	if err != nil {
		return err
	}

	c.Config.InitExistingInfra().InitConfig()
	for i := 0; i < postgresqlNodeCount; i++ {
		err = c.PromptPostgresqlIpCerts(i, hasCustomCerts, hasCustomCertsPerNode)
		if err != nil {
			return err
		}
	}
	return
}

func (c *HaDeployConfigGen) PromptPostgresqlIpCerts(i int, hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	ip, err1 := c.PromptNodeIp(fmt.Sprintf(PROMPT_NODE_IP, i+1))
	if err1 != nil {
		return err1
	}
	c.Config.ExistingInfra.Config.PostgresqlPrivateIps = append(c.Config.ExistingInfra.Config.PostgresqlPrivateIps, ip)
	if hasCustomCerts && hasCustomCertsPerNode {
		priKey, err := c.PromptCert(ip, POSTGRESQL_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}

		pubCert, err := c.PromptCert(ip, POSTGRESQL_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}

		nodeCert := config.CertByIP{
			IP:         ip,
			PrivateKey: priKey,
			PublicKey:  pubCert,
		}
		*c.Config.Postgresql.Config.CertsByIP = append(*c.Config.Postgresql.Config.CertsByIP, nodeCert)
	}
	return
}

func (c *HaDeployConfigGen) PromptPostgresqlPubPriCerts(hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	if hasCustomCerts && !hasCustomCertsPerNode {
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
	}

	if hasCustomCerts {
		rootCa, err := c.PromptCert("", POSTGRESQL_NODETYPE, ROOTCA)
		if err != nil {
			return err
		}
		c.Config.Postgresql.Config.RootCA = rootCa
	}

	if hasCustomCerts && hasCustomCertsPerNode {
		c.Config.Postgresql.Config.InitCertsByIP()
	}
	return
}

func (c *HaDeployConfigGen) PromptNodeIp(msg string) (ip string, err error) {
	ip, err = c.Prompt.InputStringRegexErrMsg(msg, IP_REGEX, ERR_MSG_IP)
	if err != nil {
		return
	}
	return
}

func (c *HaDeployConfigGen) PromptHaveCustomCerts(nodeType string) (customCerts, customCertsPerNode bool, err error) {
	if c.HasCustomCerts {
		customCerts, err = c.Prompt.Confirm("Use custom certs for "+nodeType+" Nodes", "yes", "no")
		if err != nil {
			return
		}

		if customCerts {
			commonCerts, err1 := c.Prompt.Confirm("Use same Certs for all "+nodeType+" Node", "yes", "no")
			if err1 != nil {
				return commonCerts, customCertsPerNode, err1
			}
			customCertsPerNode = !commonCerts
		}
	}
	return
}
func (c *HaDeployConfigGen) PromptCert(ip, nodeType, certName string) (certVal string, err error) {
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

func (c *HaDeployConfigGen) PromptAutomateNodes() (err error) {
	automateNodeCount, err := c.Prompt.InputIntRange("No. of Automate Nodes", 1, 100)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.InstanceCount = fmt.Sprint(automateNodeCount)

	hasCustomCerts, hasCustomCertsPerNode, err := c.PromptHaveCustomCerts(AUTOMATE_NODETYPE)
	if err != nil {
		return err
	}

	c.Config.Automate.Config.EnableCustomCerts = hasCustomCerts

	err = c.PromptAutomateCerts(hasCustomCerts, hasCustomCertsPerNode)
	if err != nil {
		return err
	}

	c.Config.InitExistingInfra().InitConfig()
	for i := 0; i < automateNodeCount; i++ {
		err = c.PromptAutomateIpCerts(i, hasCustomCerts, hasCustomCertsPerNode)
		if err != nil {
			return err
		}
	}
	return
}

func (c *HaDeployConfigGen) PromptAutomateIpCerts(i int, hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	ip, err1 := c.PromptNodeIp(fmt.Sprintf(PROMPT_NODE_IP, i+1))
	if err1 != nil {
		return err1
	}
	c.Config.ExistingInfra.Config.AutomatePrivateIps = append(c.Config.ExistingInfra.Config.AutomatePrivateIps, ip)
	if hasCustomCerts && hasCustomCertsPerNode {
		priKey, err := c.PromptCert(ip, AUTOMATE_NODETYPE, PRI_KEY)
		if err != nil {
			return err
		}

		pubCert, err := c.PromptCert(ip, AUTOMATE_NODETYPE, PUB_CERT)
		if err != nil {
			return err
		}

		nodeCert := config.CertByIP{
			IP:         ip,
			PrivateKey: priKey,
			PublicKey:  pubCert,
		}
		*c.Config.Automate.Config.CertsByIP = append(*c.Config.Automate.Config.CertsByIP, nodeCert)
	}
	return
}

func (c *HaDeployConfigGen) PromptAutomateCerts(hasCustomCerts, hasCustomCertsPerNode bool) (err error) {
	if hasCustomCerts && !hasCustomCertsPerNode {
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

	if hasCustomCerts && hasCustomCertsPerNode {
		c.Config.Automate.Config.InitCertsByIP()
	}
	return
}

func (c *HaDeployConfigGen) PromptChefInfraServerFqdn() (err error) {
	chefInfraServerFqdn, err := c.Prompt.InputStringRegexErrMsg("Chef Infra Server FQDN [Eg: my-infraserver-domain.com]", FQDN_REGEX, ERR_MSG_INFRA_FQDN)
	if err != nil {
		return
	}
	c.Config.InitChefServer().InitConfig()
	c.Config.ChefServer.Config.ChefServerFqdn = chefInfraServerFqdn
	return
}

func (c *HaDeployConfigGen) PromptChefInfraServerFqdnRootCa() (err error) {
	chefInfraServerFqdnRootCaFilePath, err := c.Prompt.InputExistingFilePath("Chef Infra Server FQDN Root CA File Path [Eg: /home/ubuntu/fqdn2-root-ca.pem]")
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

func (c *HaDeployConfigGen) PromptAutomateFqdn() (err error) {
	automateFqdn, err := c.Prompt.InputStringRegexErrMsg("Automate FQDN [Eg: my-automate-domain.com]", FQDN_REGEX, ERR_MSG_AUTOMATE_FQDN)
	if err != nil {
		return
	}
	c.Config.InitAutomate().InitConfig()
	c.Config.Automate.Config.Fqdn = automateFqdn
	return
}

func (c *HaDeployConfigGen) PromptAutomateFqdnRootCa() (err error) {
	automateFqdnRootCaFilePath, err := c.Prompt.InputExistingFilePath("Automate FQDN Root CA File Path [Eg: /home/ubuntu/fqdn2-root-ca.pem]")
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

func (c *HaDeployConfigGen) PromptSshUser() (sshUser string, err error) {
	sshUser, err = c.Prompt.InputStringRegexErrMsg("SSH User Name", LINUX_USER_REGEX, ERR_MSG_SSH_USER)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitExistingInfra()
	c.Config.Architecture.ExistingInfra.SSHUser = sshUser
	return
}

func (c *HaDeployConfigGen) PromptSshGroup(sshUser string) (err error) {
	sshGroup, err := c.Prompt.InputStringRegexDefaultErrMsg("SSH Group", LINUX_USER_REGEX, sshUser, ERR_MSG_SSH_GROUP)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitExistingInfra()
	c.Config.Architecture.ExistingInfra.SSHGroupName = sshGroup
	return
}

func (c *HaDeployConfigGen) PromptSshPort() (err error) {
	sshPort, err := c.Prompt.InputIntDefaultRange("SSH Port", 22, 0, 65535)
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitExistingInfra()
	c.Config.Architecture.ExistingInfra.SSHPort = fmt.Sprint(sshPort)
	return
}

func (c *HaDeployConfigGen) PromptSshKey() (err error) {
	sshKeyFile, err := c.Prompt.InputExistingFilePath("SSH Key File Path [*.pem downloaded for SSH Key Pair from AWS]")
	if err != nil {
		return
	}
	c.Config.InitArchitecture().InitExistingInfra()
	c.Config.Architecture.ExistingInfra.SSHKeyFile = sshKeyFile
	return
}

func (c *HaDeployConfigGen) PromptSsh() (err error) {
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

func (c *HaDeployConfigGen) DefaultExistingInfraValues() {
	c.Config.InitArchitecture().InitExistingInfra()
	c.Config.Architecture.ExistingInfra.SecretsKeyFile = "/hab/a2_deploy_workspace/secrets.key"
	c.Config.Architecture.ExistingInfra.SecretsStoreFile = "/hab/a2_deploy_workspace/secrets.json"
	c.Config.Architecture.ExistingInfra.Architecture = "existing_nodes"
	c.Config.Architecture.ExistingInfra.WorkspacePath = "/hab/a2_deploy_workspace"
}

func (c *HaDeployConfigGen) PromptBackup() (err error) {
	var isBackupNeeded bool
	if c.Config.External == nil || c.Config.External.Database.Type != "self-managed" {
		isBackupNeeded, err = c.Prompt.Confirm("Configured backup during deployment", "yes", "no")
	}
	c.Config.InitArchitecture().InitExistingInfra().AutomateBasePath = "automate"
	c.Config.InitArchitecture().InitExistingInfra().OpensearchBasePath = "elasticsearch"
	if isBackupNeeded {
		c.Config.InitArchitecture().InitExistingInfra().BackupMount = "/mnt/automate_backups"
		c.Config.InitExternal().InitDatabase()
		backupConfig := ""
		backupOption := ""
		var err1 error
		if c.Config.External != nil && c.Config.External.Database.Type == "aws" {
			_, backupOption, err1 = c.Prompt.Select("Choose backup option", AWS_S3)
			if err1 != nil {
				return err1
			}
		}

		if c.Config.External == nil || c.Config.External.Database.Type != "aws" {
			_, backupOption, err1 = c.Prompt.Select("Choose backup option", AWS_S3, MINIO, OBJECT_STORE, GOOGLE_BUCKET, FILE_SYSTEM, NFS, EFS)
			if err1 != nil {
				return err1
			}
		}

		switch backupOption {
		case AWS_S3, MINIO, OBJECT_STORE, GOOGLE_BUCKET:
			backupConfig = "object_storage"
		case FILE_SYSTEM, NFS, EFS:
			backupConfig = "file_system"
		}
		c.Config.Architecture.ExistingInfra.BackupConfig = backupConfig

		if backupConfig == "object_storage" {
			err1 := c.PromptObjectStorageSettings(backupOption)
			if err1 != nil {
				return err1
			}
		} else if backupConfig == "file_system" {
			backupMountLoc, err1 := c.Prompt.InputStringRegexDefaultErrMsg("Backup Mount Location", DIR_PATH_REGEX, "/mnt/automate_backups", ERR_MSG_DIR_LOC)
			if err1 != nil {
				return err1
			}
			c.Config.Architecture.ExistingInfra.BackupMount = backupMountLoc
		}
	}
	return
}

func (c *HaDeployConfigGen) PromptObjectStorageSettings(backupOption string) (err error) {

	bucketName, err := c.Prompt.InputStringRegexErrMsg("Bucket name", BUCKET_NAME_REGEX, ERR_MSG_BUCKET)
	if err != nil {
		return
	}
	c.Config.InitObjectStorage().InitConfig().BucketName = bucketName
	if backupOption == GOOGLE_BUCKET {
		c.Config.ObjectStorage.Config.Location = "gcs"
		jsonFile, err1 := c.Prompt.InputExistingFilePath("Provide the google service account json file path")
		if err1 != nil {
			return err1
		}
		c.Config.ObjectStorage.Config.GoogleServiceAccountFile = jsonFile

	} else {

		c.Config.ObjectStorage.Config.Location = "s3"
		if backupOption == AWS_S3 {
			accessKey, err1 := c.Prompt.InputStringRegexErrMsg("AWS Access Key ID for bucket", AWS_ACCESS_KEY_ID_REGEX, ERR_MSG_AWS_ACCESS_KEY_ID)
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.AccessKey = accessKey

			secretKey, err1 := c.Prompt.InputStringRegexErrMsg("AWS Access Key Secret for bucket", AWS_ACCESS_KEY_SECRET_REGEX, ERR_MSG_AWS_ACCESS_KEY_SECRET)
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.SecretKey = secretKey
			c.Config.ObjectStorage.Config.Endpoint = "https://s3.amazonaws.com"

			awsRegions := AwsRegionsImpFactory(c.Prompt)
			bucketRegion, err1 := awsRegions.Choose("AWS Region of bucket")
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.Region = bucketRegion
		} else {
			accessKey, err1 := c.Prompt.InputStringRegexErrMsg("Access Key ID for bucket", ACCESS_KEY_ID_REGEX, ERR_MSG_ACCESS_KEY)
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.AccessKey = accessKey

			secretKey, err1 := c.Prompt.InputStringRegexErrMsg("Access Key Secret for bucket", ACCESS_KEY_SECRET_REGEX, ERR_MSG_ACCESS_KEY)
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.SecretKey = secretKey
			bucketEndpoint, err1 := c.Prompt.InputStringRegexErrMsg("Endpoint for bucket", ENDPOINT_URL, ERR_MSG_BUCKET_ENDPOINT)
			if err1 != nil {
				return err1
			}
			c.Config.ObjectStorage.Config.Endpoint = bucketEndpoint
		}
	}
	return
}
