package main

import (
	"encoding/json"
	"reflect"
	"strconv"
	"strings"
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	shared "github.com/chef/automate/api/config/shared"
	"github.com/stretchr/testify/assert"
)

var haTfvarsJsonString = `
{"backup_config_efs":"true","existing_automate_private_ips":["10.1.0.150"],"existing_chef_server_private_ips":["10.1.0.151"],"existing_opensearch_private_ips":["10.1.0.202","10.1.1.201","10.1.2.200"],"existing_postgresql_private_ips":["10.1.0.100","10.1.1.101","10.1.2.102"],"automate_config_file":"/hab/a2_deploy_workspace/configs/automate.toml","automate_fqdn":"A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com","automate_instance_count":1,"chef_server_instance_count":1,"opensearch_instance_count":3,"nfs_mount_path":"/mnt/automate_backups","postgresql_instance_count":3,"postgresql_archive_disk_fs_path":"/mnt/automate_backups/postgresql","habitat_uid_gid":"","ssh_user":"test-user","ssh_port":"22","ssh_key_file":"/home/test-user/keys.pem"}
`

const (
	ip1        = "127.0.0.1"
	ip2        = "127.0.0.2"
	ip3        = "127.0.0.3"
	adminCert  = "it is a admin cert"
	adminKey   = "it is a admin key"
	rootCACert = "It is a root ca certificates"
)

var parsedJsonFromTfVarsStubb = &HATfvars{
	SshUser:    "ec2-user",
	SshPort:    "22",
	SshKeyFile: "/home/ec2-user/a2ha-jay-sg.pem",
}
var rootCertContents = "test...........root........cert"
var automateConfigStubb = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			Sys: &shared.V1_System{
				Tls: &shared.TLSCredentials{},
			},
		},
	},
}

var automateEmptySysConfigStubb = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{},
	},
}

func TestGetJsonFromTerraformTfVarsFile(t *testing.T) {
	jsonStrings := convTfvarToJson("../../pkg/testfiles/terraform.tfvars")
	assert.NotEqual(t, 0, len(strings.TrimSpace(jsonStrings)))
	data, err := getJsonFromTerraformTfVarsFile(jsonStrings)
	assert.NoError(t, err)
	assert.NotEmpty(t, data)
	assert.NotEmpty(t, data.AutomateCertsByIp)
	assert.NotEmpty(t, data.ChefServerCertsByIp)
	assert.NotEmpty(t, data.OpensearchCertsByIp)
	assert.NotEmpty(t, data.PostgresqlCertsByIp)
}

func TestGetA2ORCSRootCATLSEmpty(t *testing.T) {
	output := getA2ORCSRootCA(map[string]*dc.AutomateConfig{ip1: automateEmptySysConfigStubb})
	assert.Equal(t, "", output)
}

func TestGetA2ORCSRootCARootCertEmpty(t *testing.T) {
	output := getA2ORCSRootCA(map[string]*dc.AutomateConfig{ip1: automateConfigStubb})
	assert.Equal(t, "", output)
}
func TestGetA2ORCSRootCA(t *testing.T) {
	automateConfigStubb.Global.V1.Sys.Tls.RootCertContents = rootCertContents
	output := getA2ORCSRootCA(map[string]*dc.AutomateConfig{ip1: automateConfigStubb})
	assert.Equal(t, rootCertContents, output)
}

func TestGetA2ORCSRootCAEmptyMap(t *testing.T) {
	output := getA2ORCSRootCA(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func TestGetOSAdminCertAndAdminKeyAdminKey(t *testing.T) {
	certOut, keyOut := getOSAdminCertAndAdminKey(map[string]*ConfigKeys{ip1: {}, ip2: {adminCert: adminCert, adminKey: adminKey}})
	assert.Equal(t, adminCert, certOut)
	assert.Equal(t, adminKey, keyOut)
}

func TestGetOSAdminCertAndAdminKeyEmptyMap(t *testing.T) {
	certOut, keyOut := getOSAdminCertAndAdminKey(map[string]*ConfigKeys{})
	assert.Equal(t, "", certOut)
	assert.Equal(t, "", keyOut)
}

func TestGetOSAdminCertAndAdminKeyAdminKeyEmpty(t *testing.T) {
	certOut, keyOut := getOSAdminCertAndAdminKey(map[string]*ConfigKeys{ip1: {}, ip2: {adminCert: adminCert}})
	assert.Equal(t, adminCert, certOut)
	assert.Equal(t, "", keyOut)
}

func TestGetOSAdminCertAndAdminKeyAdminCertEmpty(t *testing.T) {
	certOut, keyOut := getOSAdminCertAndAdminKey(map[string]*ConfigKeys{ip1: {}, ip2: {adminKey: adminKey}})
	assert.Equal(t, "", certOut)
	assert.Equal(t, adminKey, keyOut)
}

func TestGetOSORPGRootCA(t *testing.T) {
	out := getOSORPGRootCA(map[string]*ConfigKeys{ip1: {}, ip2: {}, ip3: {rootCA: rootCACert}})
	assert.Equal(t, rootCACert, out)
}

func TestGetOSORPGRootCAEmpty(t *testing.T) {
	out := getOSORPGRootCA(map[string]*ConfigKeys{ip1: {}, ip2: {}, ip3: {rootCA: ""}})
	assert.Equal(t, "", out)
}

func TestValidateJsonFromRubyScript(t *testing.T) {
	params := HATfvars{}
	err := json.Unmarshal([]byte(haTfvarsJsonString), &params)
	assert.NoError(t, err)
	assert.NotEmpty(t, params)
	assert.Equal(t, "true", params.BackupConfigEFS)
	assert.Equal(t, "/home/test-user/keys.pem", params.SshKeyFile)
	assert.Equal(t, "22", params.SshPort)
	assert.Equal(t, 3, params.PostgresqlInstanceCount)
	assert.Equal(t, 1, params.AutomateInstanceCount)
	assert.Equal(t, 3, params.OpensearchInstanceCount)
	assert.Equal(t, 1, params.ChefServerInstanceCount)
	assert.Equal(t, "test-user", params.SshUser)
	assert.Equal(t, []string{"10.1.0.150"}, params.ExistingAutomatePrivateIps)
	assert.Equal(t, []string{"10.1.0.151"}, params.ExistingChefServerPrivateIps)
	assert.Equal(t, []string{"10.1.0.202", "10.1.1.201", "10.1.2.200"}, params.ExistingOpensearchPrivateIps)
	assert.Equal(t, []string{"10.1.0.100", "10.1.1.101", "10.1.2.102"}, params.ExistingPostgresqlPrivateIps)
	assert.Equal(t, "A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com", params.AutomateFqdn)
}

func TestGetHAConfigFromTFVars(t *testing.T) {
	params := HATfvars{}
	err := json.Unmarshal([]byte(haTfvarsJsonString), &params)
	assert.NoError(t, err)
	assert.NotEmpty(t, params)
	config, err := getExistingHAConfigFromTFVars(&params)
	assert.NoError(t, err)
	assert.NotEmpty(t, config)
	assert.Equal(t, "file_system", config.Architecture.ConfigInitials.BackupConfig)
	assert.Equal(t, params.SshKeyFile, config.Architecture.ConfigInitials.SSHKeyFile)
	assert.Equal(t, params.SshPort, config.Architecture.ConfigInitials.SSHPort)
	assert.Equal(t, params.SshUser, config.Architecture.ConfigInitials.SSHUser)
	assert.Equal(t, strconv.Itoa(params.PostgresqlInstanceCount), config.Postgresql.Config.InstanceCount)
	assert.Equal(t, strconv.Itoa(params.AutomateInstanceCount), config.Automate.Config.InstanceCount)
	assert.Equal(t, strconv.Itoa(params.OpensearchInstanceCount), config.Opensearch.Config.InstanceCount)
	assert.Equal(t, strconv.Itoa(params.ChefServerInstanceCount), config.ChefServer.Config.InstanceCount)
	assert.True(t, reflect.DeepEqual(params.ExistingAutomatePrivateIps, config.ExistingInfra.Config.AutomatePrivateIps))
	assert.True(t, reflect.DeepEqual(params.ExistingChefServerPrivateIps, config.ExistingInfra.Config.ChefServerPrivateIps))
	assert.True(t, reflect.DeepEqual(params.ExistingOpensearchPrivateIps, config.ExistingInfra.Config.OpensearchPrivateIps))
	assert.True(t, reflect.DeepEqual(params.ExistingPostgresqlPrivateIps, config.ExistingInfra.Config.PostgresqlPrivateIps))
	assert.Equal(t, params.AutomateFqdn, config.Automate.Config.Fqdn)
}
