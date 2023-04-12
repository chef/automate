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
	"google.golang.org/protobuf/types/known/wrapperspb"
)

var haTfvarsJsonString = `
{"backup_config_efs":"true","existing_automate_private_ips":["10.1.0.150"],"existing_chef_server_private_ips":["10.1.0.151"],"existing_opensearch_private_ips":["10.1.0.202","10.1.1.201","10.1.2.200"],"existing_postgresql_private_ips":["10.1.0.100","10.1.1.101","10.1.2.102"],"automate_config_file":"/hab/a2_deploy_workspace/configs/automate.toml","automate_fqdn":"A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com","automate_instance_count":1,"chef_server_instance_count":1,"opensearch_instance_count":3,"nfs_mount_path":"/mnt/automate_backups","postgresql_instance_count":3,"postgresql_archive_disk_fs_path":"/mnt/automate_backups/postgresql","habitat_uid_gid":"","ssh_user":"test-user","ssh_port":"22","ssh_key_file":"/home/test-user/keys.pem"}
`
var haAwsAutoTfvarsJsonString = `
{"backup_config_efs":"true","automate_config_file":"/hab/a2_deploy_workspace/configs/automate.toml","automate_fqdn":"A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com","automate_instance_count":1,"chef_server_instance_count":1,"opensearch_instance_count":3,"nfs_mount_path":"/mnt/automate_backups","postgresql_instance_count":3,"postgresql_archive_disk_fs_path":"/mnt/automate_backups/postgresql","habitat_uid_gid":"","ssh_user":"test-user","ssh_port":"22","ssh_key_file":"/home/test-user/keys.pem"}
`

const (
	ip1                 = "127.0.0.1"
	ip2                 = "127.0.0.2"
	ip3                 = "127.0.0.3"
	adminCert           = "it is a admin cert"
	adminKey            = "it is a admin key"
	rootCACert          = "It is a root ca certificates"
	privateKey          = "It is a private key"
	publicKey           = "It is a public key"
	enable_custom_certs = "true"
	Bucket              = "S3bucketName"
	RoleArn             = "Role:arn:..:123"
	AccessKey           = "aRSHRFHIfcsjk"
	SecretKey           = "hduysHtdKHvDrkjKnfdrYk"
	publicKey1          = `-----BEGIN CERTIFICATE-----
MIIDazCCAlOgAwIBAgIJALlhQd3q75U8MA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNDEyMDUzMDU2WhcNMjYwNDExMDUzMDU2WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZub2RlMTCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBANYbOjc3nimYAPNAg8+JaHHXoBHILGRmIRsuqPNq
7saEsHFXBAYtmVJYgJEbzgpbwPpKhPUKbFw16SGTNay6BQcaV8UA6XYIyLe4U8gp
5CLFWFiPTdbFnSPdiEqD6wp0gHrRo4+A4e0TeMljYNGJ3jklbIVVf6nIsiJfiBu4
WhrFteVLwTmQrQJ+y0vdd3gqW/Nlo3ptFFkBadtMZv4jhsy98rKYOw4y7NnnQVA3
48NQImg1my+t7lO63Mw7Z56t3Kd3mGcL4I/dIYBN1oJXI00pBBI+7P651SsJXQLq
Xfm4uqR8ke5o4SYhJLfSUxU5ViDPamL9ddnRLmdIt2uFXTkCAwEAAaMhMB8wHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3DQEBCwUAA4IBAQB4
YilaFYfXpxVo6cPJEmeRKR2c64qnlqlLybkVQoS7FE9c/BiXMG+pTl4t6jYZCFMt
Z4s/dtMNcCtx6mZiq9wv1vZ5BKJnf0iarYUtwDQKDdNGCkFQuTUxaSfzsS6PkE1I
yO6coQrBkH2ZPoCEHTOuOw3orYaZ9JK5HlrUcB4pOLQZnVF0ChphquRRF4M6G5RC
7J4HmyG5XBE6s5mFxG4LwHLl2vM90Ew0LnWySek+Weebej7TUe0cZyFEHXGzitoG
dXk8pICXoG062gZ4upox6aLNUKuv4nW5RoD/pWtawHJUQf5NQk3pW3Sh3j9N03JM
Dzzcl/KEas8qYBi2m3wI
-----END CERTIFICATE-----`

	publicKey2 = `-----BEGIN CERTIFICATE-----
MIIDazCCAlOgAwIBAgIJALlhQd3q75U9MA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNDEyMDUzMDU2WhcNMjYwNDExMDUzMDU2WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZub2RlMjCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBAMqus9mMDEvX2f3US6kcyEq3yYO1DdKj39L/tjLI
lwK2osOppincyIEq9RBfvAhR3xnq3UgfE//eMyPTVOW4Sa+0A7xXTe8OvEhOX0vM
ONDBrhxFxmji+wUZXN7JN+EkkdAi0JxLyQYeT9DjDjUlCsHRM/SOvnUJQtUBc+/u
qgdIHxbp2lafZQpGfHJdBmzs43rsHByrX3sV+N0M+JMbhuFB6TT0kFHEc36PEyWO
xO15HK2WWyEmhBr66UWdl+9f7WRbZC59ol+7aKJGEriSv+s7o7j9fCZykoaOqho2
pR96zikXCGOzRZS09L6QE/RTBI/8Ye38OCGXuFE6GZbXM20CAwEAAaMhMB8wHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3DQEBCwUAA4IBAQAp
Yrai9/zsqCUzP43rIIFaCSmnZhfMvlgY5RBFo6N/X/mXNzvL3ZwetyNKKhDuWNal
jqpsNA4oYYUbpkKP1F5sueCrRY5ogJ/MHLbuH6P3x1g1Z+PLoJ9zfsKBlFJazlPx
dJ1zGhF3kwqSRt9U/yAcz0Rqy3GEHUrscIrtE+p2/wTD4l5esiRe3RHvEXsqaKac
PEMcVyiM0PmlYUTWg2OI0NLBTxe1/seKo1iG3yhARpx/zMMx3hpFnLttD7D6ipmc
Y6T2yAVnO7g7lFhessbjm9JziT/dkCf0Osni9hssYINHK88nFmeh9cEJXalwkY+P
CJGy+SVq+fEwXpcfo9Bf
-----END CERTIFICATE-----`

	publicKey3 = `-----BEGIN CERTIFICATE-----
MIIDazCCAlOgAwIBAgIJALlhQd3q75U+MA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNDEyMDUzMDU2WhcNMjYwNDExMDUzMDU2WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZub2RlMzCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBALhj315O7Skf5P7xRD9U5VZ1Ce8sOB9LZWq8xLg6
Zv/UPLdSDuDuTjBqQwUIxOAFsnNdFiGmfC7eDmnyGpCJD3mfFd14VVCEQjPZ4eiz
zJjlTrUJKKvONrwxiY3Ri34ipw6PF5lOWNflCLL7oEteublV59uN86BLgvbLg0Aj
wC6xL608Uxxn9EpYDM8oYdvIeFqn0fh0gy+R7mzdWeQbDMwd/qCLc6ycWxkIhORW
HaTBTFV6EcjbD+E5HZSLXy4uFMpAi+c3yxpiGKBGwSX6z7p9riQxOM/wSnHWkby6
M6D2hPPM2fuL8niX0UrT48RJ0l+cEPy29WqrdIJl4+iOkssCAwEAAaMhMB8wHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMA0GCSqGSIb3DQEBCwUAA4IBAQAs
u/KGKc6NQniWdfJmVaX9lI9UrKzx3oJKRHeRTVgofEz4XWGyk4VzGjgifNTMvzRa
MnZJVWgGoyYtbalKQPpSAC6ku7nHBFCj+OJOqmX/HRLuDNC9L79HmgAsYrx/jLXP
cKilrv6xT+/8rrPFVa2YizltXgf8JvyXyZyxlPUfRMp3UtlftXDDpAaZFsw6MVNq
jOxF7aGtuGTBSvqRsvJBi/4+eFIZW9r6HpiDFv8YRY3cYNqc8lIfzNZuigBfWAzA
sYTz3dmBICLnvYIPoVDqdkePJuUBPZUwSBHs44r5x+Mte5W0ks+3czpBJCUfha/U
z/kK058r9+2Rb6bwBX/Y
-----END CERTIFICATE-----`
)

var parsedJsonFromTfVarsStubb = &HATfvars{
	SshUser:    "ec2-user",
	SshPort:    "22",
	SshKeyFile: "/home/ec2-user/a2ha-jay-sg.pem",
}

var rootCertContents = "test...........root........cert"
var PrivateKeyContents = "test...........private........key"
var PublicKeyContents = "test...........public........key"

var automateConfigStubb = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			Sys: &shared.V1_System{
				Tls: &shared.TLSCredentials{},
			},
		},
	},
}
var automateConfigKeys = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			FrontendTls: []*shared.FrontendTLSCredential{},
		},
	},
}
var externalopensearchconfig = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			External: &shared.External{
				Opensearch: &shared.External_Opensearch{
					Backup: &shared.External_Opensearch_Backup{
						S3: &shared.External_Opensearch_Backup_S3Settings{},
					},
				},
			},
		},
	},
}
var externalopensearchRoleArn = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			External: &shared.External{
				Opensearch: &shared.External_Opensearch{
					Backup: &shared.External_Opensearch_Backup{
						S3: &shared.External_Opensearch_Backup_S3Settings{
							Settings: &shared.Backups_S3_Opensearch{},
						},
					},
				},
			},
		},
	},
}
var externalopensearchKeys = &dc.AutomateConfig{
	Global: &shared.GlobalConfig{
		V1: &shared.V1{
			External: &shared.External{
				Opensearch: &shared.External_Opensearch{
					Auth: &shared.External_Opensearch_Authentication{
						AwsOs: &shared.External_Opensearch_Authentication_AwsOpensearchAuth{},
					},
				},
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

func Test_getJsonFromTerraformAwsAutoTfVarsFile(t *testing.T) {
	jsonStrings := convTfvarToJson("../../pkg/testfiles/aws.auto.tfvars")
	assert.NotEqual(t, 0, len(strings.TrimSpace(jsonStrings)))
	data, err := getJsonFromTerraformAwsAutoTfVarsFile(jsonStrings)
	assert.NoError(t, err)
	assert.NotEmpty(t, data)
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

func Test_getPrivateKeyFromFE(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Key: PrivateKeyContents,
		},
	}
	output := getPrivateKeyFromFE(map[string]*dc.AutomateConfig{ip1: automateConfigKeys})
	assert.Equal(t, PrivateKeyContents, output)
}

func Test_getPrivateKeyFromFEEmpty(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Key: "",
		},
	}
	output := getPrivateKeyFromFE(map[string]*dc.AutomateConfig{ip1: automateConfigKeys})
	assert.Equal(t, "", output)
}

func Test_getPrivateKeyFromFEEmptyMap(t *testing.T) {
	output := getPrivateKeyFromFE(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func Test_getPublicKeyFromFE(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Cert: PublicKeyContents,
		},
	}
	output := getPublicKeyFromFE(map[string]*dc.AutomateConfig{ip1: automateConfigKeys})
	assert.Equal(t, PublicKeyContents, output)
}

func Test_getPublicKeyFromFEEmpty(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Cert: "",
		},
	}
	output := getPublicKeyFromFE(map[string]*dc.AutomateConfig{ip1: automateConfigKeys})
	assert.Equal(t, "", output)
}

func Test_getPublicKeyKeyFromFEEmptyMap(t *testing.T) {
	output := getPrivateKeyFromFE(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func Test_getPrivateKeyAndPublicKeyFromBE(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Key:  PrivateKeyContents,
			Cert: PublicKeyContents,
		},
	}
	keyOut, certOut := getPrivateKeyAndPublicKeyFromBE(map[string]*ConfigKeys{ip1: {}, ip2: {privateKey: privateKey, publicKey: publicKey}})
	assert.Equal(t, publicKey, certOut)
	assert.Equal(t, privateKey, keyOut)
}

func Test_getPrivateKeyAndPublicKeyFromBEEmpty(t *testing.T) {
	automateConfigKeys.Global.V1.FrontendTls = []*shared.FrontendTLSCredential{
		{
			Key:  "",
			Cert: "",
		},
	}
	keyOut, certOut := getPrivateKeyAndPublicKeyFromBE(map[string]*ConfigKeys{ip1: {}, ip2: {privateKey: "", publicKey: ""}})
	assert.Equal(t, "", certOut)
	assert.Equal(t, "", keyOut)
}

func Test_getPrivateKeyAndPublicKeyFromBEEmptyMap(t *testing.T) {
	keyOut, certOut := getPrivateKeyAndPublicKeyFromBE(map[string]*ConfigKeys{})
	assert.Equal(t, "", certOut)
	assert.Equal(t, "", keyOut)
}

func Test_getA2fqdn(t *testing.T) {
	automateConfigKeys.Global.V1.Fqdn = &wrapperspb.StringValue{Value: fqdn}
	output := getA2fqdn(map[string]*dc.AutomateConfig{fqdn: automateConfigKeys})
	assert.Equal(t, fqdn, output)
}

func Test_getA2fqdnEmpty(t *testing.T) {
	automateConfigKeys.Global.V1.Fqdn = &wrapperspb.StringValue{Value: ""}
	output := getA2fqdn(map[string]*dc.AutomateConfig{fqdn: automateConfigKeys})
	assert.Equal(t, "", output)
}

func Test_getA2fqdnEmptyMap(t *testing.T) {
	output := getA2fqdn(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}
func Test_getS3Bucket(t *testing.T) {
	externalopensearchconfig.Global.V1.External.Opensearch.Backup.S3.Bucket = &wrapperspb.StringValue{Value: Bucket}
	output := getS3Bucket(map[string]*dc.AutomateConfig{Bucket: externalopensearchconfig})
	assert.Equal(t, Bucket, output)
}

func Test_getS3BucketEmpty(t *testing.T) {
	externalopensearchconfig.Global.V1.External.Opensearch.Backup.S3.Bucket = &wrapperspb.StringValue{Value: ""}
	output := getS3Bucket(map[string]*dc.AutomateConfig{Bucket: externalopensearchconfig})
	assert.Equal(t, "", output)
}

func Test_getS3BucketEmptyMap(t *testing.T) {
	output := getS3Bucket(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func Test_getOsRoleArn(t *testing.T) {
	externalopensearchRoleArn.Global.V1.External.Opensearch.Backup.S3.Settings.RoleArn = &wrapperspb.StringValue{Value: RoleArn}
	output := getOsRoleArn(map[string]*dc.AutomateConfig{RoleArn: externalopensearchRoleArn})
	assert.Equal(t, RoleArn, output)
}

func Test_getOsRoleArnEmpty(t *testing.T) {
	externalopensearchRoleArn.Global.V1.External.Opensearch.Backup.S3.Settings.RoleArn = &wrapperspb.StringValue{Value: ""}
	output := getOsRoleArn(map[string]*dc.AutomateConfig{RoleArn: externalopensearchRoleArn})
	assert.Equal(t, "", output)
}

func Test_getOsRoleArnEmptyMap(t *testing.T) {
	output := getOsRoleArn(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func Test_getOsAccessKey(t *testing.T) {
	externalopensearchKeys.Global.V1.External.Opensearch.Auth.AwsOs.AccessKey = &wrapperspb.StringValue{Value: AccessKey}
	output := getOsAccessKey(map[string]*dc.AutomateConfig{AccessKey: externalopensearchKeys})
	assert.Equal(t, AccessKey, output)
}
func Test_getOsAccessKeyEmpty(t *testing.T) {
	externalopensearchKeys.Global.V1.External.Opensearch.Auth.AwsOs.AccessKey = &wrapperspb.StringValue{Value: ""}
	output := getOsAccessKey(map[string]*dc.AutomateConfig{AccessKey: externalopensearchKeys})
	assert.Equal(t, "", output)
}

func Test_getOsAccessKeyEmptyMap(t *testing.T) {
	output := getOsAccessKey(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
}

func Test_getOsSecretKey(t *testing.T) {
	externalopensearchKeys.Global.V1.External.Opensearch.Auth.AwsOs.SecretKey = &wrapperspb.StringValue{Value: SecretKey}
	output := getOsSecretKey(map[string]*dc.AutomateConfig{SecretKey: externalopensearchKeys})
	assert.Equal(t, SecretKey, output)
}

func Test_getOsSecretKeyEmpty(t *testing.T) {
	externalopensearchKeys.Global.V1.External.Opensearch.Auth.AwsOs.SecretKey = &wrapperspb.StringValue{Value: ""}
	output := getOsSecretKey(map[string]*dc.AutomateConfig{SecretKey: externalopensearchKeys})
	assert.Equal(t, "", output)
}

func Test_getOsSecretKeyEmptyMap(t *testing.T) {
	output := getOsSecretKey(map[string]*dc.AutomateConfig{})
	assert.Equal(t, "", output)
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

func TestAwsValidateJsonFromRubyScript(t *testing.T) {
	params := HATfvars{}
	err := json.Unmarshal([]byte(haAwsAutoTfvarsJsonString), &params)
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
	assert.Equal(t, "A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com", params.AutomateFqdn)
}

func Test_getAwsHAConfigFromTFVars(t *testing.T) {
	params := HAAwsAutoTfvars{}
	err := json.Unmarshal([]byte(haAwsAutoTfvarsJsonString), &params)
	assert.NoError(t, err)
	assert.NotEmpty(t, params)
	assert.Equal(t, "true", params.BackupConfigEFS)
	assert.Equal(t, "/home/test-user/keys.pem", params.AwsSshKeyFile)
	assert.Equal(t, "22", params.SshPort)
	assert.Equal(t, 3, params.PostgresqlInstanceCount)
	assert.Equal(t, 1, params.AutomateInstanceCount)
	assert.Equal(t, 3, params.OpensearchInstanceCount)
	assert.Equal(t, 1, params.ChefServerInstanceCount)
	assert.Equal(t, "test-user", params.SshUser)
	assert.Equal(t, "A2-hello-automate-lbs-test.ap-region-1.elb.amazonaws.com", params.AutomateFqdn)

}

func TestGetOsCertsByIp(t *testing.T) {
	type testCaseInfo struct {
		testCaseDescreption string
		osConfigMap         map[string]*ConfigKeys
		ExpectedCertsByIp   []CertByIP
	}
	mockInfra := &AutomteHAInfraDetails{}
	mockInfra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP, ValidIP1, ValidIP2}
	p := NewPullConfigs(mockInfra, &SSHUtilImpl{})

	testCases := []testCaseInfo{
		{
			testCaseDescreption: "Three ips with different certs",
			osConfigMap: map[string]*ConfigKeys{
				ValidIP: {
					rootCA:     rootCACert,
					privateKey: PrivateKeyContents,
					publicKey:  publicKey1,
				},

				ValidIP1: {
					rootCA:     rootCACert,
					privateKey: PrivateKeyContents,
					publicKey:  publicKey2,
				},

				ValidIP2: {
					rootCA:     rootCACert,
					privateKey: PrivateKeyContents,
					publicKey:  publicKey3,
				},
			},
			ExpectedCertsByIp: []CertByIP{
				{
					ValidIP,
					PrivateKeyContents,
					publicKey1,
					"CN=chefnode1,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode2,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode3,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n",
				},

				{
					ValidIP1,
					PrivateKeyContents,
					publicKey2,
					"CN=chefnode1,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode2,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode3,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n",
				},

				{
					ValidIP2,
					PrivateKeyContents,
					publicKey3,
					"CN=chefnode1,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode2,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n  - CN=chefnode3,O=Chef Software Inc,L=Seattle,ST=Washington,C=US\\n",
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescreption, func(t *testing.T) {
			certByIpGet := p.getOsCertsByIp(testCase.osConfigMap)

			for i := 0; i < len(certByIpGet); i++ {
				assert.Equal(t, testCase.ExpectedCertsByIp[i].NodesDn, certByIpGet[i].NodesDn)
			}
		})
	}
}
