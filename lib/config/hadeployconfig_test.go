package config

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParse(t *testing.T) {
	type args struct {
		configFile string
	}
	tests := []struct {
		name    string
		args    args
		want    *HaDeployConfig
		wantErr bool
		err     error
	}{
		{
			name: "Parse AWS Config",
			args: args{configFile: "./testdata/HaAws.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					Aws: &ConfigInitials{
						SSHUser:                     "",
						SSHGroupName:                "",
						SSHKeyFile:                  "",
						SSHPort:                     "",
						SecretsKeyFile:              "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile:            "/hab/a2_deploy_workspace/secrets.json",
						SudoPassword:                "",
						LoggingMonitoringManagement: "",
						Architecture:                "aws",
						WorkspacePath:               "/hab/a2_deploy_workspace",
						BackupMount:                 "/mnt/automate_backups",
						BackupConfig:                "",
						S3BucketName:                "",
						HabitatUIDGid:               "",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword: "",
						Fqdn:          "",
						ConfigFile:    "configs/automate.toml",
						TeamsPort:     "",

						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},

				ChefServer: &ChefServerSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: false,
					},
				},
				Opensearch: &OpensearchSettings{
					Config: &ConfigOpensearchSettings{
						AdminCert:         "",
						AdminKey:          "",
						AdminDn:           "",
						NodesDn:           "",
						InstanceCount:     "",
						EnableCustomCerts: false,
						PrivateKey:        "",
						PublicKey:         "",
						CertsByIP:         nil,
					},
				},
				Postgresql: &PostgresqlSettings{
					Config: &ConfigSettings{
						RootCA:            "",
						InstanceCount:     "",
						EnableCustomCerts: false,
						PrivateKey:        "",
						PublicKey:         "",
						CertsByIP:         nil,
					},
				},
				Aws: &AwsSettings{
					Config: &ConfigAwsSettings{
						Profile:                      "",
						Region:                       "",
						AwsVpcID:                     "",
						AwsCidrBlockAddr:             "",
						PrivateCustomSubnets:         []string{},
						PublicCustomSubnets:          []string{},
						SSHKeyPairName:               "",
						SetupManagedServices:         false,
						AmiID:                        "",
						DeleteOnTermination:          true,
						AutomateServerInstanceType:   "",
						ChefServerInstanceType:       "",
						OpensearchServerInstanceType: "",
						PostgresqlServerInstanceType: "",
						AutomateLbCertificateArn:     "",
						ChefServerLbCertificateArn:   "",
						ChefEbsVolumeIops:            "",
						ChefEbsVolumeSize:            "",
						ChefEbsVolumeType:            "",
						OpensearchEbsVolumeIops:      "",
						OpensearchEbsVolumeSize:      "",
						OpensearchEbsVolumeType:      "",
						PostgresqlEbsVolumeIops:      "",
						PostgresqlEbsVolumeSize:      "",
						PostgresqlEbsVolumeType:      "",
						AutomateEbsVolumeIops:        "",
						AutomateEbsVolumeSize:        "",
						AutomateEbsVolumeType:        "",
						AmiFilterName:                "",
						AmiFilterVirtType:            "",
						AmiFilterOwner:               "",
						LbAccessLogs:                 "false",
						XContact:                     "",
						XDept:                        "",
						XProject:                     "",
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
		{
			name: "Parse AWS Managed Config",
			args: args{configFile: "./testdata/HaAwsManaged.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					Aws: &ConfigInitials{
						SSHUser:                     "ubuntu",
						SSHGroupName:                "",
						SSHKeyFile:                  "~/.ssh/central.pem",
						SSHPort:                     "22",
						SecretsKeyFile:              "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile:            "/hab/a2_deploy_workspace/secrets.json",
						SudoPassword:                "",
						LoggingMonitoringManagement: "",
						Architecture:                "aws",
						WorkspacePath:               "/hab/a2_deploy_workspace",
						BackupMount:                 "/mnt/automate_backups",
						BackupConfig:                "s3",
						S3BucketName:                "automate-test",
						HabitatUIDGid:               "",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword:     "123456789",
						Fqdn:              "",
						ConfigFile:        "configs/automate.toml",
						InstanceCount:     "2",
						EnableCustomCerts: true,
						RootCA: `-----BEGIN CERTIFICATE-----
MIIEDzCCAvegAwIBAgIBADANBgkqhkiG9w0BAQUFADBoMQswCQYDVQQGEwJVUzEl
MCMGA1UEChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMp
U3RhcmZpZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDQw
NjI5MTczOTE2WhcNMzQwNjI5MTczOTE2WjBoMQswCQYDVQQGEwJVUzElMCMGA1UE
ChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMpU3RhcmZp
ZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwggEgMA0GCSqGSIb3
DQEBAQUAA4IBDQAwggEIAoIBAQC3Msj+6XGmBIWtDBFk385N78gDGIc/oav7PKaf
8MOh2tTYbitTkPskpD6E8J7oX+zlJ0T1KKY/e97gKvDIr1MvnsoFAZMej2YcOadN
+lq2cwQlZut3f+dZxkqZJRRU6ybH838Z1TBwj6+wRir/resp7defqgSHo9T5iaU0
X9tDkYI22WY8sbi5gv2cOj4QyDvvBmVmepsZGD3/cVE8MC5fvj13c7JdBmzDI1aa
K4UmkhynArPkPw2vCHmCuDY96pzTNbO8acr1zJ3o/WSNF4Azbl5KXZnJHoe0nRrA
1W4TNSNe35tfPe/W93bC6j67eA0cQmdrBNj41tpvi/JEoAGrAgEDo4HFMIHCMB0G
A1UdDgQWBBS/X7fRzt0fhvRbVazc1xDCDqmI5zCBkgYDVR0jBIGKMIGHgBS/X7fR
zt0fhvRbVazc1xDCDqmI56FspGowaDELMAkGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8fF5Q=
-----END CERTIFICATE-----`,
						PrivateKey: `-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCyJbvhtNLZrgz7
aa1W1gIGFrwtI5h23S99A8VZlRySlbvJpNpJsQZpwBqW9HtUSsErL8u5dakgFn6l
SopXzmyzxgbuLSnAmmicNrsFI/O5BkZtacvebOjBpfm0LnPsZIfhqnlqvn8nM3a/
npzXIEa3HENUghVHOyR1nY8dvFgBHElfMbIGnLUkBZaHoQcXonG8nAw89IzoiBFo
vC2z+HikVhkt8NB1pD+7ZVTqS/oPYO5yoHLBFc7V8X8DyS9MoPyoFBCYu7V+/d2j
pMvFSVBdUi3HgdK+wO5pD0JtgWNWWfdP6XpiL5VmvwEeOx8yJ6pPIrTPZtcFinTj
s04t/R1BAgMBAAECggEATFvmM3Gpawqyn9UFKpJina+DCyoVwOU/5KsIHUTP0XkC
3ASgWDPb1tozy36RmmjMcbFz9lOboZhiwoC32bkuWMRJ1i9flSHaMxM2iJaHckhh
SaaubvFptw9of7XllG7m23CRyJJMuEXT4YCgI4m/Jd+kcIWtjzGniA53+LvxUM/S
+pR2j5cc7yHYcsc4tz842UelVQJWnMwjHwYDsuZC4ssRq1dffq7ifgfhcrxcwAXg
MMBEtmJsTeaBYjbiM4qdVTwc8BpX9lafgUi3asapR0X2u2k1hOs5fS0qdEf5IarN
FqwQDj5Xurcs0xvUoTPC7eEqFMh1SRVw8mS0hZG0EQKBgQDZ9GHhIPjDd3NwT38R
hHYd3i1EKUtJgoOlqN/WUSlcFBUKxEsQTF+5pZipPYFUjb8+DwzwQMmeSXAs9A+Z
PCFQDO1EHlE1Hv6A0Fv7RPTt3lnkZpo7SdSf/VqxKVCLOIkeU9tEMr0d/8mJ8hbj
lDIUmfZtWIo5eZnM6fQF8rM8LQKBgQDRPoIf1d5M6eMDz6Ss0dPF/UkhEhsXUQgh
d0puiyKuk0e2v6q8sESKV4CnGDFd08XLMcSnOw3SS/neLhZ7lixjtewUoDAVwxlB
Nos3W6FBIgyQRFp5SBjuHhKjPiK+BPlGsc7+39I12aGxE0r5w7Pp4X+4Z7hDF8ID
qlZtDkIN5QKBgQCe8aIjnHjtiwHraH3hF3lP5MOcDoUx8XTx7Up3L6760EZcGLQp
CZlReFrxKMJVGB3cMvubhZPC1AlzLvTlKb2ddB/fakCMfbLZ25kIj8wSX/GsJ8rX
68qcdhWaVue+75bHQB4KCPpzkyK1b4+TnXI8Jd9Y9JWwvmYT0pU7dTeSbQKBgQC1
ko6MXaQoDhWG6xq1NOeWOXLKFdIYa6Kol8GpJ2eTIg7rEGtyjWsMuV3UofPEvc43
wxopG9+ki3VqTYgI+onOhME2LMNNPx2dL12jTgoiYQ+R6R6xe9TWXJZDvdmcFujR
Zd5/4W2ieRYMePdowWBQJfQU6zxETEt5rsiMngDH2QKBgHB8r2LQvvQX4L0w2wY+
OJZOGKk4AMXIFM/J0qY60DLnw0B4RuzSXcKw2EGYRJihbGJAI8+KczU7LpVlOsw5
FzQabRrE33NPYWFv0bqzas9/p9mPwFPzhq13mkrLOHwf71T63OEalaOh9WzaE2Tx
hTp1M8sBDwrDLxCEk0X3NEL+
-----END PRIVATE KEY-----`,
						PublicKey: `-----BEGIN CERTIFICATE-----
MIIDgzCCAmugAwIBAgIJAPMNo6eG0UBgMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNTIyMTEzOTIwWhcNMjYwNTIxMTEzOTIwWjBlMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEzARBgNVBAMMCmNoZWZjbGllbnQwggEiMA0GCSqGSIb3
DQEBAQUAA4IBDwAwggEKAoIBAQCyJbvhtNLZrgz7aa1W1gIGFrwtI5h23S99A8VZ
lRySlbvJpNpJsQZpwBqW9HtUSsErL8u5dakgFn6lSopXzmyzxgbuLSnAmmicNrsF
I/O5BkZtacvebOjBpfm0LnPsZIfhqnlqvn8nM3a/npzXIEa3HENUghVHOyR1nY8d
vFgBHElfMbIGnLUkBZaHoQcXonG8nAw89IzoiBFovC2z+HikVhkt8NB1pD+7ZVTq
S/oPYO5yoHLBFc7V8X8DyS9MoPyoFBCYu7V+/d2jpMvFSVBdUi3HgdK+wO5pD0Jt
gWNWWfdP6XpiL5VmvwEeOx8yJ6pPIrTPZtcFinTjs04t/R1BAgMBAAGjODA2MB0G
A1UdJQQWMBQGCCsGAQUFBwMCBggrBgEFBQcDATAVBgNVHREEDjAMggpjaGVmY2xp
ZW50MA0GCSqGSIb3DQEBCwUAA4IBAQAOHPBbbNCFCQ+Fg5YSvgXCglNUXfKoeCla
Ljruho1re6mJQmeW0H5OTEL7CssEHM6yzR6rlVlVnnu0+RAFlb6vTXo8eK4HaXS2
A+OY7oP7UpMcxj3fxuWWgg1lnW/8a9Z+9JNSm9M59MpU17whvq/i8EJppECK4pD/
Z9J12XYepWxARbVUcgWyHFIjnSoNpFwgrm1xybnyJ2WnjRRjpq6JvkP5+eklESEm
Z4wjUCokbRJ8gbC7tuVgQk6DNM123j9djBm+A+5WlbvHUrD5trfbkp8kUJY4jaJF
csKeX402wz9P7XM5eGsToNpAZq41Q7mFzz14DfqFNttaCMHMYi4k
-----END CERTIFICATE-----`,
					},
				},

				ChefServer: &ChefServerSettings{
					Config: &ConfigSettings{
						InstanceCount:     "2",
						EnableCustomCerts: true,
						PrivateKey: `-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDQlkc2SQkhzEbD
BluJDr/9UvP4cObMbYf7r3Yy9vxds5AikR9gK0tkamAEK9e9uPMi2xSIvDPxNkwe
w2XlWmMxOHsUpUPt2gaaT2DEueiTae+TnVcLtFBIYuo3D9udDW2XAwf0rnjDWifV
rbOsr7++hr3eqcg1k46+f+KUkHwtaYG7noMAq/vwAGkN0hN+Pfa8ILZhXgtoSrLk
5vJ5KavKmD0lFMSzg5AHTT8UNfarUGdH7bCUeZGwZ6MHGdFtUDBPvJKdSgxShoV+
03DmGx4TdbCYaS0bSVWAOxHWfJ3oh3tizjp8n8u/REqSZQpsWx0us8kjf9JICBYS
afjxMBiZAgMBAAECggEAH07/P2G1EjKkSG+y256wKKkD372qlvK134xVtI6oELR8
JQQdboTxGxBwew/NoTeanxe9PEzriwA4asGvkL6BdwjgSJgJ2zDHnu/dOYoiI8ZZ
5JFQWHxnNIZMW5lwwGEcmvckgZCSpdfpdMRO3NPAdyuoYjyfxZLxcRym+N+7E+Gl
3izIlRNz4fHfdsH6rttYMJpn/T2MMyJ3BRZmGc8JPraDzHCMoMnVv2bAhJTpbglr
dFUgzE+eIa/b0ExE/sd3+WVG2hLqrrphwlFczmPlb/7nPlnDL42tVBmZnlexJm5x
pk1GePV2OmlT98rW9ItX0cwVxz15LBpXLIIXG9F7pQKBgQDw5DvUlIsqreJ61jQH
Xd5QRVKmjTkninL4oA5+0YxlzkU3kDhMTvNlmmACcLqOmJ4uOTB107auVnv/XEo2
nYtWMglx6YtdO0q2z8w2ywpnoI9PfIZtAUKNqLBK1BcD5/c9lRE05o1LFylcvL/Y
P9NZYhKkWYGOt0aFIbjNphyW1wKBgQDdq1ySbIst64pw16DssYkvA681Lr8Sbp++
3F+d1aXIKkoOYPxmdA+/lwv/47LWeBB4QDItyBAxx74pesSkEy+BbdkDNSSnMEsS
JW9mCn7gqbKwZeX9TWbdcqXtjXz1XrQZxEslVGOQJPRt7ClGfSVA+NjYtjv7iqCI
xcVzseiODwKBgQCItW5DCX4lXYN/pNroJ1yIf58VSGZcS1VORj+Tt0aPbE2Z5+4b
WF8HlWHRYLpvPKvgnbIj3F/7drduR6kSb7xo0YLMs/bUlVakgy9pFTe1cciDGq+L
Y0Cq9kX+YXkiTV3iBBw8wm8DY4SkzbWueyJtwpvDy8wb+2U5HtcrVo85BwKBgQC9
+Sx++LNXCXQ+PS5Xa9esCTZRF9z9CP2y7t6rP/yyTTvmksv9ah5NDkBkb1pHX+KN
jEb04W6vmwWoOuTn0OF3xRKlIxhkiIjt5lNQWlJebFENyGaQ7ZLo2mbF7epXx3AG
XSohte8WC/XHdwvwszQIOLxvDc7eRvJKBWSxQJmTlQKBgDf7Cx8/ym9TzCIfWx3+
AfCtltTglZAWgt0KjaOlT94lFmv+rJrDbp+rGSp4zvTW+U1/Xxt5+YdZGpMD8OND
0Wy6xd38bTp7jOzYdxTebg+KC+Gum0asXZK9+ra6vBRYsqXX7sET7rEsFMrJiata
dXO0rPR4kQZSuTqBSIxAf4Jb
-----END PRIVATE KEY-----`,
						PublicKey: `-----BEGIN CERTIFICATE-----
MIIDgzCCAmugAwIBAgIJAPMNo6eG0UBiMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNTIyMTEzOTIxWhcNMjYwNTIxMTEzOTIxWjBlMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEzARBgNVBAMMCmNoZWZjbGllbnQwggEiMA0GCSqGSIb3
DQEBAQUAA4IBDwAwggEKAoIBAQDQlkc2SQkhzEbDBluJDr/9UvP4cObMbYf7r3Yy
9vxds5AikR9gK0tkamAEK9e9uPMi2xSIvDPxNkwew2XlWmMxOHsUpUPt2gaaT2DE
ueiTae+TnVcLtFBIYuo3D9udDW2XAwf0rnjDWifVrbOsr7++hr3eqcg1k46+f+KU
kHwtaYG7noMAq/vwAGkN0hN+Pfa8ILZhXgtoSrLk5vJ5KavKmD0lFMSzg5AHTT8U
NfarUGdH7bCUeZGwZ6MHGdFtUDBPvJKdSgxShoV+03DmGx4TdbCYaS0bSVWAOxHW
fJ3oh3tizjp8n8u/REqSZQpsWx0us8kjf9JICBYSafjxMBiZAgMBAAGjODA2MB0G
A1UdJQQWMBQGCCsGAQUFBwMCBggrBgEFBQcDATAVBgNVHREEDjAMggpjaGVmY2xp
ZW50MA0GCSqGSIb3DQEBCwUAA4IBAQABwtwMsJlif2kAa9cYdSwaG0Zn5gHlqX0u
7GW8RFcTwkU+5ZpzbdcgX5XtKq7a7LDdGTYZpvmiKAvsSY4L5vDV0tfZRsJSIyOS
4kWG8P4LdtsXld2Px9V/fLGxTB//aldM3K5NDOG441KyBskaZz406oL1SJP0mEh6
JM+I3uOVqxG7o36ntLn6feSWeL6sgv1CpgzQH2kiQpgWK/T7raGxuSvngfuFoSyr
v82fAJ2GD/Cw/0E5IFj/AVPbCWY9EzE9m2AkkFhfpXTP1qLXfIaIlFZiiZZrW+fL
EM5hpYgcYA7V2hP++Im5U7MEndhRASbHW+XvCR7WKMm5V+Rt3wjy
-----END CERTIFICATE-----`,
					},
				},
				Opensearch: &OpensearchSettings{
					Config: &ConfigOpensearchSettings{
						AdminKey: `-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQD0aBYmZyhNjsbi
16ybvxYSBYrIKTdCrZ2g/FeAnfxVY4/85J6KWvvfoWFDqp0nAShlmQb7Fe5GQuN1
IMTDbpCgYxAVX6Kmk6E6iKcmpko5p5K2vEo2A0/0V9vh2W5mdJjjGOtjDgtYAYR9
aDMdLdyagKfb3AnaOx+TotQuij0nFyYaTKGwK7sh5/y9Pyt0floeB0apndiJbh+e
lDzG2t8QLBivsuIl0pI510PZSVVcWIBkJ2RVAPyy8gy/wOInsE08o2+0SrIxz2f7
j+4ZFKfXEUiBopiwy7k5aFeq2gXF2r/Ndpi96uNsP4CXxHkUd+7jcFZQVHPiXNIX
P6MCLbLXAgMBAAECggEACD7WuHb0eiFd/lsuXJbGxNbhBr21Oo+m6L56qUErOSpB
ulNwMdS9+J52LJU99gno9fyCqsfjoQUyrUnsuXcqc+7DpSTz1NDYOKRRl1E24dkQ
bw/NJSNZeDHanjT6r4QxgD/f+RiJM2/hq2VvjAV3EtNSVm2G+5DREOcGZ4eMZpwl
cPg1xrfEOf4Q7mLmiGxBW7tdGzilTsQSwzYifMWzdRmuxNSB8+jIu7Zy53mdUNyc
I8nDxX8nfLND83GSPlzYuu2S5+cIHumKwtfBcde06Mn72bCLlkHF/AQEWMzGSqv6
BQAeK3Dr1wI0JZ2Bil6L5Jld3xuRmu++i0DMF8pnwQKBgQD834ChWyoNu1YbXijS
f53TEfuRZEBgyiOtfgrW723L9uMM975OCBJrhZ4/+WlUGIiZvr2COES3ikxYCzQ6
kcukAjj9f0JupIOxABT+dwcJIJ4JDdFD8tUwAvY9ys2/WoOs5TdcCxRntrjL0xjG
ZUtDnMI90aOmihY1LB6yEVwh4QKBgQD3bchTW6jC2UdS52Kosdy7YhsgTdiZ+Mqs
kL52SzBgd0lK4K35nHMUd+C3O90icKEfrpJX1cSbyFfSuimUQdK0I1bl0ijL8+uJ
FkhOC0iy6um8xO/dvP66j7cuvNXYfOiJAqFJfGiSLe/vuT+JerbFFEidIRXqWSlh
LIHtvxbbtwKBgBxWL2Plg2DmjU+jzY9JHbZ5XWd9hHlULYtThINxcSxaDjd1y62S
2f2Si5k/qb3ywdv4s+PTyl+G7+ct2jx1+gv288v0Zs1fQiKjj7a0P+WV8h+xnLGw
lJM8wbtK7qNy0S6ewQVfeHnmz+6HSU9yKmz5NAsZYu1WrAZpW0c5CsoBAoGBAL1P
aumUhM/obLDaxtqpk2hvjK+vwB02hON5r7BUoQP94L8AnzwPXuF3QyEPFYfHQxA5
glDgBxjmNYPO2gdMQYmATHl0zbAWxczSlqnX6lyybfn3eEtg0kktsot5Aeks0MIb
mAngvSWzLhRt2VY35OVvOou2h80RQR7Pbe3YugWLAoGATLlLM3U80bexIiyvwmot
0gFgd8NIIDF48jTYEpZ9lsfieb7QfkvVVlvm3YsWaDtSd3OswWN6tkyFnz2GJxEB
j8yVCBhAOxBfDRpMRzOpBe2v8u/o2Pxtw1leHmHbWYqbfwJnupnx3wnrzCVm+oLP
UHB/aximuVLK+eRz/ZIZ3KU=
-----END PRIVATE KEY-----`,
						AdminCert: `-----BEGIN CERTIFICATE-----
MIIDgTCCAmmgAwIBAgIJAPMNo6eG0UBZMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNTIyMTEzOTE5WhcNMjYwNTIxMTEzOTE5WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZhZG1pbjCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBAPRoFiZnKE2OxuLXrJu/FhIFisgpN0KtnaD8V4Cd
/FVjj/zknopa+9+hYUOqnScBKGWZBvsV7kZC43UgxMNukKBjEBVfoqaToTqIpyam
Sjmnkra8SjYDT/RX2+HZbmZ0mOMY62MOC1gBhH1oMx0t3JqAp9vcCdo7H5Oi1C6K
PScXJhpMobAruyHn/L0/K3R+Wh4HRqmd2IluH56UPMba3xAsGK+y4iXSkjnXQ9lJ
VVxYgGQnZFUA/LLyDL/A4iewTTyjb7RKsjHPZ/uP7hkUp9cRSIGimLDLuTloV6ra
BcXav812mL3q42w/gJfEeRR37uNwVlBUc+Jc0hc/owItstcCAwEAAaM3MDUwHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMBQGA1UdEQQNMAuCCWNoZWZhZG1p
bjANBgkqhkiG9w0BAQsFAAOCAQEAD4uod0aFfmwHuXshXFPcvu5gkkvd2Prd8pMz
TiOu0A1IYgVHvAg0Bqs8bReZLsijUKhhjmlfwaUrW1i2r5jYp9Acd2K/rKwl7rov
gaP60IgkszFCjfZFU1Zb0+6OlPuG8PquMPSJuJDFm5mQkw52vb81guvF/cGlKCuI
fPwEr1CSOyHmQMQLLJsdzfB2RQ6MfG1pCH1FNPCfWjK8Rrd0Hic/sQ4Yd6q/Bmek
b9KA5lUyk1Ox/YBBfm4RfmeKRqQKtASF8UJG3eCut0h3+uqyQpCxAx/8MlipkF7g
1EvU3J0+e99VMZJ/wNlrAla08VsV/hjXd+NIZYkl31cghLXmTA==
-----END CERTIFICATE-----`,

						InstanceCount:     "3",
						EnableCustomCerts: true,
						RootCA: `-----BEGIN CERTIFICATE-----
MIIDQjCCAioCCQDgqVuWOfJnIDANBgkqhkiG9w0BAQsFADBjMQswCQYDVQQGEwJV
UzETMBEGA1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UE
CgwRQ2hlZiBTb2Z0d2FyZSBJbmMxETAPBgNVBAMMCHByb2dyZXNzMB4XDTIzMDUy
MjExMzkxOFoXDTI2MDUyMTExMzkxOFowYzELMAkGA1UEBhMCVVMxEzARBgNVBAgM
Cldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29m
dHdhcmUgSW5jMREwDwYDVQQDDAhwcm9ncmVzczCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAMgz+0U4g/jhxxlGzgY7ppZlC3F+z01DUPPDAyys3hghjjqH
CgAhdgPBkUyHvaR7XO75kVBlu7JRBydsfug15LXS6fPMjlRPMaf8Bq0sIIlHuvf0
D0T+Zwh/XwCBiOCRorLfc8DPSTm7OM/LwbCkxTPGdScusmMz0TuSJVtonScUK3x6
58vy3s3GlQAIqotSy+r6nXph3fTY2b+m3RPM1QTpnZ0hV0/M6DwKWDO38o1HZXU4
c+mDqo6CSLrBNAVnllQYZjuEf1Z7IxtT4zZHvh5quPQhE+13qM6kztSWu7fLx7T+
uDuLvBjHIJNZpxmwDyk30+b7zWogg+jkxOTjCnECAwEAATANBgkqhkiG9w0BAQsF
AAOCAQEAR6g2Zp3FEfOFmU3wf18Ldjq8Qu69AQd3p0a4PqjfGvVN4R1Rs9LA5uYv
VegcVs1LB7TxN6CEMq/fZrfGtJUYMhE2V7WM0RmUAhUHHIvq+43VVN0ZT7pTKPgl
YVLXjiBOsJpPko/j/MDIjYK67wBcOHj08bG2ew9twrfjso9SJPd4ILxlx7Iwh5R9
pQijrBxVHcj23iuN3l23aQL25soAXSjVraG+CcU/lXr8zuHSb04M8Z0LvyylNDBd
G5vGMhL6fl057HyQQw78h804cHukZrkA940YqEHadkEdfV6Z1G4YJOGcSsFPcSUR
D1C7LJG3dXCCKER6zRZqjtv3wy0K5Q==
-----END CERTIFICATE-----`,

						PrivateKey: `-----BEGIN PRIVATE KEY-----
MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQCj/umNjtsAFBql
gQSw7HgoQKCLviq/zXP0Sr0ZMliizGY/B5pJ6rux42bAF0FcKrqL/KBzko4n3fTg
NBHcZJUFHiL91ZqzWxjc2rxmSd9YOhPH7x31E+ozjBGKClNOR+pyf0wLrKos3HsV
gZKFM0lKmb/xIfrLMaaHzqUE2lfP/NIcbqAgLMYgwdO5LmOPezWlH+oKhPk6qxX3
KgidVAfQQ67pNYD+SuMNgT8/OYszz/bE3lzS7jRrt6/8UKvV8+jS/bzBbsz51FEY
cCiS3Xd5svDpC0IcTHicvp5yHtePyYzxxCEt/P0T7dIUoVXQ780NAL8Y/CGK6TUR
Ho7osjM9AgMBAAECggEASiB0AxdaaDuuG7conqwUV+V2bBPmENJWIksSFGyMYfHQ
GZdfJyAh/PNTw2n/kiCCN7pV8EeDWAPcpucCV8NjFHAd0uyVQ5Letx1r4TRs7t05
ibrMqLV6vBgI6YNnSk/5ag2eGvzN4v8552utBeY7r6u1ddItIWFs65/9OSdUX98m
9iKC9n4D7pFvJsRoUfeD5qf5tF2cmAGS8z+y3502LPx0rNoJchHh06bkEwQIdA7Y
TUsXIj6RXZqHgcyD0CGVI0gsT6lSywSzfUQvgLEV6Py4VD+1t2jo97bgScbLaFRN
upC41HJFloYBvin0jtjgo/x8OUTGgW5IkBmrhw5roQKBgQDRoQiWiNtVYh+3AX2g
DkDXqUbJLdWEElbkjxqzrC19jHCOI12S67MvNZGZ3ET//5agF5pWo/ZE1ZUy7WoJ
2C9IdbKauGFlA0Drl1xrjHXU22/w8iDFp/F9lhbvu0vGIlCGGZlarl97Ulrha33a
EHtxNtX7jlu3yrZdReqrUWEhWQKBgQDIRbyaVbVw8nBAKj49cqi8AayD/aAt1sYE
KuDG0pv1ucXkOYtO2kvTxVUdBIPozdK4nVo/XT+mZ7PYSculukegjkO/Y51F6Tg+
4jlQxMFv30UAhslacFcSNy5KCePZX/5sUbHYa6Amp5dYXMf1hud02PdIMoNygNZ4
KKeQhL7ghQKBgF1XtTlCi1fDr5ePpF6muhzNlWVzcUWz3Nk9F4i1vDPRWzUPblVD
erAkzEaUnGzZZDq5B9JYhAo2iI76xGLJzpQXRIY8X7HY9wlwhoilLLqxU3EYf5tD
ovZm5KOu5Ji/ItfzgiOszXteOnVxpcJ54F2TK0kuJIz8SKPTxCCwxe1RAoGAPA3N
VGpHEitgxZzlNP/g4R+PX7T6B0TT9AP3iyc0ZSbj1F/9ChQjkMknkJ/9/h1aBsoI
ed+4amnGYCEg0/1b5SVD42w3iPM6ToD/ttyJNMa6pkHEtz3gnjG1y7XTgSdr34dP
0RnU2EKA+5o2y8U8Oqmk3R1olTlVFor6VDe6FRECgYAsgRurQPvNyWTjtXp8QLZ/
jYa8jqXkS7a26JVGKG6gq5hoaFq/fRksNYyC4H88lD/zxRmzkFkrEDIyhjVh5OMN
PAJTLOnebC6W2xbisRJqDB2LfxmbaH2FA4kO6UzZkhYjudThu4y8uOQlEcVQO7GU
VPVlMVkJXQmXIl6v3hCN+Q==
-----END PRIVATE KEY-----`,
						PublicKey: `-----BEGIN CERTIFICATE-----
MIIDgTCCAmmgAwIBAgIJAPMNo6eG0UBaMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNTIyMTEzOTE5WhcNMjYwNTIxMTEzOTE5WjBkMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxEjAQBgNVBAMMCWNoZWZub2RlMTCCASIwDQYJKoZIhvcN
AQEBBQADggEPADCCAQoCggEBAKP+6Y2O2wAUGqWBBLDseChAoIu+Kr/Nc/RKvRky
WKLMZj8Hmknqu7HjZsAXQVwquov8oHOSjifd9OA0EdxklQUeIv3VmrNbGNzavGZJ
31g6E8fvHfUT6jOMEYoKU05H6nJ/TAusqizcexWBkoUzSUqZv/Eh+ssxpofOpQTa
V8/80hxuoCAsxiDB07kuY497NaUf6gqE+TqrFfcqCJ1UB9BDruk1gP5K4w2BPz85
izPP9sTeXNLuNGu3r/xQq9Xz6NL9vMFuzPnUURhwKJLdd3my8OkLQhxMeJy+nnIe
14/JjPHEIS38/RPt0hShVdDvzQ0Avxj8IYrpNREejuiyMz0CAwEAAaM3MDUwHQYD
VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMBMBQGA1UdEQQNMAuCCWNoZWZub2Rl
MTANBgkqhkiG9w0BAQsFAAOCAQEAI9B0IJgmeqgLgFvBiHhGsNzxAKo3z+YmU2Ta
bqmcBO8gTGnLsnaQPs25sDPvM7YEkcjazhj74+f+L70+rAXl45TLkLQnIGpa4Bbg
uBpYonPRzK3aSiDcnTeTH7LivuTJJQZptaT9jrcAcpK6AzWCopWR/E1rQ/oRCfiu
4/PGV2nllNHC8rZm4YB3uftmjaWiwISf/gRSuD5yGu1TcCnYr5w3PhvkVAKHYLdj
tIUlDTuTFXO/92ZCuW74YqBay+dAIB4ThU0jvUNYWTFV8MHmBgQ9CfG9WW4pjORI
qalq5zXuKm1t+lrxCFND6cXu9Uk8HtrnSS5IqF0DprdepkyYhA==
-----END CERTIFICATE-----`,
					},
				},

				Postgresql: &PostgresqlSettings{
					Config: &ConfigSettings{
						InstanceCount:     "3",
						EnableCustomCerts: true,
						RootCA: `-----BEGIN CERTIFICATE-----
MIIDQjCCAioCCQDgqVuWOfJnIDANBgkqhkiG9w0BAQsFADBjMQswCQYDVQQGEwJV
UzETMBEGA1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UE
CgwRQ2hlZiBTb2Z0d2FyZSBJbmMxETAPBgNVBAMMCHByb2dyZXNzMB4XDTIzMDUy
MjExMzkxOFoXDTI2MDUyMTExMzkxOFowYzELMAkGA1UEBhMCVVMxEzARBgNVBAgM
Cldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29m
dHdhcmUgSW5jMREwDwYDVQQDDAhwcm9ncmVzczCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAMgz+0U4g/jhxxlGzgY7ppZlC3F+z01DUPPDAyys3hghjjqH
CgAhdgPBkUyHvaR7XO75kVBlu7JRBydsfug15LXS6fPMjlRPMaf8Bq0sIIlHuvf0
D0T+Zwh/XwCBiOCRorLfc8DPSTm7OM/LwbCkxTPGdScusmMz0TuSJVtonScUK3x6
58vy3s3GlQAIqotSy+r6nXph3fTY2b+m3RPM1QTpnZ0hV0/M6DwKWDO38o1HZXU4
c+mDqo6CSLrBNAVnllQYZjuEf1Z7IxtT4zZHvh5quPQhE+13qM6kztSWu7fLx7T+
uDuLvBjHIJNZpxmwDyk30+b7zWogg+jkxOTjCnECAwEAATANBgkqhkiG9w0BAQsF
AAOCAQEAR6g2Zp3FEfOFmU3wf18Ldjq8Qu69AQd3p0a4PqjfGvVN4R1Rs9LA5uYv
VegcVs1LB7TxN6CEMq/fZrfGtJUYMhE2V7WM0RmUAhUHHIvq+43VVN0ZT7pTKPgl
YVLXjiBOsJpPko/j/MDIjYK67wBcOHj08bG2ew9twrfjso9SJPd4ILxlx7Iwh5R9
pQijrBxVHcj23iuN3l23aQL25soAXSjVraG+CcU/lXr8zuHSb04M8Z0LvyylNDBd
G5vGMhL6fl057HyQQw78h804cHukZrkA940YqEHadkEdfV6Z1G4YJOGcSsFPcSUR
D1C7LJG3dXCCKER6zRZqjtv3wy0K5Q==
-----END CERTIFICATE-----`,
						PrivateKey: `-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDFyv9EdnKYnvHp
Xd9Uc2z6mnNY5KP5ZfQkObc/T8pHV1XkOvOJyXy0lWmJHd+eIJA5ij/AQDq5GmW+
YHW6c9NNFrvXZf5avMtfb2xcmgvXby2FGmnpcjtHd85fI08QAZk7qAeexKkep09k
IIvouYhL3CHj/4CpD4p2gKHEnoPtGNWaK8JTinm3Iu1s2E7jsgQ/+lNddSadfb3O
Vjp0LXBwSxG3fO/AoljCBdSAyyFXMHaH7sjw4G48FdONQULyjj7KvvGO0gHUApBg
d5lrFajLTPxgVTAJjhmu/LY/tjATZ0BWKmdJSQ022ZEKx4up1yGaHHBUpYI/UaiQ
Vmp92SxXAgMBAAECggEBALClRSk9p8bKXT6QCb6Af5mois+fExrPhSU9Ln0qo3rn
ctwsEgjCm88jiWdd+LJeXrAk2h62vjtGaguGVl44x0OXxBbxDiK3beJDvsFNCrpS
npK7Lk/BJ1QCmZq6DAg9hT6UKIoRFQE9Z1gDATDNUf5+EP5w19Uk/gIri03wS95Y
t6xnA11cdP1tpEnGgqG+0wgjxgsYz6EdrBOj/UQeIGE9tCM/G13+cZFNIDhuVCMm
IRkMqbdBVjlXmqi1f+bxYYPV8+2A6yCHN4x9fq9LZzAYXfReaG+wzGLIhXwvKEtq
TOBCFkgnYu7wYZYwNEtmTO49xaZQfWCpNAbREupLD7ECgYEA4a10ye11NUGy7Cym
FzM8XgTI0gF4NCg+UMoGR7D1uoVOI6f0Y4JTwauYRZaR7K5IN4k4i27GgEerYfck
LGWpkA6gpkclunUQ8Ubxm0YBHZnfmheqMoayP/SzoGQdknfdPkiXhaI37IWejKcO
5ILfxSGK8vzaXPZpKomljk9hJ40CgYEA4F5nqkWYVzbWKUPboap8v8Ct03y6DzP+
S1vgQRsm01JQnNfklETXof8+YCJqfMuSrykzRGAe7TwGWmLgwLvlxiWgRsTAfeo2
s4tTZmMcBVa2MvtqdJD2njOlfFMfCejkV/8KKXy9DJ1yQ9euWtblesU9CBJRr5SC
9svOPGHYCHMCgYA9samHuj6cfIVpQxt0pDEQksZDgttVhtriQxhMaPgEMYUXAkcx
HOPAwiQygeMKjOp5JC4tD+98Chu0AFgHOxOLqjQIwNJzkqU7EGXkSNLtQK979JQ2
k9QO39prMnNTIyl8aWPiyGH5at3ZHaJYnd6GiZDutGkNmN9PHaoAqXqp0QKBgCav
oGg3f8Dp75tF3ATQBJp7en1QsDQW3u3XdZ9EMzmUo9mnT/5QsG16OSMSTBIgd7ZE
AFb1y99TzjSff+k7fK7hpfUNz7LmQ3BJwaORyy8QeHHp770RkbRNa2c4Xc2znkud
6f6lR2N5ck5ITgPTsdWtVIyju/nuPXaYRYMby8gJAoGBANoLWRp5Vyic8S8hBS/C
DILTDvolupQTrcUfMN0shdTdjxcHnU8Wr0XhMTrMdKWR4hVPuvv/Y58oG3w709Bo
UghRv7EU457jiqqUtoI/R+NV+WGK3v1Yf57dd7qCH+7KVsnckmwwLzx2JXqzcpqe
PuFdbAb/aYtEr88nS44l8FXv
-----END PRIVATE KEY-----`,
						PublicKey: `-----BEGIN CERTIFICATE-----
MIIDfzCCAmegAwIBAgIJAPMNo6eG0UBdMA0GCSqGSIb3DQEBCwUAMGMxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwHhcN
MjMwNTIyMTEzOTIwWhcNMjYwNTIxMTEzOTIwWjBjMQswCQYDVQQGEwJVUzETMBEG
A1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hl
ZiBTb2Z0d2FyZSBJbmMxETAPBgNVBAMMCGNoZWZub2RlMIIBIjANBgkqhkiG9w0B
AQEFAAOCAQ8AMIIBCgKCAQEAxcr/RHZymJ7x6V3fVHNs+ppzWOSj+WX0JDm3P0/K
R1dV5Drzicl8tJVpiR3fniCQOYo/wEA6uRplvmB1unPTTRa712X+WrzLX29sXJoL
128thRpp6XI7R3fOXyNPEAGZO6gHnsSpHqdPZCCL6LmIS9wh4/+AqQ+KdoChxJ6D
7RjVmivCU4p5tyLtbNhO47IEP/pTXXUmnX29zlY6dC1wcEsRt3zvwKJYwgXUgMsh
VzB2h+7I8OBuPBXTjUFC8o4+yr7xjtIB1AKQYHeZaxWoy0z8YFUwCY4Zrvy2P7Yw
E2dAVipnSUkNNtmRCseLqdchmhxwVKWCP1GokFZqfdksVwIDAQABozYwNDAdBgNV
HSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwEwEwYDVR0RBAwwCoIIY2hlZm5vZGUw
DQYJKoZIhvcNAQELBQADggEBAK3O2VEYJQ4byjMYxuxmVEwHaNrl1XrwDZ7pKciq
x9GyLFwYva4svKZfawUbZUQsIBLxRAaUDPVdfb8MYn1lTI4q2y9XGT1osM0SuL6f
gllp4Yg4rTc94G5yXzhnWYCqrX9XZK0muFKCuJnniC0VmrP9zfojUsa0x6qvIfKf
EB5u6SKRbQGS1ECryGQfiwXzhwy17/Qw7Ab44ufDpWmiw6fdNY/KhBKIwCRSHoJJ
QQSO1eGYc/f3j7Kve5LaTOxADqBhGcxyItqdEJlmhKwAp2aOmO6pZLPCuZvCmvDL
Dv6bUUXSsZF4fb1diLIBpmD1hh8OGNY65LUPpzAxJeZvo5w=
-----END CERTIFICATE-----`,
					},
				},
				Aws: &AwsSettings{
					Config: &ConfigAwsSettings{
						Profile:                     "default",
						Region:                      "ca-central-1",
						AwsVpcID:                    "vpc-b2fad5db",
						AwsCidrBlockAddr:            "172.31.192.0",
						PrivateCustomSubnets:        []string{},
						PublicCustomSubnets:         []string{},
						SSHKeyPairName:              "central",
						SetupManagedServices:        true,
						ManagedOpensearchDomainName: "managed-services-os",
						ManagedOpensearchDomainURL:  "search-managed-services-os-eckom3msrwqlmjlgbdu.us-east-1.es.amazonaws.com",

						ManagedOpensearchUsername:     "admin",
						ManagedOpensearchUserPassword: "Progress@123",
						ManagedOpensearchCertificate:  "",
						AwsOsSnapshotRoleArn:          "arn:aws:iam::1127583934333:role/managed-services",
						OsSnapshotUserAccessKeyID:     "AKIA..........PQS7Q7A",
						OsSnapshotUserAccessKeySecret: "skP4Mqihj....................anAXAX",
						ManagedRdsInstanceURL:         "managed-rds-db.cww4poze5gkx.ap-northeast-1.rds.amazonaws.com:5432",
						ManagedRdsSuperuserUsername:   "postgres",
						ManagedRdsSuperuserPassword:   "chefautomate",
						ManagedRdsDbuserUsername:      "postgres",
						ManagedRdsDbuserPassword:      "chefautomate",
						ManagedRdsCertificate:         "",
						AmiID:                         "ami-01c7ecac079939e18",
						DeleteOnTermination:           true,
						AutomateServerInstanceType:    "t3.medium",
						ChefServerInstanceType:        "t3.medium",
						OpensearchServerInstanceType:  "m5.large",
						PostgresqlServerInstanceType:  "m5.large",
						AutomateLbCertificateArn:      "arn:aws:acm:ca-central-1:112758395563:certificate/1754e15c-5834-44b0-82a7-0508c3155601",
						ChefServerLbCertificateArn:    "arn:aws:acm:ca-central-1:112758395563:certificate/1754e15c-5834-44b0-82a7-0508c3155601",
						AutomateEbsVolumeIops:         "100",
						AutomateEbsVolumeSize:         "50",
						AutomateEbsVolumeType:         "gp3",
						ChefEbsVolumeIops:             "100",
						ChefEbsVolumeSize:             "50",
						ChefEbsVolumeType:             "gp3",
						OpensearchEbsVolumeIops:       "100",
						OpensearchEbsVolumeSize:       "50",
						OpensearchEbsVolumeType:       "gp3",
						PostgresqlEbsVolumeIops:       "100",
						PostgresqlEbsVolumeSize:       "50",
						PostgresqlEbsVolumeType:       "gp3",
						AmiFilterName:                 "",
						AmiFilterVirtType:             "",
						AmiFilterOwner:                "",
						LbAccessLogs:                  "false",
						XContact:                      "user",
						XDept:                         "",
						XProject:                      "",
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
		{
			name: "Parse OnPrem Db Aws Managed Config",
			args: args{configFile: "./testdata/HaOnPremDbAwsManaged.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					ExistingInfra: &ConfigInitials{
						SSHUser:      "ubuntu",
						SSHGroupName: "ubuntu",

						SSHKeyFile:       "~/.ssh/A2HA.pem",
						SSHPort:          "22",
						SecretsKeyFile:   "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile: "/hab/a2_deploy_workspace/secrets.json",
						Architecture:     "existing_nodes",
						WorkspacePath:    "/hab/a2_deploy_workspace",
						BackupMount:      "/mnt/automate_backups",
						BackupConfig:     "file_system",
					},
				},
				ObjectStorage: &ObjectStorage{
					Config: &ConfigObjectStorage{
						BucketName: "",
						AccessKey:  "",
						SecretKey:  "",
						Endpoint:   "",
						Region:     "us-west-1",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword:     "123456789",
						Fqdn:              "chefautomate.example.com",
						InstanceCount:     "2",
						ConfigFile:        "configs/automate.toml",
						TeamsPort:         "",
						EnableCustomCerts: false,
					},
				},
				ChefServer: &ChefServerSettings{
					Config: &ConfigSettings{
						InstanceCount:     "2",
						EnableCustomCerts: false,
					},
				},
				Opensearch: &OpensearchSettings{
					Config: &ConfigOpensearchSettings{
						InstanceCount:     "3",
						EnableCustomCerts: false,
					},
				},
				Postgresql: &PostgresqlSettings{
					Config: &ConfigSettings{
						InstanceCount:     "3",
						EnableCustomCerts: false,
					},
				},
				ExistingInfra: &ExistingInfraSettings{
					Config: &ConfigExistingInfraSettings{
						AutomatePrivateIps:   []string{"192.0.0.11", "192.0.0.12"},
						ChefServerPrivateIps: []string{"192.0.0.11", "192.0.0.12"},
						OpensearchPrivateIps: []string{"192.0.0.1", "192.0.0.2", "192.0.0.2"},
						PostgresqlPrivateIps: []string{"192.0.0.1", "192.0.0.2", "192.0.0.2"},
					},
				},
				External: &ExternalSettings{
					Database: &ExternalDBSettings{Type: "aws",
						PostgreSQL: &ExternalPgSettings{
							InstanceURL:        "managed-rds-db.c5gkx.ap-northeast-1.rds.amazonaws.com:5432",
							SuperuserUsername:  "postgres",
							SuperuserPassword:  "Progress123",
							DbuserUsername:     "postgres",
							DbuserPassword:     "Progress123",
							PostgresqlRootCert: "<cert_content>",
						},
						OpenSearch: &ExternalOsSettings{
							OpensearchDomainName:   "managed-services-os",
							OpensearchDomainURL:    "search-managed-services-os.us-east-1.es.amazonaws.com",
							OpensearchUsername:     "admin",
							OpensearchUserPassword: "Progress@123",
							OpensearchRootCert:     "<cert_content>",

							Aws: &AwsExternalOsSettings{
								AwsOsSnapshotRoleArn:          "arn:aws:iam::1127583934333:role/managed-services",
								OsSnapshotUserAccessKeyID:     "AKIA..........PQS7Q7A",
								OsSnapshotUserAccessKeySecret: "skP4Mqihj....................anAXAX",
							},
						},
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
		{
			name: "Parse OnPrem Db Self-Managed Config",
			args: args{configFile: "./testdata/HaOnPremDbSelfManaged.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					ExistingInfra: &ConfigInitials{
						SSHUser:          "ubuntu",
						SSHGroupName:     "ubuntu",
						SSHKeyFile:       "~/.ssh/A2HA.pem",
						SSHPort:          "22",
						SecretsKeyFile:   "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile: "/hab/a2_deploy_workspace/secrets.json",
						Architecture:     "existing_nodes",
						WorkspacePath:    "/hab/a2_deploy_workspace",
						BackupMount:      "/mnt/automate_backups",
						BackupConfig:     "file_system",
					},
				},
				ObjectStorage: &ObjectStorage{
					Config: &ConfigObjectStorage{
						BucketName: "",
						AccessKey:  "",
						SecretKey:  "",
						Endpoint:   "",
						Region:     "us-west-1",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword:     "123456789",
						Fqdn:              "chefautomate.example.com",
						InstanceCount:     "2",
						ConfigFile:        "configs/automate.toml",
						TeamsPort:         "",
						EnableCustomCerts: false,
					},
				},
				ChefServer: &ChefServerSettings{
					Config: &ConfigSettings{
						InstanceCount:     "2",
						EnableCustomCerts: false,
					},
				},
				Opensearch: &OpensearchSettings{
					Config: &ConfigOpensearchSettings{
						InstanceCount:     "3",
						EnableCustomCerts: false,
					},
				},
				Postgresql: &PostgresqlSettings{
					Config: &ConfigSettings{
						InstanceCount:     "3",
						EnableCustomCerts: false,
					},
				},
				ExistingInfra: &ExistingInfraSettings{
					Config: &ConfigExistingInfraSettings{
						AutomatePrivateIps:   []string{"192.0.0.11", "192.0.0.12"},
						ChefServerPrivateIps: []string{"192.0.0.11", "192.0.0.12"},
						OpensearchPrivateIps: []string{"192.0.0.1", "192.0.0.2", "192.0.0.2"},
						PostgresqlPrivateIps: []string{"192.0.0.1", "192.0.0.2", "192.0.0.2"},
					},
				},
				External: &ExternalSettings{
					Database: &ExternalDBSettings{Type: "self-managed",
						PostgreSQL: &ExternalPgSettings{
							InstanceURL:        "",
							SuperuserUsername:  "",
							SuperuserPassword:  "",
							DbuserUsername:     "",
							DbuserPassword:     "",
							PostgresqlRootCert: "",
						},
						OpenSearch: &ExternalOsSettings{
							OpensearchDomainName:   "",
							OpensearchDomainURL:    "",
							OpensearchUsername:     "",
							OpensearchUserPassword: "",
							OpensearchRootCert:     "",

							Aws: &AwsExternalOsSettings{
								AwsOsSnapshotRoleArn:          "",
								OsSnapshotUserAccessKeyID:     "",
								OsSnapshotUserAccessKeySecret: "",
							},
						},
					},
				},
			},
			wantErr: false,
			err:     nil,
		},
		{
			name: "Parse OnPrem Config",
			args: args{configFile: "./testdata/HaOnPrem.toml"},
			want: &HaDeployConfig{
				Architecture: &Architecture{
					ExistingInfra: &ConfigInitials{
						SSHUser:                     "",
						SSHGroupName:                "",
						SSHKeyFile:                  "",
						SSHPort:                     "",
						SecretsKeyFile:              "/hab/a2_deploy_workspace/secrets.key",
						SecretsStoreFile:            "/hab/a2_deploy_workspace/secrets.json",
						SudoPassword:                "",
						LoggingMonitoringManagement: "",
						Architecture:                "existing_nodes",
						WorkspacePath:               "/hab/a2_deploy_workspace",
						BackupMount:                 "/mnt/automate_backups",
						BackupConfig:                "",
						S3BucketName:                "",
						HabitatUIDGid:               "",
					},
				},
				ObjectStorage: &ObjectStorage{
					Config: &ConfigObjectStorage{
						BucketName: "",
						AccessKey:  "",
						SecretKey:  "",
						Endpoint:   "",
						Region:     "",
					},
				},
				Automate: &AutomateSettings{
					Config: &ConfigAutomateSettings{
						AdminPassword:     "",
						Fqdn:              "",
						ConfigFile:        "configs/automate.toml",
						TeamsPort:         "",
						InstanceCount:     "",
						EnableCustomCerts: true,
						RootCA:            "a2_cert",
						PrivateKey:        "a2_pvt_key",
						PublicKey:         "a2_public_key",
						CertsByIP:         &[]CertByIP{{IP: "127.0.0.1", PrivateKey: "a2_pvt_key", PublicKey: "a2_public_key"}},
					},
				},
				ChefServer: &ChefServerSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: true,
						PrivateKey:        "cs_pvt_key",
						PublicKey:         "cs_public_key",
						CertsByIP:         &[]CertByIP{{IP: "127.0.0.1", PrivateKey: "cs_pvt_key", PublicKey: "cs_public_key"}},
					},
				},
				Opensearch: &OpensearchSettings{
					Config: &ConfigOpensearchSettings{
						InstanceCount:     "",
						EnableCustomCerts: true,
						RootCA:            "os_cert",
						PrivateKey:        "os_pvt_key",
						PublicKey:         "os_public_key",
						AdminCert:         "os_admin_cert",
						AdminKey:          "os_admin_key",
						CertsByIP:         &[]CertByIP{{IP: "127.0.0.1", PrivateKey: "os_pvt_key", PublicKey: "os_public_key"}},
					},
				},
				Postgresql: &PostgresqlSettings{
					Config: &ConfigSettings{
						InstanceCount:     "",
						EnableCustomCerts: true,
						RootCA:            "pg_cert",
						PrivateKey:        "pg_pvt_key",
						PublicKey:         "pg_pvt_key",
						CertsByIP:         &[]CertByIP{{IP: "127.0.0.1", PrivateKey: "pg_pvt_key", PublicKey: "pg_pvt_key"}},
					},
				},
				ExistingInfra: &ExistingInfraSettings{
					Config: &ConfigExistingInfraSettings{
						AutomatePrivateIps:   []string{},
						ChefServerPrivateIps: []string{},
						OpensearchPrivateIps: []string{},
						PostgresqlPrivateIps: []string{},
					},
				},
			},
			wantErr: false,
			err:     nil,
		}, {
			name:    "Parse OnPrem Config file not found",
			args:    args{configFile: "./testdata/OnPremConfig.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error reading config TOML file: open ./testdata/OnPremConfig.toml: no such file or directory"),
		},
		{
			name:    "Error unmarshalling toml file",
			args:    args{configFile: "./testdata/UnmarshalErr.toml"},
			want:    nil,
			wantErr: true,
			err:     errors.New("error unmarshalling config TOML file: (5, 2): unexpected token table key cannot contain ']', was expecting a table key"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			config := HaDeployConfig{}
			got, err := config.Parse(tt.args.configFile)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			}
			// Compare the actual and expected configuration structs
			// if !reflect.DeepEqual(got, tt.want) {
			// 	t.Errorf("got %v, want %v", got, tt.want)
			// }
			assert.Equal(t, tt.want, got)
		})
	}
}
