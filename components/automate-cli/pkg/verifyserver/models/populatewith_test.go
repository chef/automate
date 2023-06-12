package models

import (
	"errors"
	"reflect"
	"testing"

	"github.com/chef/automate/lib/config"
	"github.com/stretchr/testify/assert"
)

func TestAppendCertsByIpToNodeCerts(t *testing.T) {
	// Define test cases
	testCases := []struct {
		name          string
		certsByIP     []config.CertByIP
		privateKey    string
		publicKey     string
		adminKey      string
		adminCert     string
		expectedNodes []NodeCert
	}{
		{
			name: "Valid input with multiple certs",
			certsByIP: []config.CertByIP{
				{IP: "192.168.0.1", PrivateKey: "key1_private.pem", PublicKey: "key1_public.pem"},
				{IP: "192.168.0.2", PrivateKey: "key2_private.pem", PublicKey: "key2_public.pem"},
			},
			privateKey: "private.pem",
			publicKey:  "public.pem",
			adminKey:   "admin_key.pem",
			adminCert:  "admin_cert.pem",
			expectedNodes: []NodeCert{
				{
					IP:        "192.168.0.1",
					Key:       "key1_private.pem",
					Cert:      "key1_public.pem",
					AdminKey:  "admin_key.pem",
					AdminCert: "admin_cert.pem",
				},
				{
					IP:        "192.168.0.2",
					Key:       "key2_private.pem",
					Cert:      "key2_public.pem",
					AdminKey:  "admin_key.pem",
					AdminCert: "admin_cert.pem",
				},
			},
		},
		{
			name:       "Valid input with a single cert",
			certsByIP:  []config.CertByIP{{IP: "192.168.0.1", PrivateKey: "key1_private.pem", PublicKey: "key1_public.pem"}},
			privateKey: "private.pem",
			publicKey:  "public.pem",
			adminKey:   "admin_key.pem",
			adminCert:  "admin_cert.pem",
			expectedNodes: []NodeCert{
				{
					IP:        "192.168.0.1",
					Key:       "key1_private.pem",
					Cert:      "key1_public.pem",
					AdminKey:  "admin_key.pem",
					AdminCert: "admin_cert.pem",
				},
			},
		},
	}

	// Perform tests
	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			c := Config{}
			c.appendCertsByIpToNodeCerts(&tc.certsByIP, tc.privateKey, tc.publicKey, tc.adminKey, tc.adminCert)

			// Verify the expected result
			if !reflect.DeepEqual(c.Certificate.Nodes, tc.expectedNodes) {
				t.Errorf("Unexpected node certificates. Expected: %v, Actual: %v", tc.expectedNodes, c.Certificate.Nodes)
			}
		})
	}
}

func TestPopulateWith(t *testing.T) {
	tests := []struct {
		name     string
		filePath string
		want     *Config
		wantErr  bool
		err      error
	}{
		{
			name:     "PopulateWith Invalid OnPrem Config",
			filePath: "./testdata/InvalidHaOnPrem.toml",
			want:     &Config{},
			wantErr:  true,
			err:      errors.New("invalid or empty: ssh_user\ninvalid or empty: ssh_key_file\ninvalid S3 endpoint format. Endpoint should end with '.amazonaws.com'\nautomate private ip 1324.2534.1is not valid\ninvalid or empty: chef_server_private_ips\ninvalid or empty: opensearch_private_ips\ninvalid or empty: postgresql_private_ips\nurl should not include the protocol (http:// or https://): automate fqdn\nempty value: automate instance_count\ninvalid value 'automate.toml' for field 'config_file'. Expected values are: configs/automate.toml\ninvalid format. Failed to decode root_ca for automate\ninvalid format. Failed to decode private_key for automate\ninvalid format. Failed to decode public_key for automate\ninvalid format. Failed to decode private_key for automate ip\ninvalid format. Failed to decode public_key for automate ip\ninvalid value 'chef server instance_count' for field 'two'\ninvalid format. Failed to decode private_key for chef-server\ninvalid format. Failed to decode public_key for chef-server\ninvalid format. Failed to decode private_key for chef server ip\ninvalid format. Failed to decode public_key for chef server ip\nempty value: opensearch instance_count\nopensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false\nopensearch iproot_ca and/or public_key and/or private_key are missing in certs_by_ip. Otherwise set enable_custom_certs to false\nempty value: postgresql instance_count\ninvalid format. Failed to decode root_ca for postgresql\ninvalid format. Failed to decode private_key for postgresql\ninvalid format. Failed to decode public_key for postgresql\npostgresql ip 0.0.1 for certs is not valid\ninvalid format. Failed to decode private_key for postgresql ip\ninvalid format. Failed to decode public_key for postgresql ip"),
		},
		{
			name:     "PopulateWith OnPrem Db Aws Managed Config",
			filePath: "./testdata/HaOnPremDbAwsManaged.toml",
			want: &Config{
				SSHUser: SSHUser{
					Username:     "ubuntu",
					PrivateKey:   "./testdata/A2HA.pem",
					SudoPassword: "",
				},
				Arch: "existing_nodes",
				Backup: Backup{
					FileSystem: FileSystem{
						MountLocation: "/mnt/automate_backups",
					},
					ObjectStorage: ObjectStorage{
						Endpoint:   "",
						BucketName: "",
						BasePath:   "",
						AccessKey:  "",
						SecretKey:  "",
						AWSRegion:  "us-west-1",
					},
				},
				Hardware: Hardware{
					AutomateNodeCount: 2,
					AutomateNodeIps: []string{
						"192.0.0.11", "192.0.0.12",
					},
					ChefInfraServerNodeCount: 2,
					ChefInfraServerNodeIps: []string{
						"192.0.0.11", "192.0.0.12",
					},
					PostgresqlNodeCount: 3,
					PostgresqlNodeIps:   nil,
					OpenSearchNodeCount: 3,
					OpenSearchNodeIps:   nil,
				},
				Certificate: Certificate{
					AutomateFqdn:   "chefautomate.example.com",
					ChefServerFqdn: "chefautomate.example.com",
					RootCert:       "",
					Nodes:          nil,
				},
				ExternalOS: ExternalOS{
					OSDomainName:   "managed-services-os",
					OSDomainURL:    "search-managed-services-os.us-east-1.es.amazonaws.com",
					OSUsername:     "admin",
					OSUserPassword: "Progress@123",
					OSCert:         "<cert_content>",
					OSRoleArn:      "arn:aws:iam::1127583934333:role/managed-services",
				},
				ExternalPG: ExternalPG{
					PGInstanceURL:       "managed-rds-db.c5gkx.ap-northeast-1.rds.amazonaws.com:5432",
					PGSuperuserName:     "postgres",
					PGSuperuserPassword: "Progress123",
					PGDbUserName:        "postgres",
					PGDbUserPassword:    "Progress123",
					PGRootCert:          "<cert_content>",
				},
				DeploymentState: "",
				APIToken:        "",
			},
			wantErr: false,
			err:     nil,
		},
		{
			name:     "PopulateWith AWS Managed Config",
			filePath: "./testdata/HaAwsManaged.toml",
			want: &Config{
				SSHUser: SSHUser{
					Username:     "ubuntu",
					PrivateKey:   "./testdata/A2HA.pem",
					SudoPassword: "",
				},
				Arch: "aws",
				Backup: Backup{
					FileSystem: FileSystem{
						MountLocation: "/mnt/automate_backups",
					},
					ObjectStorage: ObjectStorage{
						Endpoint:   "",
						BucketName: "",
						BasePath:   "",
						AccessKey:  "",
						SecretKey:  "",
						AWSRegion:  "",
					},
				},
				Hardware: Hardware{
					AutomateNodeCount:        2,
					AutomateNodeIps:          nil,
					ChefInfraServerNodeCount: 2,
					ChefInfraServerNodeIps:   nil,
					PostgresqlNodeCount:      3,
					PostgresqlNodeIps:        nil,
					OpenSearchNodeCount:      3,
					OpenSearchNodeIps:        nil,
				},
				Certificate: Certificate{
					AutomateFqdn:   "chefautomate.example.com",
					ChefServerFqdn: "chefautomate.example.com",
					RootCert:       "-----BEGIN CERTIFICATE-----\nMIIEDzCCAvegAwIBAgIBADANBgkqhkiG9w0BAQUFADBoMQswCQYDVQQGEwJVUzEl\nMCMGA1UEChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMp\nU3RhcmZpZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDQw\nNjI5MTczOTE2WhcNMzQwNjI5MTczOTE2WjBoMQswCQYDVQQGEwJVUzElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMpU3RhcmZp\nZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwggEgMA0GCSqGSIb3\nDQEBAQUAA4IBDQAwggEIAoIBAQC3Msj+6XGmBIWtDBFk385N78gDGIc/oav7PKaf\n8MOh2tTYbitTkPskpD6E8J7oX+zlJ0T1KKY/e97gKvDIr1MvnsoFAZMej2YcOadN\n+lq2cwQlZut3f+dZxkqZJRRU6ybH838Z1TBwj6+wRir/resp7defqgSHo9T5iaU0\nX9tDkYI22WY8sbi5gv2cOj4QyDvvBmVmepsZGD3/cVE8MC5fvj13c7JdBmzDI1aa\nK4UmkhynArPkPw2vCHmCuDY96pzTNbO8acr1zJ3o/WSNF4Azbl5KXZnJHoe0nRrA\n1W4TNSNe35tfPe/W93bC6j67eA0cQmdrBNj41tpvi/JEoAGrAgEDo4HFMIHCMB0G\nA1UdDgQWBBS/X7fRzt0fhvRbVazc1xDCDqmI5zCBkgYDVR0jBIGKMIGHgBS/X7fR\nzt0fhvRbVazc1xDCDqmI56FspGowaDELMAkGA1UEBhMCVVMxJTAjBgNVBAoTHFN0\nYXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD\nbGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w\nDQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3\nL7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D\neruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl\nxy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp\nVSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY\nWQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8fF5Q=\n-----END CERTIFICATE-----",
					Nodes:          nil,
				},
				ExternalOS: ExternalOS{
					OSDomainName:   "",
					OSDomainURL:    "",
					OSUsername:     "",
					OSUserPassword: "",
					OSCert:         "",
					OSRoleArn:      "",
				},
				ExternalPG: ExternalPG{
					PGInstanceURL:       "",
					PGSuperuserName:     "",
					PGSuperuserPassword: "",
					PGDbUserName:        "",
					PGDbUserPassword:    "",
					PGRootCert:          "",
				},
				DeploymentState: "",
				APIToken:        "",
			},
			wantErr: false,
			err:     nil,
		},
		{
			name:     "PopulateWith OnPrem Config",
			filePath: "./testdata/HaOnPrem.toml",
			want: &Config{
				SSHUser: SSHUser{
					Username:     "ubuntu",
					PrivateKey:   "./testdata/A2HA.pem",
					SudoPassword: "",
				},
				Arch: "existing_nodes",
				Backup: Backup{
					FileSystem: FileSystem{
						MountLocation: "automate_backups",
					},
					ObjectStorage: ObjectStorage{
						Endpoint:   "s3.amazonaws.com",
						BucketName: "test",
						BasePath:   "",
						AccessKey:  "test_access_key",
						SecretKey:  "test_secret_key",
						AWSRegion:  "us-west-1",
					},
				},
				Hardware: Hardware{
					AutomateNodeCount: 2,
					AutomateNodeIps: []string{
						"192.0.0.1", "192.0.0.2",
					},
					ChefInfraServerNodeCount: 2,
					ChefInfraServerNodeIps: []string{
						"192.0.1.1", "192.0.1.2",
					},
					PostgresqlNodeCount: 3,
					PostgresqlNodeIps: []string{
						"192.0.3.1", "192.0.3.2", "192.0.3.3",
					},
					OpenSearchNodeCount: 3,
					OpenSearchNodeIps: []string{
						"192.0.2.1", "192.0.2.2", "192.0.2.3",
					},
				},
				Certificate: Certificate{
					AutomateFqdn:   "chefautomate.example.com",
					ChefServerFqdn: "chefautomate.example.com",
					RootCert:       "-----BEGIN CERTIFICATE-----\nMIIEDzCCAvegAwIBAgIBADANBgkqhkiG9w0BAQUFADBoMQswCQYDVQQGEwJVUzEl\nMCMGA1UEChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMp\nU3RhcmZpZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDQw\nNjI5MTczOTE2WhcNMzQwNjI5MTczOTE2WjBoMQswCQYDVQQGEwJVUzElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMpU3RhcmZp\nZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwggEgMA0GCSqGSIb3\nDQEBAQUAA4IBDQAwggEIAoIBAQC3Msj+6XGmBIWtDBFk385N78gDGIc/oav7PKaf\n8MOh2tTYbitTkPskpD6E8J7oX+zlJ0T1KKY/e97gKvDIr1MvnsoFAZMej2YcOadN\n+lq2cwQlZut3f+dZxkqZJRRU6ybH838Z1TBwj6+wRir/resp7defqgSHo9T5iaU0\nX9tDkYI22WY8sbi5gv2cOj4QyDvvBmVmepsZGD3/cVE8MC5fvj13c7JdBmzDI1aa\nK4UmkhynArPkPw2vCHmCuDY96pzTNbO8acr1zJ3o/WSNF4Azbl5KXZnJHoe0nRrA\n1W4TNSNe35tfPe/W93bC6j67eA0cQmdrBNj41tpvi/JEoAGrAgEDo4HFMIHCMB0G\nA1UdDgQWBBS/X7fRzt0fhvRbVazc1xDCDqmI5zCBkgYDVR0jBIGKMIGHgBS/X7fR\nzt0fhvRbVazc1xDCDqmI56FspGowaDELMAkGA1UEBhMCVVMxJTAjBgNVBAoTHFN0\nYXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD\nbGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w\nDQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3\nL7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D\neruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl\nxy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp\nVSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY\nWQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8fF5Q=\n-----END CERTIFICATE-----",
					Nodes:          nil,
				},
				ExternalOS: ExternalOS{
					OSDomainName:   "",
					OSDomainURL:    "",
					OSUsername:     "",
					OSUserPassword: "",
					OSCert:         "",
					OSRoleArn:      "",
				},
				ExternalPG: ExternalPG{
					PGInstanceURL:       "",
					PGSuperuserName:     "",
					PGSuperuserPassword: "",
					PGDbUserName:        "",
					PGDbUserPassword:    "",
					PGRootCert:          "",
				},
				DeploymentState: "",
				APIToken:        "",
			},
			wantErr: false,
			err:     nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			haConfig := &config.HaDeployConfig{}
			err := haConfig.Parse(tt.filePath)
			if err != nil {
				t.Errorf("Error parsing HaDeployConfig: %v", err)
				return
			}
			c := &Config{}
			err = c.PopulateWith(haConfig)
			if tt.wantErr {
				assert.Equal(t, tt.err.Error(), err.Error())
			}

			assert.Equal(t, tt.want, c)
		})
	}
}
