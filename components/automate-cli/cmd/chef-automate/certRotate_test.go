package main

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

const (
	RemoteFilePath = "198.51.100.0:/home/ec2-user/certs/public.pem"
	LocalFilePath  = "/home/ec2-user/certs/public.pem"
	ValidIP        = "198.51.100.0"
	ValidIP1       = "198.51.100.1"
	ValidIP2       = "198.51.100.2"
	ValidIP3       = "198.51.100.3"
	ValidIP4       = "198.51.100.4"
	ValidIP5       = "198.51.100.5"
	ValidIP6       = "198.51.100.6"
	ValidIP7       = "198.51.100.7"
	ValidIP8       = "198.51.100.8"
	ValidIP9       = "198.51.100.9"
	FileContent    = `-----BEGIN CERTIFICATE-----
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
zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
-----END CERTIFICATE-----`
	ValidCertPath                = "./certRotate.go"
	TestOpensearchAdminAndRootCA = `
	[tls]
	rootCA = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	admin_cert = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	admin_key = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	ssl_cert = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	ssl_key = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
[plugins.security.authcz]
	admin_dn = '- CN=chefadmin,O=Chef Software Inc,L=Seattle,ST=Washington,C=US'
[plugins.security.ssl.transport]
	enforce_hostname_verification = false
	resolve_hostname = false
[plugins.security]
	nodes_dn = '- CN=chefnode1,O=Chef Software Inc,L=Seattle,ST=Washington,C=US'`
	TestFrontendConfig = `[[load_balancer.v1.sys.frontend_tls]]
	cert = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	key = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
[[global.v1.frontend_tls]]
	cert = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""
	key = """-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----"""`
	testfile = `./testfiles/ssh`

	rootCA = `-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----`
	admin_cert = `-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----`
	admin_key = `-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----`

	public_key = `-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----`
	private_key = `-----BEGIN CERTIFICATE-----
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
	zt0fhvRbVazc1xDCDqmI56FspGowaaBCDEfGA1UEBhMCVVMxJTAjBgNVBAoTHFN0
	YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD
	bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w
	DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3
	L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D
	eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl
	xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp
	VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY
	WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8xY5Z=
	-----END CERTIFICATE-----`

	private_cert_path    = "../../pkg/testfiles/certs/private_key.pem"
	public_cert_path     = "../../pkg/testfiles/certs/public_key.pem"
	OPENSEARCH_USER_TOML = `
	[discovery]
  minimum_master_nodes = 2
  ping_unicast_hosts = ["10.1.0.176", "10.1.1.125", "10.1.2.247"]

[network]
  host = "10.1.0.176"
  port = 9200

[plugins]
  [plugins.security]
    nodes_dn = "- CN=chefnode,O=Chef Software Inc,L=Seattle,ST=Washington,C=US"
    [plugins.security.authcz]
      admin_dn = "- CN=chefadmin,O=Chef Software Inc,L=Seattle,ST=Washington,C=US"
    [plugins.security.ssl]
      [plugins.security.ssl.transport]
        enforce_hostname_verification = false
        resolve_hostname = false

[tls]
  admin_cert = "-----BEGIN CERTIFICATE-----\nMIIDdjCCAl6gAwIBAgIUGRIvHV9V+hVgzYl6q2Keemg5vvMwDQYJKoZIhvcNAQEL\nBQAwYzELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhw\ncm9ncmVzczAeFw0yMzA5MTQxNDI2MjhaFw0yNjA5MTMxNDI2MjhaMGQxCzAJBgNV\nBAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow\nGAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzESMBAGA1UEAwwJY2hlZmFkbWluMIIB\nIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA56ettU4VAn7qz1j1NoiBQfn7\nS96t+Ta6CeeySm1RihsJdxhqAmPK83Jro/QFDzySrVD2GwuQ43DQfOKy25LRvKhV\nu5AY5k4Pqy8d/5T4Ike+MaC4SiVb5/In8Uqe6tLeQprun1J39Qo8FJ8CvEWsLbDx\nATLWo0olQDY60ciH6D02NHoRVqQ9dz8vleCJf+978GmvJqpUHnziYKyy/A+3Z8aY\nyjZncjwOP/KIPcKnrDg/4cLN4SZB3D/ZPyev+80fUEJfZGXv4xpr8JJILYSi/ryV\nDSvQjZKl7jiXFfKdZo3Zz6LSKovt4MfLVa/2mLQiuNCkmJZTq1hZE0qcQ/Q4lwID\nAQABoyEwHzAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwEwDQYJKoZIhvcN\nAQELBQADggEBADZSRlmAyLdwTc10Jh7WeuhoK3DklSIHk2hiWemiI4wuWiJ19IBg\nqPrBsSsZevODL6FgNlyEgdfSGXJpgZfzlNqBeW4nUc+lMkiTSGTvgr9SVmZAPz52\nzRahRxnUXJkbcwuU51bJn58xmIB1SFUHqAALHuAAOhoEqTxVfwtkC2dL4IFhNmWI\noZMMc6pQmR7B4dteJKogsE1sp031/PC0qch+8yDlxY8tfSLLYq0lIDaUVbvY+RDK\n2d/6zFSywsK0NfLb0gSZ/UayQtSSINwsH6AwkWlJuDNjC0qv9EbSZSH9hte8ofDN\nDrg3vVH19nkLatp4+eZZZf8yiGdkl8hNZFQ=\n-----END CERTIFICATE-----"
  admin_key = "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDnp621ThUCfurP\nWPU2iIFB+ftL3q35NroJ57JKbVGKGwl3GGoCY8rzcmuj9AUPPJKtUPYbC5DjcNB8\n4rLbktG8qFW7kBjmTg+rLx3/lPgiR74xoLhKJVvn8ifxSp7q0t5Cmu6fUnf1CjwU\nnwK8RawtsPEBMtajSiVANjrRyIfoPTY0ehFWpD13Py+V4Il/73vwaa8mqlQefOJg\nrLL8D7dnxpjKNmdyPA4/8og9wqesOD/hws3hJkHcP9k/J6/7zR9QQl9kZe/jGmvw\nkkgthKL+vJUNK9CNkqXuOJcV8p1mjdnPotIqi+3gx8tVr/aYtCK40KSYllOrWFkT\nSpxD9DiXAgMBAAECggEBAMoneqBYVl9KMFDnmX2QW/QeWVzZI0rypiDUr7LheSGi\n/HyQspoJSedut15pKS0lt/5FQ69QRY0lOttw3ZJdqmgPIlm+ouv/vQ6u3GfYMT2B\nDAm07n3N4kkj+hVIACx9/fVzzL7+Ma6F0u5P4Qw5ZMquuXJJUiNHJgDGEkhVzbws\nXaNucuk7WvbLnLR2MHrJPeAOrlF2/Wv6VBsEQRJkaEDwhIKmMWG+k/M3Q8DaerDj\nmtx+tbAMZhBKQr0x/2H3GYQI45IVXtiBfpoSdjq8TmaHMsFqPRB+G4KU+JbBW5Fw\nPirPnM1H9d8VNTlaB8b9T1lgcHGxWIYkMC5RfV06wRkCgYEA9Lc/7DubTh7cWwlR\nmvadMK0vrtXYbVQCok7qUlZC8WgwUJqfA0MfDC6qxAFrPNo/HEkSAWk4MPdRFwIV\nAMHB663NhFLtmxqZhfDgphYoQdX5jrD2YifgDU4w/fIr2kyWzoB6r4t/iXXBbtHO\nGrtFccF4q1Cmg9ZSDcf/uqFEGEUCgYEA8lZAkRm8UIBJ/5ZxO3XkMrxrADZMAS2u\nS4XCV2EEGeiBEAkMQtT5xccxfQ3faV9rs0YkVXXGyDg4a2AUCcbKyV9vwlLiy0vh\nGd8MZ79IwCr9AREI8uUTsKenr26+mbqCofX+6lZwyNCxbZSc/ver8UXpTwpc2hAG\n7H+t5aEqYSsCgYAra0ggNgM8PSWD0Yd1I0SImnHXZ4HbBAjjm3Tf3wZJpt7LrmOA\nRKyBkNYjqMzKIz8HWb+kGHMr3PW6S1hGphouIsxQKhaWaoXKyg5R6aSC6eA5fRR6\nHfEW60SuCgXV2bj4MruR4gJi9U24x+j1vTx5DobGfqzMv34Xi+DH0E0wsQKBgQDx\ndtns+py2Ba41+pwm6CgSGcXwNynyPqcd31Cuqh9hBVecN7e82+NomzsHZQxIPWjd\n/7TyZmFEXShybRBqUWb70ZlVIiuS76+CjMoakfGWcP8Z0fR9uZ2t9s/RsOI/4SSK\n5scyjiDhJ3izPFJWh0gPJ176f9PXAAM2IV6PoM/OcwKBgDT3SGqdpuwpuC/dRNU4\n5Q5WmTz3YWYxiVdJqVbL40TM2yEHh9ocsdgl9xDN8ELp38wlw5r4P6lfKjB9fmRZ\nYrFxlDZ0LURKyNEJq4KOp8criB2Q9UDDblgPVm8u7mIu24TqF2l9AFUf7AWn0px2\nvtG5yZjAY92/XaJkK2cRDop7\n-----END PRIVATE KEY-----"
  rootCA = "-----BEGIN CERTIFICATE-----\nMIIDpzCCAo+gAwIBAgIUBYPmNYODsegtqdC4UMDEqgtLitIwDQYJKoZIhvcNAQEL\nBQAwYzELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhw\ncm9ncmVzczAeFw0yMzA5MTQxNDI2MjhaFw0yNjA5MTMxNDI2MjhaMGMxCzAJBgNV\nBAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow\nGAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIcHJvZ3Jlc3MwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDAVWQtiuUvZlGk0csKpYtFVxLY\nHxL+D2Mec/7IxqGbccmNMsRLXw+ukRzGx0R6ppj5hE6bjuZeihaHFtAMpMBIDauX\nQy12W/0Nkn0yALRrlq6IhHyt+axYZoF60BeEgTFiME/ai8CeyTUz2301oe0rEp58\nPX3Pr1FOmwGkGhXO88cArdkWMblKFxh9fsorhGW50TYrXPg09zpIcX5EnH1tsWv0\nIBjVgUPMWY50wdB7gzNOWbMtuburt/jzuT3oRmWu4OGebclpkgALKuC3xDPMtZ4j\n4w/eiGjm2D4yAYNeVjhwTk2o1DckUUY4WGNFXEaVBVDBT03rz9iaAidHoj7jAgMB\nAAGjUzBRMB0GA1UdDgQWBBT2GQGV/1o07Y0OjBj/PBqYDCHLWTAfBgNVHSMEGDAW\ngBT2GQGV/1o07Y0OjBj/PBqYDCHLWTAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3\nDQEBCwUAA4IBAQATCKTpgj8P3zDMVZuzFg3vpaBZHwiB0pH4ZrKEu4d5fX2rgede\n10WyuJxT0Lwfms0Ou7qxpS6Th6RgBFM0riFk7+lMmIxZvgSO+Kxq9Re1UO6aduon\nPbPFhiTAdhOXT/9NAVStGljpTsrJMbXnVzZL6jUbkXK+cdR2zwW0zTkma6Ja2Ygf\n7bBmv3wOfzde3mw0AMlk9JWmFbIpyNKER4D60x6+F+g7foo4w5+OsNQQYHIL3b2l\n9h48bn2apwAc49l0RHIL0QSBkeklcsCO0H4Es8AwKi1+Q3J+P5Q7HrZie9gIH4D1\nSnWqpKuioaE82pLXRbT9+iWEJdj9mDkkMtbD\n-----END CERTIFICATE-----"
  ssl_cert = "-----BEGIN CERTIFICATE-----\nMIIDdTCCAl2gAwIBAgIUGRIvHV9V+hVgzYl6q2Keemg5vvQwDQYJKoZIhvcNAQEL\nBQAwYzELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhw\ncm9ncmVzczAeFw0yMzA5MTQxNDI2MjhaFw0yNjA5MTMxNDI2MjhaMGMxCzAJBgNV\nBAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow\nGAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzERMA8GA1UEAwwIY2hlZm5vZGUwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDFloOr9RoDqZHf7oRcE1TdrDX0\n9lrTsBGW34IK3Sl5mmPMNWx9PD08FwFBg2SyFfQs3Lmqdq69CSEqTj17TquzlFTj\na/1Sx7/j/2SF/6sp3EVb5F72KAzZFDJFTHLXZTPWK7PSYlYNJstWFOVf63owhzNd\n1xnDnnXw+hJ/6sUf/3jttLUcKICF01JfW3f9bJiCNFnwZrZTUiS2wNFIwEZPYdp8\ntPlrkNvt7I/G4RJUbvsX0ZGbp9GUg358Gm7bOCOFrKQBpWnuQqKN7ota8cGfhaz1\n7MhAJQuCbV0sV/kMFvXW9xCMwZgwGT8/52seHJplT2ICe+mKG6n/enU5U3iVAgMB\nAAGjITAfMB0GA1UdJQQWMBQGCCsGAQUFBwMCBggrBgEFBQcDATANBgkqhkiG9w0B\nAQsFAAOCAQEAHbFSowIZIgp7PJBscxf6yiGfZAkJ40bxPG1jOx1msuPVMqd1aYux\nvgfMiSSB4VgTlfevjt9OduuLitN02oKXFRMXc5WPCoZm6WLFKwTT5S13s0BH4mOr\ntOFm84iMlnvLrlUNbOwdIpjbJVdDIvi/l1kEs9zCHMlMgnXlgRuZWABfciqWSmr0\nBb6vYDNCJ3sfgdBifS1NeX9IJ/yTj9Zs+dEn1tFrljGY8Xg8pTmtf7oChQRnFEKW\ndSUZt/vAkug3u739KzeHfLcwiGDC336PhGoLbDw0x6AsT4BCcwB7jMrOBsBtCK5Z\nw0m2pi4hVt1O1M5A/m3aAIDIPYbhFW7owg==\n-----END CERTIFICATE-----"
  ssl_key = "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDFloOr9RoDqZHf\n7oRcE1TdrDX09lrTsBGW34IK3Sl5mmPMNWx9PD08FwFBg2SyFfQs3Lmqdq69CSEq\nTj17TquzlFTja/1Sx7/j/2SF/6sp3EVb5F72KAzZFDJFTHLXZTPWK7PSYlYNJstW\nFOVf63owhzNd1xnDnnXw+hJ/6sUf/3jttLUcKICF01JfW3f9bJiCNFnwZrZTUiS2\nwNFIwEZPYdp8tPlrkNvt7I/G4RJUbvsX0ZGbp9GUg358Gm7bOCOFrKQBpWnuQqKN\n7ota8cGfhaz17MhAJQuCbV0sV/kMFvXW9xCMwZgwGT8/52seHJplT2ICe+mKG6n/\nenU5U3iVAgMBAAECggEAH0VG6XwM9e9sSshw4jGdCMgscexbS41d+0a8SgPegIRS\nrwr1dIyIFG5/oGKvGRAoaME2EShfV0OOoCdpy44T1oPvO17n5KYAVJEi06I28JUP\n1Q87iDGmduSfYCBNPJGjto2MFAvEGqi9HY6JDrkxyWRcWMmmJjN57v1k2CHLuNhq\nLvxgxRT1Dbh4oTtU5//rYZmFVVZ2Cfl2b4sJivX9o3RRRNklQpPv03f0YbIlRtIw\n3FjYjAG17WQpnMhmtCo6k45eAqomr4OV1BxrMl0Ltx6YLpLXh3zas96h5xQEIdM5\n74oSmseI67VKyLtmqDCvIxJFqtkhAB/0Z5sEdmh0gQKBgQD6Nsx/dwUxJ3HgqHxp\njlFyNeWPuDhq6woiaUUaV2FMNc9zO0Lw2zYOKiVRKhbQ5X+gWXwMf+7L+6nwRSUa\nuXNRDd/t080z+5M0YHJhe80yy3saxAplqgpMPZWqhduPNAo5DSEAn/hDmq7mUb+4\nsS7MdqPPub2MM92hAaXRTIWJOQKBgQDKKC7OPdOVmLDcmBG/3RFkEFumm2gqBtmF\n4aL7Nn3dIxmkVs9HXH5JSY8r0ENrXlynSxO+174boqvFR/0Kw3YRnAJhcg+phECG\nxK4Jt3LMIkZqxhkgevHrL0l8mrCo4qEhbAuE/ppxO7dkVSwnn/y6lnjOrXmncz6d\nSnUSEc32PQKBgQDifVYRD3CQtO7c+EZd2iiZZHYe6ReQmJ881ONrW6tEK/VTjlIi\n6Zr9qWLMHHg4sXUcdcPXILrMxEpopn5WuYXL2e5YPn+iTVhLcf43hbQSBaSybsAm\nlIvrxVdD2xUKhIW4bMzx3twAffVRoLAWA7Sj+cSAVNZiIdS9WFt7oHPD6QKBgQCk\nv+61QgnHZvLsNDpy5JUhuXsX4psXdRGdXG/Yz2Xv7IUfO2gdvjFlRL+bc1UekX5t\nEHB9HZHfL8lRNAPy26zDWSNPiwdcnV2A95TmckzqNByzM9KSd9/kTVtUYzUJzfiH\nJmiU8HGpSoOBDzC28lmjLrIxrYrxfqhOw7l5Cm2R+QKBgFuwyuHD3Xr9H51Ma5l3\nJ2PQzmNcXpev2h+3yW4s5YcSNPMmUusfrueaSDC+sPKin5GTcvY4D0PcHRsyvUPL\nneBKRoED3B9lkbs6ce7cVqmeQhjkT+hIW650MEfpYPvZlgkvAm3RUB+aX0PppGdC\nv993qjI98DUAwRz2p8YIga6S\n-----END PRIVATE KEY-----"

[transport]
  port = 9300
	`
)

var sshConfig = sshutils.SSHConfig{
	SshUser:    "ubuntu",
	SshPort:    "22",
	SshKeyFile: testfile,
	HostIP:     ValidIP,
	Timeout:    150,
}

func TestIsRemotePath(t *testing.T) {
	c := certRotateFlow{fileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription string
		input               string
		expected            bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Valid remote path",
			input:               RemoteFilePath,
			expected:            true,
		},
		{
			testCaseDescription: "Local path instead of remote path",
			input:               LocalFilePath,
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 1",
			input:               "/home/ec2-user/certs/public.pem198.51.100.0",
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 2",
			input:               "198.51.100.0/home/ec2-user/certs/public.pem",
			expected:            false,
		},
		{
			testCaseDescription: "Invalid remote path 3",
			input:               "\n   198.51.100.0:/home/ec2-user/certs/public.pem",
			expected:            false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			actual := c.IsRemotePath(tc.input)
			assert.Equal(t, tc.expected, actual)
		})
	}
}

func TestGetIPV4(t *testing.T) {
	c := certRotateFlow{fileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription string
		input               string
		expected            string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Valid IP V4",
			input:               RemoteFilePath,
			expected:            ValidIP,
		},
		{
			testCaseDescription: "Valid IP V4 but invalid remote path 1",
			input:               "/home/ec2-user/certs/public.pem:127.0.0.1",
			expected:            "127.0.0.1",
		},
		{
			testCaseDescription: "Valid IP V4 but invalid remote path 2",
			input:               "/home/ec2-user/:0.0.0.0:/certs/public.pem",
			expected:            "0.0.0.0",
		},
		{
			testCaseDescription: "Invalid IP v4 and valid path",
			input:               "256.256.256.256:/home/ec2-user/certs/public.pem",
			expected:            "",
		},
		{
			testCaseDescription: "Invalid IP v4 and invalid path",
			input:               "/home/ec2-user/certs/public.pem:1.2.3",
			expected:            "",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			actual := c.GetIPV4(tc.input)
			assert.Equal(t, tc.expected, actual)
		})
	}
}

func TestGetRemoteFileDetails(t *testing.T) {
	c := certRotateFlow{fileUtils: mockFS()}

	type testCaseInfo struct {
		testCaseDescription    string
		input                  string
		isError                bool
		expectedErrorMessage   string
		expectedRemoteFilePath string
		expectedFileName       string
		expectedHostIP         string
		clusterIPs             []string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription:    "Valid remote path",
			input:                  RemoteFilePath,
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: LocalFilePath,
			expectedFileName:       "public.pem",
			expectedHostIP:         ValidIP,
			clusterIPs:             []string{ValidIP},
		},
		{
			testCaseDescription:    "Invalid Remote Path - Local path",
			input:                  LocalFilePath,
			isError:                true,
			expectedErrorMessage:   " is not a valid IPv4 address",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
			clusterIPs:             []string{ValidIP},
		},
		{
			testCaseDescription:    "Invalid Remote Path - Colon missing",
			input:                  "198.51.100.0/home/ec2-user/certs/public.pem",
			isError:                true,
			expectedErrorMessage:   "Invalid remote path: 198.51.100.0/home/ec2-user/certs/public.pem",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
			clusterIPs:             []string{ValidIP},
		},
		{
			testCaseDescription:    "Invalid Remote Path - No filename",
			input:                  "198.51.100.0:/home/ec2-user/certs/public/",
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: "/home/ec2-user/certs/public",
			expectedFileName:       "public",
			expectedHostIP:         ValidIP,
			clusterIPs:             []string{ValidIP},
		},
		{
			testCaseDescription:    "Invalid Remote Path - Reverse",
			input:                  "/home/ec2-user/certs/public/:198.51.100.0",
			isError:                false,
			expectedErrorMessage:   "",
			expectedRemoteFilePath: ValidIP,
			expectedFileName:       ValidIP,
			expectedHostIP:         ValidIP,
			clusterIPs:             []string{ValidIP},
		},
		{
			testCaseDescription:    "Invalid Remote Path - Empty Path",
			input:                  ValidIP + ":",
			isError:                true,
			expectedErrorMessage:   "Invalid remote path: " + ValidIP + ":",
			expectedRemoteFilePath: "",
			expectedFileName:       "",
			expectedHostIP:         "",
			clusterIPs:             []string{ValidIP},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			infra := &AutomateHAInfraDetails{}
			infra.Outputs.AutomatePrivateIps.Value = tc.clusterIPs
			remoteFilePathRes, fileNameRes, hostIPRes, err := c.GetRemoteFileDetails(tc.input, infra)
			if tc.isError {
				assert.Error(t, err)
				assert.Equal(t, tc.expectedErrorMessage, err.Error())
			} else {
				assert.NoError(t, err)
			}
			assert.Equal(t, tc.expectedRemoteFilePath, remoteFilePathRes)
			assert.Equal(t, tc.expectedFileName, fileNameRes)
			assert.Equal(t, tc.expectedHostIP, hostIPRes)
		})
	}
}

func TestGetCerts(t *testing.T) {
	c := certRotateFlow{fileUtils: mockFS()}

	infra := &AutomateHAInfraDetails{}

	type testCaseInfo struct {
		testCaseDescription string
		flagsObj            certRotateFlags
		rootCaWant          string
		publicCertWant      string
		privateCertWant     string
		adminCertWant       string
		adminKeyWant        string
		isError             bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "All paths given and flag is automate service",
			flagsObj: certRotateFlags{
				automate:        true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         false,
		},
		{
			testCaseDescription: "All paths given except root-ca flag is automate service and node flag given",
			flagsObj: certRotateFlags{
				automate:        true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
				node:            "ip-given",
			},
			rootCaWant:      "",
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         false,
		},
		{
			testCaseDescription: "All paths given and flag is opensearch service",
			flagsObj: certRotateFlags{
				opensearch:      true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
				rootCAPath:      ValidCertPath,
				adminCertPath:   ValidCertPath,
				adminKeyPath:    ValidCertPath,
			},
			rootCaWant:      FileContent,
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   FileContent,
			adminKeyWant:    FileContent,
			isError:         false,
		},
		{
			testCaseDescription: "All paths empty and flag for automate service",
			flagsObj:            certRotateFlags{},
			rootCaWant:          "",
			publicCertWant:      "",
			privateCertWant:     "",
			adminCertWant:       "",
			adminKeyWant:        "",
			isError:             true,
		},
		{
			testCaseDescription: "some invalid paths given and flag is automate service",
			flagsObj: certRotateFlags{
				automate:        true,
				privateCertPath: ValidCertPath,
				publicCertPath:  "./xyx-cert.go",
				rootCAPath:      ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "All paths given but invalid (file not exist in (f.s)and flag is automate service",
			flagsObj: certRotateFlags{
				automate:        true,
				privateCertPath: "./xyz.go",
				publicCertPath:  "./xyz.go",
				rootCAPath:      "./xyx.go",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "All paths given except root-ca and flag is automate service",
			flagsObj: certRotateFlags{
				automate:        true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  FileContent,
			privateCertWant: FileContent,
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         false,
		},
		{
			testCaseDescription: "Some mandatory path not given and flag is opensearch service",
			flagsObj: certRotateFlags{
				opensearch:      true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
				rootCAPath:      ValidCertPath,
				adminCertPath:   ValidCertPath,
				adminKeyPath:    "",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "Invalid adminCert path and flag is opensearch service",
			flagsObj: certRotateFlags{
				opensearch:      true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
				rootCAPath:      ValidCertPath,
				adminCertPath:   "./xyz-cert.go",
				adminKeyPath:    ValidCertPath,
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
		{
			testCaseDescription: "Invalid adminKey path and flag is opensearch service",
			flagsObj: certRotateFlags{
				opensearch:      true,
				privateCertPath: ValidCertPath,
				publicCertPath:  ValidCertPath,
				rootCAPath:      ValidCertPath,
				adminCertPath:   ValidCertPath,
				adminKeyPath:    "./xyz-cert.go",
			},
			rootCaWant:      "",
			publicCertWant:  "",
			privateCertWant: "",
			adminCertWant:   "",
			adminKeyWant:    "",
			isError:         true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.testCaseDescription, func(t *testing.T) {
			certsGot, err := c.getCerts(infra, &tc.flagsObj)
			if tc.isError {
				assert.Error(t, err)
				assert.Nil(t, certsGot)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, certsGot)
			}
			if certsGot != nil {
				assert.Equal(t, tc.rootCaWant, certsGot.rootCA)
				assert.Equal(t, tc.publicCertWant, certsGot.publicCert)
				assert.Equal(t, tc.privateCertWant, certsGot.privateCert)
				assert.Equal(t, tc.adminCertWant, certsGot.adminCert)
				assert.Equal(t, tc.adminKeyWant, certsGot.adminKey)
			}
		})
	}
}

func TestCompareCurrentCertsWithNewCerts(t *testing.T) {
	c, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		testCaseDescription string
		remoteService       string
		newCerts            *certificates
		flagsObj            *certRotateFlags
		currentCertsInfo    *certShowCertificates
		skipIpsList         []string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Automate new certs are equal | No node flag",
			remoteService:       AUTOMATE,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				AutomateRootCert: FileContent,
				AutomateCertsByIP: []CertByIP{
					{
						IP:         ValidIP,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP1,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: infra.Outputs.AutomatePrivateIps.Value,
		},
		{
			testCaseDescription: "Automate new certs are different | No node flag",
			remoteService:       AUTOMATE,
			newCerts: &certificates{
				publicCert:  FileContent + "a",
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				AutomateRootCert: FileContent,
				AutomateCertsByIP: []CertByIP{
					{
						IP:         ValidIP,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP1,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Automate new certs are eqaul | Node flag",
			remoteService:       AUTOMATE,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
			},
			flagsObj: &certRotateFlags{
				node: ValidIP1,
			},
			currentCertsInfo: &certShowCertificates{
				AutomateCertsByIP: []CertByIP{
					{
						IP:         ValidIP,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP1,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: []string{ValidIP1},
		},
		{
			testCaseDescription: "Chef-server new certs are equal | No node flag",
			remoteService:       CHEF_SERVER,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				ChefServerCertsByIP: []CertByIP{
					{
						IP:         ValidIP2,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP3,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: infra.Outputs.ChefServerPrivateIps.Value,
		},
		{
			testCaseDescription: "Chef-server new certs are different | No node flag",
			remoteService:       CHEF_SERVER,
			newCerts: &certificates{
				publicCert:  FileContent + "a",
				privateCert: FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				ChefServerCertsByIP: []CertByIP{
					{
						IP:         ValidIP2,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP3,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Opensearch new certs are equals | No node flag",
			remoteService:       OPENSEARCH,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
				adminCert:   FileContent,
				adminKey:    FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				OpensearchAdminCert: FileContent,
				OpensearchAdminKey:  FileContent,
				OpensearchRootCert:  FileContent,
				OpensearchCertsByIP: []CertByIP{
					{
						IP:         ValidIP4,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP5,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP6,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: infra.Outputs.OpensearchPrivateIps.Value,
		},
		{
			testCaseDescription: "Opensearch new certs are different | No node flag",
			remoteService:       OPENSEARCH,
			newCerts: &certificates{
				publicCert:  FileContent + "a",
				privateCert: FileContent,
				rootCA:      FileContent,
				adminCert:   FileContent,
				adminKey:    FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				OpensearchAdminCert: FileContent,
				OpensearchAdminKey:  FileContent,
				OpensearchRootCert:  FileContent,
				OpensearchCertsByIP: []CertByIP{
					{
						IP:         ValidIP4,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP5,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP6,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Postgresql new certs are eqaul | No node flag",
			remoteService:       POSTGRESQL,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				PostgresqlRootCert: FileContent,
				PostgresqlCertsByIP: []CertByIP{
					{
						IP:         ValidIP7,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP8,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP9,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: infra.Outputs.PostgresqlPrivateIps.Value,
		},
		{
			testCaseDescription: "Postgresql new certs are different | No node flag",
			remoteService:       POSTGRESQL,
			newCerts: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent + "a",
				rootCA:      FileContent,
			},
			flagsObj: &certRotateFlags{},
			currentCertsInfo: &certShowCertificates{
				PostgresqlRootCert: FileContent,
				PostgresqlCertsByIP: []CertByIP{
					{
						IP:         ValidIP7,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP8,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
					{
						IP:         ValidIP9,
						PublicKey:  FileContent,
						PrivateKey: FileContent,
					},
				},
			},
			skipIpsList: []string{},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			skipIpsListGot := c.compareCurrentCertsWithNewCerts(testCase.remoteService, testCase.newCerts, testCase.flagsObj, testCase.currentCertsInfo)
			sort.Strings(testCase.skipIpsList)
			sort.Strings(skipIpsListGot)
			assert.Equal(t, testCase.skipIpsList, skipIpsListGot)
		})
	}
}

func TestGetFrontEndIpsForSkippingCnAndRootCaPatching(t *testing.T) {
	c, infra := getMockCertRotateFlowAndInfra()
	nodesDn, _ := getDistinguishedNameFromKey(FileContent)
	newCn := nodesDn.CommonName

	type testCaseInfo struct {
		testCaseDescription string
		newRootCA           string
		newCn               string
		oldCn               string
		oldRootCA           string
		node                string
		infra               *AutomateHAInfraDetails
		isIpSkip            bool
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Comparing old cn nd old rootCA | same cn and root-ca",
			newRootCA:           FileContent,
			oldRootCA:           FileContent,
			newCn:               newCn,
			oldCn:               newCn,
			infra:               infra,
			isIpSkip:            true,
		},
		{
			testCaseDescription: "Comparing old cn nd old rootCA | same root-ca different cn",
			newRootCA:           FileContent,
			oldRootCA:           FileContent,
			newCn:               newCn + "n",
			oldCn:               "",
			infra:               infra,
			isIpSkip:            false,
		},
		{
			testCaseDescription: "Comparing old cn nd old rootCA | different root-ca",
			newRootCA:           FileContent + "a",
			oldRootCA:           FileContent,
			newCn:               newCn,
			oldCn:               newCn,
			infra:               infra,
			isIpSkip:            false,
		},
		{
			testCaseDescription: "Comparing old cn nd old rootCA | different root-ca same cn",
			newRootCA:           FileContent + "a",
			oldRootCA:           FileContent,
			newCn:               newCn,
			oldCn:               newCn,
			infra:               infra,
			isIpSkip:            false,
		},
		{
			testCaseDescription: "Comparing old cn | different cn with node flag",
			newCn:               newCn + "a",
			oldCn:               newCn,
			infra:               infra,
			node:                ValidIP,
			isIpSkip:            false,
		},
		{
			testCaseDescription: "Comparing old cn | same cn with node flag",
			newCn:               newCn,
			oldCn:               newCn,
			infra:               infra,
			node:                ValidIP,
			isIpSkip:            true,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			isIpSkipped := c.getFrontEndIpsForSkippingCnAndRootCaPatching(testCase.newRootCA, testCase.newCn, testCase.oldCn, testCase.oldRootCA, testCase.node)
			assert.Equal(t, testCase.isIpSkip, isIpSkipped)
		})
	}
}

func TestGetFilteredIps(t *testing.T) {
	c, infra := getMockCertRotateFlowAndInfra()

	type testCaseInfo struct {
		serviceIps          []string
		skipIpsList         []string
		filteredIpsExpected []string
	}

	testCases := []testCaseInfo{
		{
			serviceIps:          infra.Outputs.OpensearchPrivateIps.Value,
			skipIpsList:         []string{ValidIP5},
			filteredIpsExpected: []string{ValidIP4, ValidIP6},
		},
		{
			serviceIps:          infra.Outputs.OpensearchPrivateIps.Value,
			skipIpsList:         []string{},
			filteredIpsExpected: infra.Outputs.OpensearchPrivateIps.Value,
		},
		{
			serviceIps:          infra.Outputs.OpensearchPrivateIps.Value,
			skipIpsList:         infra.Outputs.OpensearchPrivateIps.Value,
			filteredIpsExpected: []string{},
		},
	}

	for _, testCase := range testCases {
		t.Run("TestingGetFilteredIps", func(t *testing.T) {
			filteredIpsGot := c.getFilteredIps(testCase.serviceIps, testCase.skipIpsList)
			assert.Equal(t, testCase.filteredIpsExpected, filteredIpsGot)
		})
	}
}

func TestGetFrontIpsToSkipRootCAandCNPatchingForOs(t *testing.T) {
	c, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		testCaseDescription string
		automatesConfig     *deployment.AutomateConfig
		newRootCA           string
		oldRootCA           string
		newCn               string
		oldCn               string
		node                string
		infra               *AutomateHAInfraDetails
		skipIpsList         []string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Opensearch root-ca and cn patching | same rootca and CN ",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{
							Opensearch: &shared.External_Opensearch{
								Ssl: &shared.External_Opensearch_SSL{
									ServerName: &wrapperspb.StringValue{
										Value: "chefnode",
									},
									RootCert: &wrapperspb.StringValue{
										Value: FileContent,
									},
								},
							},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent,
			newCn:       "chefnode",
			oldCn:       "chefnode",
			infra:       infra,
			skipIpsList: []string{ValidIP},
		},
		{
			testCaseDescription: "Opensearch root-ca and cn patching | diff rootca and CN",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{Opensearch: &shared.External_Opensearch{
							Ssl: &shared.External_Opensearch_SSL{
								ServerName: &wrapperspb.StringValue{
									Value: "chefnode",
								},
								RootCert: &wrapperspb.StringValue{
									Value: FileContent,
								},
							},
						},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent + "a",
			newCn:       "chefnode1",
			oldCn:       "chefnode",
			infra:       infra,
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Opensearch root-ca and cn patching | same rootca and diff CN",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{Opensearch: &shared.External_Opensearch{
							Ssl: &shared.External_Opensearch_SSL{
								ServerName: &wrapperspb.StringValue{
									Value: "chefnode",
								},
								RootCert: &wrapperspb.StringValue{
									Value: FileContent,
								},
							},
						},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent,
			newCn:       "chefnode1",
			oldCn:       "chefnode",
			infra:       infra,
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Opensearch root-ca and cn patching | diff rootca and same CN",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{Opensearch: &shared.External_Opensearch{
							Ssl: &shared.External_Opensearch_SSL{
								ServerName: &wrapperspb.StringValue{
									Value: "chefnode",
								},
								RootCert: &wrapperspb.StringValue{
									Value: FileContent,
								},
							},
						},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent + "a",
			newCn:       "chefnode",
			oldCn:       "chefnode",
			infra:       infra,
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Opensearch root-ca and cn patching | diff CN with node flag",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{Opensearch: &shared.External_Opensearch{
							Ssl: &shared.External_Opensearch_SSL{
								ServerName: &wrapperspb.StringValue{
									Value: "chefnode",
								},
							},
						},
						},
					},
				},
			},
			newCn:       "chefnode1",
			oldCn:       "chefnode",
			node:        ValidIP,
			infra:       infra,
			skipIpsList: []string{},
		},
		{
			testCaseDescription: "Opensearch root-ca and cn patching | same CN with node flag",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{Opensearch: &shared.External_Opensearch{
							Ssl: &shared.External_Opensearch_SSL{
								ServerName: &wrapperspb.StringValue{
									Value: "chefnode",
								},
							},
						},
						},
					},
				},
			},
			newCn:       "chefnode",
			oldCn:       "chefnode",
			node:        ValidIP,
			infra:       infra,
			skipIpsList: []string{ValidIP},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			configMap := map[string]*deployment.AutomateConfig{
				ValidIP: testCase.automatesConfig,
			}
			skipIpsListGot := c.getFrontIpsToSkipRootCAandCNPatchingForOs(configMap, testCase.newRootCA, testCase.newCn, testCase.node, infra)
			sort.Strings(testCase.skipIpsList)
			sort.Strings(skipIpsListGot)
			assert.Equal(t, testCase.skipIpsList, skipIpsListGot)

		})
	}
}

func TestGetFrontendIPsToSkipRootCAPatchingForPg(t *testing.T) {
	c, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		testCaseDescription string
		automatesConfig     *deployment.AutomateConfig
		newRootCA           string
		oldRootCA           string
		infra               *AutomateHAInfraDetails
		skipIpsList         []string
	}

	testCases := []testCaseInfo{
		{
			testCaseDescription: "Postgresql root-ca comparing | same rootca ",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{
							Postgresql: &shared.External_Postgresql{
								Ssl: &shared.External_Postgresql_SSL{
									RootCert: &wrapperspb.StringValue{
										Value: FileContent,
									},
								},
							},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent,
			infra:       infra,
			skipIpsList: []string{ValidIP},
		},
		{
			testCaseDescription: "Postgresql root-ca comparing | diff rootca ",
			automatesConfig: &deployment.AutomateConfig{
				Global: &shared.GlobalConfig{
					V1: &shared.V1{
						External: &shared.External{
							Postgresql: &shared.External_Postgresql{
								Ssl: &shared.External_Postgresql_SSL{
									RootCert: &wrapperspb.StringValue{
										Value: FileContent,
									},
								},
							},
						},
					},
				},
			},
			oldRootCA:   FileContent,
			newRootCA:   FileContent + "a",
			infra:       infra,
			skipIpsList: []string{},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			configMap := map[string]*deployment.AutomateConfig{
				ValidIP: testCase.automatesConfig,
			}
			skipIpsListGot := c.getFrontendIPsToSkipRootCAPatchingForPg(configMap, testCase.newRootCA, infra)
			sort.Strings(testCase.skipIpsList)
			sort.Strings(skipIpsListGot)
			assert.Equal(t, testCase.skipIpsList, skipIpsListGot)

		})
	}
}

func TestGetSkipIpsListForPgRootCAPatching(t *testing.T) {
	infra := NewMockInfra()
	c := NewCertRotate()

	type testCaseInfo struct {
		description     string
		sshUtil         SSHUtil
		certs           *certificates
		infra           *AutomateHAInfraDetails
		MockPullConfigs PullConfigs
		skipIpsList     []string
		ExpectedError   string
		wantError       bool
	}
	testCases := []testCaseInfo{
		{
			description: "same root ca",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			infra: infra,
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP7, ValidIP7},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "Different rootca",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent + "a",
			},
			infra: infra,
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "Error while pulling chefserver config",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			infra: infra,
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return nil, nil, errors.New("ERROR")
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "ERROR",
			wantError:     true,
		},
		{
			description: "Error while pulling Automate config",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			infra: infra,
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return nil, nil, errors.New("ERROR")
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return nil, nil, errors.New("ERROR")
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "ERROR",
			wantError:     true,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c.pullConfigs = testCase.MockPullConfigs
			skipIpsListGot, err := c.getSkipIpsListForPgRootCAPatching(testCase.infra, testCase.sshUtil, testCase.certs)
			if !testCase.wantError {
				sort.Strings(testCase.skipIpsList)
				sort.Strings(skipIpsListGot)
				assert.Equal(t, testCase.skipIpsList, skipIpsListGot)
			} else {
				assert.Error(t, err)
			}

		})
	}
}

func TestGetSkipIpsListForOsRootCACNPatching(t *testing.T) {
	infra := NewMockInfra()
	c := NewCertRotate()

	type testCaseInfo struct {
		description     string
		sshUtil         SSHUtil
		certs           *certificates
		infra           *AutomateHAInfraDetails
		MockPullConfigs PullConfigs
		nodesCn         string
		flagsObj        *certRotateFlags
		skipIpsList     []string
		ExpectedError   string
		wantError       bool
	}
	testCases := []testCaseInfo{
		{
			description: "same root ca and cn in all ips",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent,
			},
			infra:    infra,
			nodesCn:  "chefnode",
			flagsObj: &certRotateFlags{},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP, ValidIP1},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "same root ca in all ips but different cn in one ip",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent,
			},
			infra:    infra,
			nodesCn:  "chefnode",
			flagsObj: &certRotateFlags{},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "diff root ca and cn in all ips",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent + "a",
			},
			infra:    infra,
			nodesCn:  "chefnodee",
			flagsObj: &certRotateFlags{},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "same CN with node flag",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent,
			},
			infra: infra,
			flagsObj: &certRotateFlags{
				node: ValidIP4,
			},
			nodesCn: "chefnode",
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP, ValidIP1},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "diff CN with node flag",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent + "a",
			},
			infra: infra,
			flagsObj: &certRotateFlags{
				node: ValidIP4,
			},
			nodesCn: "chefnode",
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "Error while fetching chefserver config",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			infra: infra,
			flagsObj: &certRotateFlags{
				node: ValidIP4,
			},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return nil, nil, errors.New("ERROR")
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "ERROR",
			wantError:     true,
		},
		{
			description: "Error while fetching automate config",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				publicCert:  FileContent,
				privateCert: FileContent,
				rootCA:      FileContent,
			},
			infra: infra,
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return nil, nil, errors.New("ERROR")
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP7: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Postgresql: &shared.External_Postgresql{
											Ssl: &shared.External_Postgresql_SSL{
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{},
			ExpectedError: "ERROR",
			wantError:     true,
		},
		{
			description: "same root ca and cn in one of the automate and chefserver ips ",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent,
			},
			infra:    infra,
			nodesCn:  "chefnode",
			flagsObj: &certRotateFlags{},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent + "a",
												},
											},
										},
									},
								},
							},
						},
						ValidIP1: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP2: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
						ValidIP3: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent + "a",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP1, ValidIP2},
			ExpectedError: "",
			wantError:     false,
		},
		{
			description: "same rootca and cn in chefserver",
			sshUtil:     GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			certs: &certificates{
				rootCA: FileContent,
			},
			infra:    infra,
			nodesCn:  "chefnode",
			flagsObj: &certRotateFlags{},
			MockPullConfigs: &MockPullConfigs{
				pullAutomateConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnodee",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent + "a",
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
				pullChefServerConfigsFunc: func(removeUnreachableNodes bool) (map[string]*deployment.AutomateConfig, []string, error) {
					return map[string]*deployment.AutomateConfig{
						ValidIP2: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
						ValidIP3: {
							Global: &shared.GlobalConfig{
								V1: &shared.V1{
									External: &shared.External{
										Opensearch: &shared.External_Opensearch{
											Ssl: &shared.External_Opensearch_SSL{
												ServerName: &wrapperspb.StringValue{
													Value: "chefnode",
												},
												RootCert: &wrapperspb.StringValue{
													Value: FileContent,
												},
											},
										},
									},
								},
							},
						},
					}, nil, nil
				},
			},

			skipIpsList:   []string{ValidIP2, ValidIP3},
			ExpectedError: "",
			wantError:     false,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c.pullConfigs = testCase.MockPullConfigs
			skipIpsListGot, err := c.getSkipIpsListForOsRootCACNPatching(testCase.infra, testCase.sshUtil, testCase.certs, testCase.nodesCn, testCase.flagsObj)
			sort.Strings(testCase.skipIpsList)
			sort.Strings(skipIpsListGot)
			if !testCase.wantError {
				assert.Equal(t, testCase.skipIpsList, skipIpsListGot)
			} else {
				assert.Error(t, err)
			}

		})
	}
}

func TestPatchConfig(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description   string
		param         *patchFnParameters
		MockSSHUtil   sshutils.SSHUtil
		isError       bool
		ExpectedError string
	}
	testCases := []testCaseInfo{
		{
			description: "Test Case to rotate on backend",
			param: &patchFnParameters{
				sshUtil:       getMockSSHUtil(&SSHConfig{}, nil, "", nil),
				config:        TestOpensearchAdminAndRootCA,
				fileName:      "cert-rotate-os.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: OPENSEARCH,
				concurrent:    false,
				infra:         infra,
				flagsObj: &certRotateFlags{
					opensearch: true,
				},
				skipIpsList: []string{},
			},
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			isError:       false,
			ExpectedError: "",
		},
		{
			description: "Test Case to rotate on frontend",
			param: &patchFnParameters{
				sshUtil:       getMockSSHUtil(&SSHConfig{}, nil, "", nil),
				config:        TestFrontendConfig,
				fileName:      "cert-rotate-fe.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: AUTOMATE,
				concurrent:    true,
				infra:         infra,
				flagsObj: &certRotateFlags{
					automate: true,
				},
				skipIpsList: []string{},
			},
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			isError:       false,
			ExpectedError: "",
		},
		{
			description: "remote copying failed on on frontend",
			param: &patchFnParameters{
				config:        TestFrontendConfig,
				fileName:      "cert-rotate-fe.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: AUTOMATE,
				concurrent:    true,
				infra:         infra,
				flagsObj: &certRotateFlags{
					automate: true,
				},
				skipIpsList: []string{},
			},
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  errors.New("remote copying failed on node"),
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
			},
			isError:       true,
			ExpectedError: "remote copying failed on node",
		},
		{
			description: "remote execution failed on on frontend",
			param: &patchFnParameters{
				config:        TestFrontendConfig,
				fileName:      "cert-rotate-fe.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: AUTOMATE,
				concurrent:    true,
				infra:         infra,
				flagsObj: &certRotateFlags{
					automate: true,
				},
				skipIpsList: []string{},
			},
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  errors.New("remote execution failed"),
							Output: "",
						},
					}
				},
			},
			isError:       false,
			ExpectedError: "",
		},
		{
			description: "Deployment service error on frontend",
			param: &patchFnParameters{
				config:        TestFrontendConfig,
				fileName:      "cert-rotate-fe.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: AUTOMATE,
				concurrent:    true,
				infra:         infra,
				flagsObj: &certRotateFlags{
					automate: true,
				},
				skipIpsList: []string{},
			},
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "DeploymentServiceCallError",
						},
					}
				},
			},
			isError:       false,
			ExpectedError: "",
		},
		{
			description: "Error occured while reading infra details on backend",
			param: &patchFnParameters{
				sshUtil:       getMockSSHUtil(&SSHConfig{}, nil, "", errors.Errorf("Error occured while reading infra details")),
				config:        TestOpensearchAdminAndRootCA,
				fileName:      "cert-rotate-os.toml",
				timestamp:     time.Now().Format("20060102150405"),
				remoteService: OPENSEARCH,
				concurrent:    false,
				infra:         infra,
				flagsObj: &certRotateFlags{
					opensearch: true,
				},
				skipIpsList: []string{},
			},
			isError:       true,
			ExpectedError: "Error occured while reading infra details",
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl()}
			testDir := t.TempDir()
			fPath := filepath.Join(testDir, testCase.param.fileName)
			testCase.param.fileName = fPath
			output := c.patchConfig(testCase.param, true)
			if testCase.isError {
				assert.EqualError(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func getMockCertRotateFlowAndInfra() (*certRotateFlow, *AutomateHAInfraDetails) {
	return NewCertRotate(), NewMockInfra()
}

func NewMockInfra() *AutomateHAInfraDetails {
	infra := &AutomateHAInfraDetails{}
	infra.Outputs.AutomatePrivateIps.Value = []string{ValidIP, ValidIP1}
	infra.Outputs.ChefServerPrivateIps.Value = []string{ValidIP2, ValidIP3}
	infra.Outputs.OpensearchPrivateIps.Value = []string{ValidIP4, ValidIP5, ValidIP6}
	infra.Outputs.PostgresqlPrivateIps.Value = []string{ValidIP7, ValidIP8, ValidIP9}
	infra.Outputs.SSHUser.Value = "ubuntu"
	infra.Outputs.SSHKeyFile.Value = "new.pem"
	infra.Outputs.SSHPort.Value = "22"
	return infra
}

func NewCertRotate() *certRotateFlow {
	log, _ := logger.NewLogger("text", "debug")
	c := NewCertRotateFlow(mockFS(), &sshutils.MockSSHUtilsImpl{}, writer, &MockPullConfigs{}, log, &NodeUtilsImpl{})
	return c
}

func mockFS() *fileutils.MockFileSystemUtils {
	return &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filename string) ([]byte, error) {
			if _, err := os.Stat(filename); err == nil {
				// path/to/whatever exists
				return []byte(FileContent), nil
			} else if errors.Is(err, os.ErrNotExist) {
				// path/to/whatever does *not* exist
				return []byte{}, err
			} else {
				return []byte{}, err
			}
		},
		WriteFileFunc: func(filename string, data []byte, prem fs.FileMode) error {
			if len(filename) > 0 {
				// path/to/whatever exists
				return nil
			} else {
				errors.New("File not found")
			}
			return nil
		},
	}
}

func TestGetIPS(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description   string
		inf           *AutomateHAInfraDetails
		nodeType      string
		isError       bool
		ExpectedError string
	}
	testCases := []testCaseInfo{
		{
			description: "Test Case to get IP of Automate node",
			inf:         infra,
			nodeType:    AUTOMATE,
			isError:     false,
		},
		{
			description: "Test Case to get IP of Chef Server node",
			inf:         infra,
			nodeType:    CHEF_SERVER,
			isError:     false,
		},
		{
			description: "Test Case to get IP of Postgresql node",
			inf:         infra,
			nodeType:    POSTGRESQL,
			isError:     false,
		},
		{
			description: "Test Case to get IP of Opensearch node",
			inf:         infra,
			nodeType:    OPENSEARCH,
			isError:     false,
		},
		{
			description: "Test Case to get Dummy Server of Automate node",
			inf:         infra,
			nodeType:    "DummyServer",
			isError:     true,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			output := getIPS(testCase.inf, testCase.nodeType)
			if testCase.isError {
				assert.Empty(t, output, "Invalid server name")
			} else {
				assert.NotEmpty(t, output, "Get IPs for "+testCase.nodeType)
			}
		})
	}
}

func TestPopulateCertificateConfig(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()

	t.Run("get certificate toml", func(t *testing.T) {
		err, output := populateCertificateConfig(infra)
		if err != nil {
			assert.Error(t, err, "Error in populating certs")
		} else {
			assert.NotNil(t, output, "got populated certificates")
		}
	})
}

func TestGetCertsFromTemplate(t *testing.T) {
	type testCaseInfo struct {
		description   string
		filepath      string
		isError       bool
		ExpectedError string
	}
	testCases := []testCaseInfo{
		{
			description:   "get to certificates from correct template file path",
			filepath:      "../../pkg/testfiles/onprem/certs-config.toml",
			isError:       false,
			ExpectedError: "",
		},
		{
			description:   "get to certificates from incorrect template file path",
			filepath:      "../../pkg/testfiles/onprem/certs-config1.toml",
			isError:       true,
			ExpectedError: "Error in fetching certificates from template file",
		},
		{
			description:   "get to certificates from empty template file path",
			filepath:      "",
			isError:       true,
			ExpectedError: "Cluster certificate file is required",
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			output, err := getCertsFromTemplate(testCase.filepath)
			if testCase.isError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.ObjectsAreEqual(mockCertifiateTemplate(), output)
			}
		})
	}

}

func TestWriteCertificateConfigToFile(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description   string
		inf           *AutomateHAInfraDetails
		args          []string
		certTemplate  *CertificateToml
		fileUtil      fileutils.FileUtils
		isError       bool
		ExpectedError string
	}
	testCases := []testCaseInfo{
		{
			description:   "Test to generate file",
			inf:           infra,
			args:          []string{"cert-config.toml"},
			certTemplate:  mockCertifiateTemplate(),
			fileUtil:      mockFS(),
			isError:       false,
			ExpectedError: "",
		},
		{
			description:   "Test to generate file with empty file name",
			inf:           infra,
			args:          []string{},
			certTemplate:  mockCertifiateTemplate(),
			fileUtil:      mockFS(),
			isError:       true,
			ExpectedError: "command need a output file name like cert-config.toml",
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			err := writeCertificateConfigToFile(testCase.inf, testCase.args, testCase.certTemplate, testCase.fileUtil)
			if testCase.isError {
				assert.EqualError(t, err, testCase.ExpectedError)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestRotateClusterFrontendCertificates(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				privateCertPath: private_cert_path,
				publicCertPath:  public_cert_path,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
					return "", nil
				},
			},
			isError:       true,
			ExpectedError: "No  IPs are found",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl()}
			output := c.rotateClusterFrontendCertificates(testCase.inf, testCase.sshutil, testCase.flagsObj, testCase.currentCertsInfo, testCase.certToml)
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func TestRotateChefServerNodeCerts(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				privateCertPath: private_cert_path,
				publicCertPath:  public_cert_path,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
					return "", nil
				},
			},
			isError:       false,
			ExpectedError: "Please Enter Valid chef_server IP",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl()}
			output := c.rotateChefServerNodeCerts(testCase.inf, testCase.sshutil, testCase.currentCertsInfo, testCase.certToml, &testCase.certToml.ChefServer.IPS[0])
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func TestRotateAutomateNodeCerts(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				privateCertPath: private_cert_path,
				publicCertPath:  public_cert_path,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
					return "", nil
				},
			},
			isError:       false,
			ExpectedError: "Please Enter Valid opensearch IP",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl()}
			output := c.rotateAutomateNodeCerts(testCase.inf, testCase.sshutil, testCase.currentCertsInfo, testCase.certToml, &testCase.certToml.Automate.IPS[0])
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func TestRotatePGNodeCerts(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	log, _ := logger.NewLogger("text", "info")
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				privateCertPath: private_cert_path,
				publicCertPath:  public_cert_path,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
					return "", nil
				},
			},
			isError:       false,
			ExpectedError: "Please Enter Valid postgresql IP",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl(),
				log:     log,
			}
			output := c.rotatePGNodeCerts(testCase.inf, testCase.sshutil, testCase.currentCertsInfo, testCase.certToml.PostgreSQL.RootCA, &testCase.certToml.PostgreSQL.IPS[0], true)
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func TestRotateOSNodeCerts(t *testing.T) {
	log, _ := logger.NewLogger("text", "info")
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				privateCertPath: private_cert_path,
				publicCertPath:  public_cert_path,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil: &sshutils.MockSSHUtilsImpl{
				CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
					return []sshutils.Result{
						{
							HostIP: "",
							Error:  nil,
							Output: "",
						},
					}
				},
				Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
					return "", nil
				},
			},
			isError:       true,
			ExpectedError: "Near line 1 (last key parsed 'config'): expected key separator '=', but got 's' instead",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl(), log: log}
			output := c.rotateOSNodeCerts(testCase.inf, testCase.sshutil, testCase.currentCertsInfo, &testCase.certToml.OpenSearch, &testCase.certToml.OpenSearch.IPS[0], false)
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func TestCertRotateFromTemplate(t *testing.T) {
	log, _ := logger.NewLogger("text", "info")
	_, infra := getMockCertRotateFlowAndInfra()
	mockSSHUtils := &sshutils.MockSSHUtilsImpl{
		CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
			return "", nil
		},
	}
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		node:         fmt.Sprintf("%s,%s,%s,%s", ValidIP, ValidIP3, ValidIP5, ValidIP8),
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})
	type testCaseInfo struct {
		description      string
		certFileName     string
		inf              *AutomateHAInfraDetails
		currentCertsInfo *certShowCertificates
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		statusSummary    StatusSummary
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description:      "Rotate only frontend Certs",
			inf:              infra,
			certFileName:     "../../pkg/testfiles/onprem/certs-config_only_frontend.toml",
			currentCertsInfo: mockCertShowCertificates(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mockSSHUtils,
			statusSummary:    statusSummary,
			isError:          false,
			ExpectedError:    "",
		},
		{
			description:      "Rotate Cluster Certs without OpenSearch certs",
			inf:              infra,
			certFileName:     "../../pkg/testfiles/onprem/certs-config_without_opensearch.toml",
			currentCertsInfo: mockCertShowCertificates(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mockSSHUtils,
			statusSummary:    statusSummary,
			isError:          false,
			ExpectedError:    "",
		},
		{
			description:      "Rotate Cluster Certs without PostgreSQL certs",
			inf:              infra,
			certFileName:     "../../pkg/testfiles/onprem/certs-config_without_pg.toml",
			currentCertsInfo: mockCertShowCertificates(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, OPENSEARCH_USER_TOML, nil, "", nil),
			MockSSHUtil:      mockSSHUtils,
			statusSummary:    statusSummary,
			isError:          false,
			ExpectedError:    "",
		},
		{
			description:      "Rotate all Cluster Certs",
			inf:              infra,
			certFileName:     "../../pkg/testfiles/onprem/certs-config.toml",
			currentCertsInfo: mockCertShowCertificates(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, OPENSEARCH_USER_TOML, nil, "", nil),
			MockSSHUtil:      mockSSHUtils,
			statusSummary:    statusSummary,
			isError:          false,
			ExpectedError:    "",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{
				nodeUtils: &MockNodeUtilsImpl{
					postPGCertRotateFunc: func(pgIps []string, sshconfig SSHConfig, fileUtils fileutils.FileUtils, log logger.Logger) error {
						return nil
					},
				},
				fileUtils: &fileutils.MockFileSystemUtils{
					ReadFileFunc: func(filepath string) ([]byte, error) {
						if strings.Contains(filepath, "a2ha_manifest.auto.tfvars") {
							return []byte(`elasticsidecar_pkg_ident = "chef/automate-ha-elasticsidecar/0.1.0/20240725174312"
	proxy_pkg_ident = "chef/automate-ha-haproxy/2.2.14/20240725171322"
	opensearch_pkg_ident = "chef/automate-ha-opensearch/1.3.14/20240725173732"
	pgleaderchk_pkg_ident = "chef/automate-ha-pgleaderchk/0.1.0/20240725172920"
	postgresql_pkg_ident = "chef/automate-ha-postgresql/13.18.0/20241203070217"`), nil
						}
						fu := fileutils.FileSystemUtils{}
						return fu.ReadFile(filepath)
					},
					CreateTempFileFunc: func(content, filename, dir string) (string, error) {
						return filename, nil
					},
					RemoveFileFunc: func(filename string) error {
						return nil
					},
				},
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl(), log: log}
			output := c.certRotateFromTemplate(testCase.certFileName, testCase.sshutil, testCase.inf, testCase.currentCertsInfo, testCase.statusSummary, false, 0, &certRotateFlags{})
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
			}
		})
	}
}

func mockCertifiateTemplate() *CertificateToml {
	return &CertificateToml{
		Automate: NodeCertficate{
			RootCA:          "../../pkg/testfiles/certs/test_root_ca.pem",
			AdminPublickey:  "",
			AdminPrivateKey: "",
			IPS: []IP{
				{
					IP:         ValidIP,
					Publickey:  "../../pkg/testfiles/certs/test_a2_public_key_1.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_a2_private_key_1.pem",
				},
				{
					IP:         ValidIP1,
					Publickey:  "../../pkg/testfiles/certs/test_a2_public_key_2.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_a2_private_key_2.pem",
				},
			},
		},
		ChefServer: NodeCertficate{
			RootCA:          "../../pkg/testfiles/certs/test_root_ca.pem",
			AdminPublickey:  "",
			AdminPrivateKey: "",
			IPS: []IP{
				{
					IP:         ValidIP2,
					Publickey:  "../../pkg/testfiles/certs/test_cs_public_key_1.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_cs_private_key_1.pem",
				},
				{
					IP:         ValidIP3,
					Publickey:  "../../pkg/testfiles/certs/test_cs_public_key_2.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_cs_private_key_2.pem",
				},
			},
		},
		OpenSearch: NodeCertficate{
			RootCA:          "../../pkg/testfiles/certs/test_root_ca.pem",
			AdminPublickey:  "../../pkg/testfiles/certs/test_admin_cert.pem",
			AdminPrivateKey: "../../pkg/testfiles/certs/test_admin_key.pem",
			IPS: []IP{
				{
					IP:         ValidIP4,
					Publickey:  "../../pkg/testfiles/certs/test_os_public_key_1.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_os_private_key_1.pem",
				},
				{
					IP:         ValidIP5,
					Publickey:  "../../pkg/testfiles/certs/test_os_public_key_2.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_os_private_key_2.pem",
				},
				{
					IP:         ValidIP6,
					Publickey:  "../../pkg/testfiles/certs/test_os_public_key_3.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_os_private_key_3.pem",
				},
			},
		},
		PostgreSQL: NodeCertficate{
			RootCA:          "../../pkg/testfiles/certs/test_root_ca.pem",
			AdminPublickey:  "",
			AdminPrivateKey: "",
			IPS: []IP{
				{
					IP:         ValidIP7,
					Publickey:  "../../pkg/testfiles/certs/test_pg_public_key_1.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_pg_private_key_1.pem",
				},
				{
					IP:         ValidIP8,
					Publickey:  "../../pkg/testfiles/certs/test_pg_public_key_2.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_pg_private_key_2.pem",
				},
				{
					IP:         ValidIP9,
					Publickey:  "../../pkg/testfiles/certs/test_pg_public_key_3.pem",
					PrivateKey: "../../pkg/testfiles/certs/test_pg_private_key_3.pem",
				},
			},
		},
	}
}

func mockCertShowCertificates() *certShowCertificates {
	return &certShowCertificates{
		AutomateRootCert:    rootCA,
		PostgresqlRootCert:  rootCA,
		OpensearchRootCert:  rootCA,
		OpensearchAdminCert: admin_cert,
		OpensearchAdminKey:  admin_key,
		AutomateCertsByIP: []CertByIP{
			{
				IP:         ValidIP,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
			{
				IP:         ValidIP1,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
		},
		ChefServerCertsByIP: []CertByIP{
			{
				IP:         ValidIP2,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
			{
				IP:         ValidIP3,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
		},
		OpensearchCertsByIP: []CertByIP{
			{
				IP:         ValidIP4,
				PublicKey:  public_key,
				PrivateKey: private_key,
				NodesDn:    "test_node_dn",
			},
			{
				IP:         ValidIP5,
				PublicKey:  public_key,
				PrivateKey: private_key,
				NodesDn:    "test_node_dn",
			},
			{
				IP:         ValidIP6,
				PublicKey:  public_key,
				PrivateKey: private_key,
				NodesDn:    "test_node_dn",
			},
		},
		PostgresqlCertsByIP: []CertByIP{
			{
				IP:         ValidIP7,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
			{
				IP:         ValidIP8,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
			{
				IP:         ValidIP9,
				PublicKey:  public_key,
				PrivateKey: private_key,
			},
		},
	}
}

func mockCerts() *certificates {
	return &certificates{
		privateCert: private_key,
		publicCert:  public_key,
		rootCA:      rootCA,
		adminCert:   admin_cert,
		adminKey:    admin_key,
	}
}

func TestValidateCertificateTemplate(t *testing.T) {
	log, _ := logger.NewLogger("text", "info")
	_, infra := getMockCertRotateFlowAndInfra()
	certificateTemplate := mockCertifiateTemplate()
	mockSSHUtils := &sshutils.MockSSHUtilsImpl{
		CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
			return "", nil
		},
	}
	statusSummary := NewStatusSummary(infra, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{
		node:         fmt.Sprintf("%s,%s,%s,%s", ValidIP, ValidIP3, ValidIP5, ValidIP8),
		isAutomate:   true,
		isChefServer: true,
		isOpenSearch: true,
		isPostgresql: true,
	}, &MockRemoteCmdExecutor{
		ExecuteWithNodeMapFunc: func(nodeMap *NodeTypeAndCmd) (map[string][]*CmdResult, error) {
			return nil, nil
		},
	})
	type testCaseInfo struct {
		description      string
		certFileName     string
		inf              *AutomateHAInfraDetails
		currentCertsInfo *certShowCertificates
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		statusSummary    StatusSummary
		isError          bool
		ExpectedError    string
	}
	testCases := []testCaseInfo{
		{
			description:      "Rotate only frontend Certs",
			inf:              infra,
			certFileName:     "../../pkg/testfiles/onprem/certs-config_only_frontend.toml",
			currentCertsInfo: mockCertShowCertificates(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mockSSHUtils,
			statusSummary:    statusSummary,
			isError:          false,
			ExpectedError:    "",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: &fileutils.FileSystemUtils{},
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl(), log: log}
			output := c.validateCertificateTemplate(certificateTemplate, testCase.inf)
			fmt.Println(output)
			if testCase.isError && len(output) >= 1 {
				assert.Error(t, output[0], testCase.ExpectedError)
			} else {
				assert.NoError(t, nil)
			}
		})
	}
}

func TestGetScriptCommandsNoRestart(t *testing.T) {
	type testCaseInfo struct {
		patchParams *patchFnParameters
		expectedRes string
	}
	testCases := []testCaseInfo{
		{
			patchParams: &patchFnParameters{
				remoteService: AUTOMATE,
				timestamp:     "test-time_automate",
			},
			expectedRes: "\n\tsudo chef-automate config patch /tmp/automatetest-time_automate;\n\texport TIMESTAMP=$(date +'%Y%m%d%H%M%S');\n\tsudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;\n\tsudo chef-automate config show > sudo /etc/chef-automate/config.toml",
		},
		{
			patchParams: &patchFnParameters{
				remoteService: CHEF_SERVER,
				timestamp:     "test-time_chef_server",
			},
			expectedRes: "\n\tsudo chef-automate config patch /tmp/chef_servertest-time_chef_server;\n\texport TIMESTAMP=$(date +'%Y%m%d%H%M%S');\n\tsudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;\n\tsudo chef-automate config show > sudo /etc/chef-automate/config.toml",
		},
		{
			patchParams: &patchFnParameters{
				remoteService: "frontend",
				timestamp:     "test-time_frontend",
			},
			expectedRes: "\n\tsudo chef-automate config patch /tmp/frontendtest-time_frontend;\n\texport TIMESTAMP=$(date +'%Y%m%d%H%M%S');\n\tsudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;\n\tsudo chef-automate config show > sudo /etc/chef-automate/config.toml",
		},
		{
			patchParams: &patchFnParameters{
				remoteService: POSTGRESQL,
				timestamp:     "test-time_postgresql",
			},
			expectedRes: "\n\techo \"y\" | sudo cp /tmp/postgresqltest-time_postgresql /hab/user/automate-ha-postgresql/config/user.toml\n\t",
		},
		{
			patchParams: &patchFnParameters{
				remoteService: OPENSEARCH,
				timestamp:     "test-time_opensearch",
			},
			expectedRes: "\n\techo \"y\" | sudo cp /tmp/opensearchtest-time_opensearch /hab/user/automate-ha-opensearch/config/user.toml\n\t",
		},
	}

	for _, tests := range testCases {
		command := getScriptCommandsNoRestart(tests.patchParams, "")
		assert.Equal(t, tests.expectedRes, command)
	}

}

func TestRotateClusterPGOSRootFrontendCertificates(t *testing.T) {
	_, infra := getMockCertRotateFlowAndInfra()
	type testCaseInfo struct {
		description      string
		inf              *AutomateHAInfraDetails
		flagsObj         certRotateFlags
		currentCertsInfo *certShowCertificates
		certToml         *CertificateToml
		MockSSHUtil      sshutils.SSHUtil
		sshutil          SSHUtil
		isError          bool
		ExpectedError    string
		FilterIps        []IP
		ExpectedPatch    []string
	}
	mocksshUtils := &sshutils.MockSSHUtilsImpl{
		CopyFileToRemoteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, srcFilePath string, destFileName string, destDir string, removeFile bool, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		ExecuteConcurrentlyFunc: func(sshConfig sshutils.SSHConfig, cmd string, hostIPs []string) []sshutils.Result {
			return []sshutils.Result{
				{
					HostIP: "",
					Error:  nil,
					Output: "",
				},
			}
		},
		Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
			return "", nil
		},
	}
	testCases := []testCaseInfo{
		{
			description: "Rotate PG Root Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				timeout: 1000,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mocksshUtils,
			isError:          false,
			ExpectedError:    "No  IPs are found",
			FilterIps:        []IP{},
			ExpectedPatch:    []string{ValidIP, ValidIP1, ValidIP2, ValidIP3},
		},
		{
			description: "Rotate PG Root Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				timeout: 1000,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mocksshUtils,
			isError:          false,
			ExpectedError:    "No  IPs are found",
			FilterIps: []IP{
				{
					IP: ValidIP1,
				},
			},
			ExpectedPatch: []string{ValidIP, ValidIP2, ValidIP3},
		},
		{
			description: "Rotate PG Root Frontend Certs",
			inf:         infra,
			flagsObj: certRotateFlags{
				timeout: 1000,
			},
			currentCertsInfo: mockCertShowCertificates(),
			certToml:         mockCertifiateTemplate(),
			sshutil:          GetMockSSHUtil(&SSHConfig{}, nil, completedMessage, nil, "", nil),
			MockSSHUtil:      mocksshUtils,
			isError:          false,
			ExpectedError:    "No  IPs are found",
			FilterIps: []IP{
				{
					IP: ValidIP1,
				},
				{
					IP: ValidIP3,
				},
			},
			ExpectedPatch: []string{ValidIP, ValidIP2},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.description, func(t *testing.T) {
			c := certRotateFlow{fileUtils: mockFS(),
				sshUtil: testCase.MockSSHUtil,
				writer:  getMockWriterImpl()}
			filteredIps, output := c.patchPGOSRootCAOnFrontend(testCase.inf, testCase.sshutil, testCase.currentCertsInfo, testCase.certToml, testCase.FilterIps)
			fmt.Println(output)
			if testCase.isError {
				assert.Error(t, output, testCase.ExpectedError)
			} else {
				assert.NoError(t, output)
				assert.EqualValues(t, testCase.ExpectedPatch, filteredIps)
			}
		})
	}
}
