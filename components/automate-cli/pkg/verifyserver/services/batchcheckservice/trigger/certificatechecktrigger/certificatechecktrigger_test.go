package certificatechecktrigger

import (
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

// mockTransport is a mock implementation of the http.RoundTripper interface
type mockTransport struct{}

// RoundTrip returns an error for every request
func (m *mockTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	return nil, fmt.Errorf("mock error")
}

const (
	certificateCheckSuccessResp = `{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Certificates have valid expiry date",
			  "passed": true,
			  "success_msg": "All certificates expiry date is later than 365 days",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificates are of X509 V3 format",
			  "passed": true,
			  "success_msg": "All certificates are of X509 V3 format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Private Keys are of PKCS8 format",
			  "passed": true,
			  "success_msg": "The private keys are of PKCS8 format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
			  "passed": true,
			  "success_msg": "All certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`
	certificateCheckFailureResp = `{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Certificates have valid expiry date",
			  "passed": true,
			  "success_msg": "All certificates expiry date is later than 365 days",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificates are of X509 V3 format",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "The <node/root/admin> certificates are not of X509 V3 format",
			  "resolution_msg": "Generate and provide certificates of X509 v3 format"
			},
			{
			  "title": "Private Keys are of PKCS8 format",
			  "passed": true,
			  "success_msg": "The private keys are of PKCS8 format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
			  "passed": true,
			  "success_msg": "All certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`
)

func GetRequestJsonWithSameFrontEnd() models.Config {
	ipConfig := models.Config{}

	json.Unmarshal([]byte(`{
		"ssh_user": {
			"user_name": "ubuntu",
			"private_key": "test_key",
			"sudo_password": "test@123"
		},
		"arch": "existing_nodes",
		"backup": {
			"file_system": {
				"mount_location": "/mnt/automate_backups"
			}
		},
		"hardware": {
			"automate_node_count": 1,
			"automate_node_ips": [
				"1.2.3.4"
			],
			"chef_infra_server_node_count": 1,
			"chef_infra_server_node_ips": [
				"5.6.7.8"
			],
			"postgresql_node_count": 1,
			"postgresql_node_ips": [
				"9.10.11.12"
			],
			"opensearch_node_count": 1,
			"opensearch_node_ips": [
				"14.15.16.17"
			]
		},
		"certificate": [
			{
				"fqdn": "automate_fqdn",
				"fqdn_root_ca": "---- VALID FQDN ROOT CA ----",
				"node_type": "automate",
				"nodes": [
					{
						"ip": "5.6.7.8",
						"cert": "---- VALID NODE CERT ----",
						"root_cert": "---- VALID ROOT CA ----",
						"key": "---- VALID PRIVATE KEY ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "chef_server_fqdn",
				"fqdn_root_ca": "---- VALID FQDN ROOT CA ----",
				"node_type": "chef-infra-server",
				"nodes": [
					{
						"ip": "5.6.7.8",
						"cert": "---- VALID NODE CERT ----",
						"key": "---- VALID PRIVATE KEY ----",
						"root_cert": "---- VALID ROOT CA ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "",
				"fqdn_root_ca": "",
				"node_type": "postgresql",
				"nodes": [
					{
						"ip": "9.10.11.12",
						"cert": "---- VALID NODE CERT ----",
						"key": "---- VALID PRIVATE KEY ----",
						"root_cert": "---- VALID ROOT CA ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "",
				"fqdn_root_ca": "",
				"node_type": "opensearch",
				"nodes": [
					{
						"ip": "14.15.16.17",
						"cert": "---- VALID NODE CERT ----",
						"root_cert": "---- VALID ROOT CA ----",
						"key": "---- VALID PRIVATE KEY ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			}
		]
	}`), &ipConfig)
	return ipConfig
}

func GetRequestJson() models.Config {
	ipConfig := models.Config{}

	json.Unmarshal([]byte(`{
		"ssh_user": {
			"user_name": "ubuntu",
			"private_key": "test_key",
			"sudo_password": "test@123"
		},
		"arch": "existing_nodes",
		"backup": {
			"file_system": {
				"mount_location": "/mnt/automate_backups"
			}
		},
		"hardware": {
			"automate_node_count": 1,
			"automate_node_ips": [
				"1.2.3.4"
			],
			"chef_infra_server_node_count": 1,
			"chef_infra_server_node_ips": [
				"5.6.7.8"
			],
			"postgresql_node_count": 1,
			"postgresql_node_ips": [
				"9.10.11.12"
			],
			"opensearch_node_count": 1,
			"opensearch_node_ips": [
				"14.15.16.17"
			]
		},
		"certificate": [
			{
				"fqdn": "automate_fqdn",
				"fqdn_root_ca": "---- VALID FQDN ROOT CA ----",
				"node_type": "automate",
				"nodes": [
					{
						"ip": "1.2.3.4",
						"cert": "---- VALID NODE CERT ----",
						"root_cert": "---- VALID ROOT CA ----",
						"key": "---- VALID PRIVATE KEY ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "chef_server_fqdn",
				"fqdn_root_ca": "---- VALID FQDN ROOT CA ----",
				"node_type": "chef-infra-server",
				"nodes": [
					{
						"ip": "5.6.7.8",
						"cert": "---- VALID NODE CERT ----",
						"key": "---- VALID PRIVATE KEY ----",
						"root_cert": "---- VALID ROOT CA ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "",
				"fqdn_root_ca": "",
				"node_type": "postgresql",
				"nodes": [
					{
						"ip": "9.10.11.12",
						"cert": "---- VALID NODE CERT ----",
						"key": "---- VALID PRIVATE KEY ----",
						"root_cert": "---- VALID ROOT CA ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			},
			{
				"fqdn": "",
				"fqdn_root_ca": "",
				"node_type": "opensearch",
				"nodes": [
					{
						"ip": "14.15.16.17",
						"cert": "---- VALID NODE CERT ----",
						"root_cert": "---- VALID ROOT CA ----",
						"key": "---- VALID PRIVATE KEY ----",
						"admin_key": "---- VALID ADMIN PRIVATE KEY ----",
						"admin_cert": "---- VALID ADMIN CERT ----"
					}
				]
			}
		]
	}`), &ipConfig)
	return ipConfig
}

func GetCertificateRequest() models.CertificateCheckRequest {
	req := models.CertificateCheckRequest{
		AdminPrivateKey:  "---- VALID ADMIN PRIVATE KEY ----",
		PrivateKey:       "---- VALID PRIVATE KEY ----",
		NodeCertificate:  "---- VALID NODE CERT ----",
		RootCertificate:  "---- VALID ROOT CA ----",
		AdminCertificate: "---- VALID ADMIN CERT ----",
	}

	return req
}

func TestNewCertificateCheck(t *testing.T) {
	testPort := "1234"
	cc := NewCertificateCheck(logger.NewTestLogger(), testPort)
	assert.NotNil(t, cc)
	assert.NotNil(t, cc.log)
	assert.Equal(t, constants.LOCAL_HOST_URL, cc.host)
	assert.Equal(t, testPort, cc.port)
}

func startMockServerOnCustomPort(mockServer *httptest.Server, port string) error {
	l, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%s", port))
	if err != nil {
		return err
	}
	mockServer.Listener = l
	mockServer.Start()
	return nil
}

func TestCertificateCheck_Run(t *testing.T) {

	t.Run("Returns OK", func(t *testing.T) {
		request := GetRequestJson()
		//starting the mock server on custom port
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			req := r.Body
			//not handling error because of test file
			reader, _ := io.ReadAll(req)
			var actualRequest models.CertificateCheckRequest
			json.Unmarshal([]byte(reader), &actualRequest)

			certificateRequest := GetCertificateRequest()

			assert.NotNil(t, actualRequest)
			assert.Equal(t, actualRequest.NodeCertificate, certificateRequest.NodeCertificate)
			assert.Equal(t, actualRequest.AdminCertificate, certificateRequest.AdminCertificate)
			assert.NotNil(t, actualRequest)
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(certificateCheckSuccessResp))
		}))
		err := startMockServerOnCustomPort(mockServer, "1234")
		assert.NoError(t, err)
		defer mockServer.Close()

		cc := NewCertificateCheck(logger.NewTestLogger(), "1234")
		finalResp := cc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.NotEmpty(t, resp.Result.Checks)
			if resp.Host == "14.15.16.17" {
				triggerResp := resp
				assert.Equal(t, "SUCCESS", triggerResp.Status)
				assert.NotEmpty(t, triggerResp.Result)
				assert.Equal(t, resp.Result.Passed, true)
				assert.Equal(t, 4, len(triggerResp.Result.Checks))
				assert.Equal(t, "Certificates have valid expiry date", triggerResp.Result.Checks[0].Title)
				assert.Equal(t, true, triggerResp.Result.Checks[0].Passed)
				assert.Equal(t, "All certificates expiry date is later than 365 days", triggerResp.Result.Checks[0].SuccessMsg)
			} else {
				assert.Equal(t, resp.Result.Passed, true)
			}
		}
	})
	t.Run("Checking with Same Node for chef server and automate", func(t *testing.T) {
		request := GetRequestJson()
		//starting the mock server on custom port
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			req := r.Body
			//not handling error because of test file
			reader, _ := io.ReadAll(req)
			var actualRequest models.CertificateCheckRequest
			json.Unmarshal([]byte(reader), &actualRequest)

			certificateRequest := GetCertificateRequest()

			assert.NotNil(t, actualRequest)
			assert.Equal(t, actualRequest.NodeCertificate, certificateRequest.NodeCertificate)
			assert.Equal(t, actualRequest.AdminCertificate, certificateRequest.AdminCertificate)
			assert.NotNil(t, actualRequest)
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(certificateCheckSuccessResp))
		}))
		err := startMockServerOnCustomPort(mockServer, "1234")
		assert.NoError(t, err)
		defer mockServer.Close()

		cc := NewCertificateCheck(logger.NewTestLogger(), "1234")
		finalResp := cc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.NotEmpty(t, resp.Result.Checks)
			listForType := []string{constants.AUTOMATE, constants.CHEF_INFRA_SERVER}
			if resp.Host == "5.6.7.8" {
				triggerResp := resp
				assert.Equal(t, "SUCCESS", triggerResp.Status)
				assert.Contains(t, listForType, resp.NodeType)
				assert.NotEmpty(t, triggerResp.Result)
				assert.Equal(t, resp.Result.Passed, true)
				assert.Equal(t, 4, len(triggerResp.Result.Checks))
				assert.Equal(t, "Certificates have valid expiry date", triggerResp.Result.Checks[0].Title)
				assert.Equal(t, true, triggerResp.Result.Checks[0].Passed)
				assert.Equal(t, "All certificates expiry date is later than 365 days", triggerResp.Result.Checks[0].SuccessMsg)
			}

			if resp.Host == "14.15.16.17" {
				triggerResp := resp
				assert.Equal(t, "SUCCESS", triggerResp.Status)
				assert.NotEmpty(t, triggerResp.Result)
				assert.Equal(t, resp.Result.Passed, true)
				assert.Equal(t, 4, len(triggerResp.Result.Checks))
				assert.Equal(t, "Certificates have valid expiry date", triggerResp.Result.Checks[0].Title)
				assert.Equal(t, true, triggerResp.Result.Checks[0].Passed)
				assert.Equal(t, "All certificates expiry date is later than 365 days", triggerResp.Result.Checks[0].SuccessMsg)
			} else {
				assert.Equal(t, resp.Result.Passed, true)
			}
		}
	})

	t.Run("Failure response", func(t *testing.T) {
		//starting the mock server on custom port
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			req := r.Body
			//not handling error because of test file
			reader, _ := io.ReadAll(req)
			var actualRequest models.CertificateCheckRequest
			json.Unmarshal([]byte(reader), &actualRequest)

			certificateRequest := GetCertificateRequest()

			assert.NotNil(t, actualRequest)
			assert.Equal(t, actualRequest.NodeCertificate, certificateRequest.NodeCertificate)
			assert.Equal(t, actualRequest.AdminCertificate, certificateRequest.AdminCertificate)
			assert.NotNil(t, actualRequest)
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(certificateCheckFailureResp))
		}))
		err := startMockServerOnCustomPort(mockServer, "1234")
		assert.NoError(t, err)
		defer mockServer.Close()

		cc := NewCertificateCheck(logger.NewTestLogger(), "1234")
		request := GetRequestJson()
		finalResp := cc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.NotEmpty(t, resp.Result.Checks)
			if resp.Host == "14.15.16.17" {
				triggerResp := resp
				assert.Equal(t, "SUCCESS", triggerResp.Status)
				assert.NotEmpty(t, triggerResp.Result)
				assert.Equal(t, resp.Result.Passed, false)
				assert.Equal(t, 4, len(triggerResp.Result.Checks))
				assert.Equal(t, "Private Keys are of PKCS8 format", triggerResp.Result.Checks[2].Title)
				assert.Equal(t, false, triggerResp.Result.Checks[1].Passed)
				assert.Equal(t, "The <node/root/admin> certificates are not of X509 V3 format", triggerResp.Result.Checks[1].ErrorMsg)
				assert.Equal(t, "Generate and provide certificates of X509 v3 format", triggerResp.Result.Checks[1].ResolutionMsg)
				assert.Empty(t, triggerResp.Result.Checks[1].SuccessMsg)

			} else {
				assert.Equal(t, resp.Result.Passed, false)
			}
		}
	})

	t.Run("Returns error", func(t *testing.T) {
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			req := r.Body
			//not handling error because of test file
			reader, _ := io.ReadAll(req)
			var actualRequest models.CertificateCheckRequest
			json.Unmarshal([]byte(reader), &actualRequest)

			certificateRequest := GetCertificateRequest()

			assert.NotNil(t, actualRequest)
			assert.Equal(t, actualRequest.NodeCertificate, certificateRequest.NodeCertificate)
			assert.Equal(t, actualRequest.AdminCertificate, certificateRequest.AdminCertificate)
			assert.NotNil(t, actualRequest)
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(`invalid JSON`))
		}))
		err := startMockServerOnCustomPort(mockServer, "1234")
		assert.NoError(t, err)
		defer mockServer.Close()

		cc := NewCertificateCheck(logger.NewTestLogger(), "1234")
		request := GetRequestJson()
		finalResp := cc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.NotNil(t, resp.Result.Error)
			assert.Empty(t, resp.Result.Checks)
			assert.Equal(t, resp.Result.Passed, false)
		}
	})
	t.Run("Returns 500 Internal server error", func(t *testing.T) {
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			req := r.Body
			//not handling error because of test file
			reader, _ := io.ReadAll(req)
			var actualRequest models.CertificateCheckRequest
			json.Unmarshal([]byte(reader), &actualRequest)

			certificateRequest := GetCertificateRequest()

			assert.NotNil(t, actualRequest)
			assert.Equal(t, actualRequest.NodeCertificate, certificateRequest.NodeCertificate)
			assert.Equal(t, actualRequest.AdminCertificate, certificateRequest.AdminCertificate)
			assert.NotNil(t, actualRequest)
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte(`invalid JSON`))
		}))
		err := startMockServerOnCustomPort(mockServer, "1234")
		assert.NoError(t, err)
		defer mockServer.Close()

		cc := NewCertificateCheck(logger.NewTestLogger(), "1234")
		request := GetRequestJson()
		finalResp := cc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.NotNil(t, resp.Result.Error)
			assert.Empty(t, resp.Result.Checks)
			assert.Equal(t, resp.Result.Passed, false)
		}
	})

}
