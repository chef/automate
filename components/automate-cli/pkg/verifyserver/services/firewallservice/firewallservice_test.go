package firewallservice_test

import (
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/firewallservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const (
	TIMEOUT                         = 1
	LOCALHOST                       = "localhost"
	VALID_IP                        = "192.165.54.34"
	INVALID_PORT_NUMBER             = "abcd"
	PORT_REACHABLE_SUCCESS_RESPONSE = `{
		"status": "SUCCESS",
		"result": {
			"title": "Check for reachability of service at destination port",
			"passed": true,
			"success_msg": "The tcp service running at ANY_IP:7432 is reachable",
			"error_msg": "",
			"resolution_msg": ""
		}
	}
`

	PORT_REACHABLE_FAILED_RESPONSE = `{
    "status": "SUCCESS",
    "result": {
        "title": "Check for reachability of service at destination port",
        "passed": false,
        "success_msg": "",
        "error_msg": "The tcp service running at ANY_IP:7433 is not reachable",
        "resolution_msg": "Check your firewall settings to provide access to 7433 port at 13.39.148.115"
    }
}
`

	PORT_REACHABLE_ERROR_RESPONSE = `{
	"status": "FAILED",
	"result": null,
	"error": {
		"code": 400,
		"message": "For Some reasons Port-reachable API got failed"
	}
}
`
	PORT_REACHABLE_INVALID_RESPONSE = "HELLO"

	PORT_REACHABLE_UNEXPECTED_RESULT_STRUCT_RESPONSE = `{
		"status": "SUCCESS",
		"result": {
		}
	}
	`
)

func TestNewFirewallService(t *testing.T) {
	testPort := "3073"
	fw := firewallservice.NewFirewallService(logger.NewTestLogger(), time.Duration(TIMEOUT), testPort)
	assert.NotNil(t, fw)
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

func createDummyServer(t *testing.T, SuccessResponse, FailedResponse, ErrorResponse, InvalidResponse, UnexpectedResponse bool, port string) *httptest.Server {
	server := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == constants.PORT_REACHABLE_API_PATH {
			body, err := io.ReadAll(r.Body)
			assert.NoError(t, err)

			var req models.PortReachableRequest
			err = json.Unmarshal(body, &req)
			require.NoError(t, err)
			require.NotZero(t, req)
			require.NotEmpty(t, req)

			w.WriteHeader(http.StatusOK)
			if SuccessResponse {
				w.Write([]byte(PORT_REACHABLE_SUCCESS_RESPONSE))
			} else if FailedResponse {
				w.Write([]byte(PORT_REACHABLE_FAILED_RESPONSE))
			} else if ErrorResponse {
				w.Write([]byte(PORT_REACHABLE_ERROR_RESPONSE))
			} else if InvalidResponse {
				w.Write([]byte(PORT_REACHABLE_INVALID_RESPONSE))
			} else if UnexpectedResponse {
				w.Write([]byte(PORT_REACHABLE_UNEXPECTED_RESULT_STRUCT_RESPONSE))
			}
		}
	}))
	err := startMockServerOnCustomPort(server, port)
	assert.NoError(t, err)
	return server
}
func TestGetFirewallDetails(t *testing.T) {
	PortReachablePassServerPort := "3074"
	PortReachableFailedResponseServerPort := "3075"
	PortReachableErrorResponseServerPort := "3076"
	PortReachableInvalidResponseServerPort := "3077"
	PortReachableUnexpectedResultStructResponseServerPort := "3078"
	VerifyServerNotRunningPort := "3079" // we are using this port when we don't need to start our mock server
	// otherwise for some cases even it will fail in the beginning still it will create mock server for that case also
	httpsTestPort := "3080"

	tests := []struct {
		TestName           string
		ReqBody            models.FirewallRequest
		VerifyServerPort   string
		ExpectedRespBody   models.FirewallResponse
		SuccessResponse    bool
		FailedResponse     bool
		ErrorResponse      bool
		InvalidResponse    bool
		UnexpectedResponse bool
	}{
		{
			TestName: "Destination IP is reachable from source Node for given port",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: PortReachablePassServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        true,
						SuccessMsg:    firewallservice.GetFirewallSuccessMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			SuccessResponse: true,
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because verify server not running",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: VerifyServerNotRunningPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, LOCALHOST),
					},
				},
			},
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because port-reachable API giving Failed Response",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: PortReachableFailedResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, LOCALHOST),
					},
				},
			},
			FailedResponse: true,
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because port-reachable API giving Error Response",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: PortReachableErrorResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, LOCALHOST),
					},
				},
			},
			ErrorResponse: true,
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because port-reachable API giving Invalid Response",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: PortReachableInvalidResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, LOCALHOST),
					},
				},
			},
			InvalidResponse: true,
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because port-reachable API giving Unexpected Result Struct Value Response",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: PortReachableUnexpectedResultStructResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, LOCALHOST),
					},
				},
			},
			UnexpectedResponse: true,
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because Source Node IP is not correct",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               "%%",
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     httpsTestPort,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: VerifyServerNotRunningPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, httpsTestPort, "%%"),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, httpsTestPort, "%%"),
					},
				},
			},
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because Giving Invalid Port Number",
			ReqBody: models.FirewallRequest{
				SourceNodeIP:               LOCALHOST,
				DestinationNodeIP:          VALID_IP,
				DestinationServicePort:     INVALID_PORT_NUMBER,
				DestinationServiceProtocol: constants.HTTPS,
				RootCert:                   "CA_CERT",
			},
			VerifyServerPort: VerifyServerNotRunningPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      firewallservice.GetFirewallErrorMsg(constants.HTTPS, VALID_IP, INVALID_PORT_NUMBER, LOCALHOST),
						ResolutionMsg: firewallservice.GetFirewallResolutionMsg(VALID_IP, INVALID_PORT_NUMBER, LOCALHOST),
					},
				},
			},
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			if e.VerifyServerPort != VerifyServerNotRunningPort {
				server := createDummyServer(t, e.SuccessResponse, e.FailedResponse, e.ErrorResponse, e.InvalidResponse, e.UnexpectedResponse, e.VerifyServerPort)
				defer server.Close()
			}
			fw := firewallservice.NewFirewallService(logger.NewTestLogger(), time.Duration(TIMEOUT), e.VerifyServerPort)
			assert.NotNil(t, fw)
			resp := fw.GetFirewallDetails(e.ReqBody)
			assert.Equal(t, e.ExpectedRespBody, resp)
		})
	}
}
