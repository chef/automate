package firewallservice_test

import (
	"fmt"
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

func TestGetFirewallDetails(t *testing.T) {
	PortReachablePassServerPort := "3074"
	PortReachableFailedResponseServerPort := "3075"
	PortReachableErrorResponseServerPort := "3076"
	PortReachableInvalidResponseServerPort := "3077"
	PortReachableUnexpectedResultStructResponseServerPort := "3078"
	VerifyServerNotRunningPort := "3079"
	httpsTestPort := "3080"

	PortReachablePassMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(PORT_REACHABLE_SUCCESS_RESPONSE))
	}))
	err := startMockServerOnCustomPort(PortReachablePassMockServer, PortReachablePassServerPort)
	assert.NoError(t, err)
	defer PortReachablePassMockServer.Close()

	portReachableFailedMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(PORT_REACHABLE_FAILED_RESPONSE))
	}))
	err = startMockServerOnCustomPort(portReachableFailedMockServer, PortReachableFailedResponseServerPort)
	assert.NoError(t, err)
	defer portReachableFailedMockServer.Close()

	portReachableInvalidMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(PORT_REACHABLE_INVALID_RESPONSE))
	}))
	err = startMockServerOnCustomPort(portReachableInvalidMockServer, PortReachableInvalidResponseServerPort)
	assert.NoError(t, err)
	defer portReachableInvalidMockServer.Close()

	portReachableUnexpectedResultStructMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(PORT_REACHABLE_UNEXPECTED_RESULT_STRUCT_RESPONSE))
	}))
	err = startMockServerOnCustomPort(portReachableUnexpectedResultStructMockServer, PortReachableUnexpectedResultStructResponseServerPort)
	assert.NoError(t, err)
	defer portReachableUnexpectedResultStructMockServer.Close()

	portReachableErrorMockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(PORT_REACHABLE_ERROR_RESPONSE))
	}))
	err = startMockServerOnCustomPort(portReachableErrorMockServer, PortReachableErrorResponseServerPort)
	assert.NoError(t, err)
	defer portReachableErrorMockServer.Close()

	tests := []struct {
		TestName         string
		ReqBody          models.FirewallRequest
		VerifyServerPort string
		ExpectedRespBody models.FirewallResponse
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
						SuccessMsg:    fmt.Sprintf(constants.FIREWALL_SUCCESS_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			TestName: "Destination IP is not reachable from source Node for given port because verify server not running there",
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
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, LOCALHOST),
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
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, LOCALHOST),
					},
				},
			},
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
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, LOCALHOST),
					},
				},
			},
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
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, LOCALHOST),
					},
				},
			},
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
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, LOCALHOST),
					},
				},
			},
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
			VerifyServerPort: PortReachableUnexpectedResultStructResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, httpsTestPort, "%%"),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, httpsTestPort, VALID_IP, "%%"),
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
			VerifyServerPort: PortReachableUnexpectedResultStructResponseServerPort,
			ExpectedRespBody: models.FirewallResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      fmt.Sprintf(constants.FIREWALL_ERROR_MESSAGE, constants.HTTPS, VALID_IP, INVALID_PORT_NUMBER, LOCALHOST),
						ResolutionMsg: fmt.Sprintf(constants.FIREWALL_RESOLUTION_MESSAGE, INVALID_PORT_NUMBER, VALID_IP, LOCALHOST),
					},
				},
			},
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			fw := firewallservice.NewFirewallService(logger.NewTestLogger(), time.Duration(TIMEOUT), e.VerifyServerPort)
			assert.NotNil(t, fw)
			resp := fw.GetFirewallDetails(e.ReqBody)
			assert.Equal(t, e.ExpectedRespBody, resp)
		})
	}
}
