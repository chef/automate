package v1_test

import (
	"io"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/firewallservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupFirewallHandler(fw firewallservice.IFirewallService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddFirewallService(fw)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockFirewallService() firewallservice.IFirewallService {
	return &firewallservice.MockFirewallService{
		GetFirewallDetailsFunc: func(reqBody models.FirewallRequest) models.FirewallResponse {
			return models.FirewallResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.FIREWALL_TITLE,
						Passed:        true,
						SuccessMsg:    firewallservice.GetFirewallSuccessMsg(constants.TCP, "13.39.148.115", "7432", "15.237.128.20"),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			}
		},
	}
}

func TestFirewallCheck(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName: "Valid Body Request",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "7432",
				"destination_service_protocol": "tcp"
			  }`,
			ExpectedCode: 200,
			ExpectedBody: `{
				"status": "SUCCESS",
				"result": {
					"passed": true,
					"checks": [
						{
							"title": "check for reachability of service at destination port from the source node",
							"passed": true,
							"success_msg": "The tcp service running on 13.39.148.115:7432 is reachable from 15.237.128.20",
							"error_msg": "",
							"resolution_msg": "",
							"skipped":false
						}
					]
				}
			}`,
		},
		{
			TestName:     "Invalid Body Request",
			RequestBody:  "Invalid Body",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid Body Request"
				}
			}`,
		},
		{
			TestName: "Not Given all the required Fields",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "7432"
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "source_node_ip, destination_node_ip, destination_service_port or destination_service_protocol cannot be empty"
				}
			}`,
		},
		{
			TestName: "Giving Space instead of actual Values",
			RequestBody: `{
				"source_node_ip": "",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "7432",
				"destination_service_protocol": "   "
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "source_node_ip, destination_node_ip, destination_service_port or destination_service_protocol cannot be empty"
				}
			}`,
		},
		{
			TestName: "Port Number is not a integer",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "abcd",
				"destination_service_protocol": "tcp"
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid destination_service_port number"
				}
			}`,
		},
		{
			TestName: "Port Number is not a valid number",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "-1234",
				"destination_service_protocol": "tcp"
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid destination_service_port range"
				}
			}`,
		},
		{
			TestName: "Source IP is same as Destination IP",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "15.237.128.20",
				"destination_service_port": "7432",
				"destination_service_protocol": "tcp"
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "source_node_ip and destination_node_ip cannot be same"
				}
			}`,
		},
		{
			TestName: "Giving Invalid Protocol",
			RequestBody: `{
				"source_node_ip": "15.237.128.20",
				"destination_node_ip": "13.39.148.115",
				"destination_service_port": "7432",
				"destination_service_protocol": "udp4"
			  }`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Please Give Valid Protocol i.e tcp, udp or https"
				}
			}`,
		},
	}

	FirewallEndpoint := constants.FIREWALL_API_PATH

	app, err := SetupFirewallHandler(SetupMockFirewallService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", FirewallEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, test.ExpectedBody, string(body))
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
