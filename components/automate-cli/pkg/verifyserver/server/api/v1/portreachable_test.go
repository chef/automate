package v1_test

import (
	"fmt"
	"io"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/portreachableservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupPortReachableMountHandler(pr portreachableservice.IPortReachableService) (*fiber.App, error) {
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
		AddPortReachableService(pr)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockPortReachableService() portreachableservice.IPortReachableService {
	return &portreachableservice.MockPortReachableService{
		GetPortReachableDetailsFunc: func(reqBody models.PortReachableRequest) models.Checks {
			return models.Checks{
				Title:         constants.PORT_REACHABLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(constants.PORT_REACHABLE_SUCCESS_MSG, reqBody.DestinationNodeServiceProtocol, reqBody.DestinationNodeIp, reqBody.DestinationNodePort),
				ErrorMsg:      "",
				ResolutionMsg: "",
			}
		},
	}
}

func TestPortReachable(t *testing.T) {
	tests := []struct {
		TestName     string
		ExpectedCode int
		ExpectedBody string
		RequestBody  string
	}{
		{
			TestName:     "Valid Body Request",
			ExpectedCode: 200,
			ExpectedBody: `{
				"status": "SUCCESS",
				"result": {
					"title": "Check for reachability of service at destination port",
					"passed": true,
					"success_msg": "The udp service running at 13.37.213.16:1234 is reachable",
					"error_msg": "",
					"resolution_msg": "",
					"skipped":false
				}
			}`,
			RequestBody: `{
				"destination_node_ip": "13.37.213.16",
				"destination_node_port": 1234,
				"destination_node_service_protocol": "udp"
			  }`,
		},
		{
			TestName:     "Invalid Body Request",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid Body Request"
				}
			}`,
			RequestBody: "Invalid Body",
		},
		{
			TestName:     "Not Given all the required Parameters",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "DestinationNodeIp, DestinationNodePort, or DestinationNodeServiceProtocol cannot be empty"
				}
			}`,
			RequestBody: `{
				"destination_node_ip": "",
				"destination_node_port": 1234,
				"destination_node_service_protocol": "udp"
			  }`,
		},
		{
			TestName:     "Giving Invalid Protocol Type",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Please Give Valid Protocol i.e tcp, udp or https"
				}
			}`,
			RequestBody: `{
				"destination_node_ip": "13.37.23.16",
				"destination_node_port": 1234,
				"destination_node_service_protocol": "udp4"
			  }`,
		},
		{
			TestName:     "Giving Invalid Port Number",
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Invalid port number"
				}
			}`,
			RequestBody: `{
				"destination_node_ip": "13.37.23.16",
				"destination_node_port": -1,
				"destination_node_service_protocol": "udp"
			  }`,
		},
	}

	PortReachableEndpoint := constants.PORT_REACHABLE_API_PATH

	app, err := SetupPortReachableMountHandler(SetupMockPortReachableService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", PortReachableEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
