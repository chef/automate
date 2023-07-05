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
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/fqdnservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupMockFqdnService() fqdnservice.IFqdnService {
	return &fqdnservice.MockFqdnService{
		CheckFqdnReachabilityFunc: func(req models.FqdnRequest, port string) models.FqdnResponse {
			return models.FqdnResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         constants.FQDN_TITLE,
						Passed:        true,
						SuccessMsg:    constants.FQDN_TITLE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.NODE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.NODE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.CERTIFICATE_TITLE,
						Passed:        true,
						SuccessMsg:    constants.CERTIFICATE_SUCCESS_MESSAGE,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			}
		},
	}
}

func SetupFqdnHandlers(fq fqdnservice.IFqdnService) (*fiber.App, error) {
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
		AddFqdnService(fq)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup(false)
	return vs.App, nil
}

func TestCheckFqdn(t *testing.T) {
	tests := []struct {
		TestName     string
		RequestBody  string
		ExpectedCode int
		ExpectedBody string
	}{
		{
			TestName: "200:success",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "chef-infra-server",
				"root_cert": "-----BEGIN CERTIFICATE-----\nVALID_CERTIFICATE\n-----END CERTIFICATE-----\n",
				"is_after_deployment": false,
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 200,
			ExpectedBody: `{
				"status": "SUCCESS",
				"result": {
					"passed": true,
					"checks": [
						{
							"title": "FQDN is reachable",
							"passed": true,
							"success_msg": "FQDN is reachable",
							"error_msg": "",
							"resolution_msg": "",
							"skipped":false
						},
						{
							"title": "Nodes are reachable",
							"passed": true,
							"success_msg": "All nodes are reachable",
							"error_msg": "",
							"resolution_msg": "",
							"skipped":false
						},
						{
							"title": "Certificate validity for FQDN",
							"passed": true,
							"success_msg": "FQDN has with valid certificates",
							"error_msg": "",
							"resolution_msg": "",
							"skipped":false
						}
					]
				}
			}`,
		},
		{
			TestName: "400: Failure - FQDN is empty",
			RequestBody: `{
				"fqdn": "",
				"node_type": "chef-infra-server",
				"root_cert": "-----BEGIN CERTIFICATE-----\nVALID_CERTIFICATE\n-----END CERTIFICATE-----\n",
				"is_after_deployment": false,
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "fqdn, nodes can't be empty, Please provide all the required fields."
				}
			}`,
		},
		{
			TestName: "400: Failure - Node Type is empty",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "",
				"root_cert": "-----BEGIN CERTIFICATE-----\nVALID_CERTIFICATE\n-----END CERTIFICATE-----\n",
				"is_after_deployment": true,
				"nodes": [
					"172.154.0.2"
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "node_type should be automate or chef-infra-server, Please provide node_type."
				}
			}`,
		},
		{
			TestName: "400: Failure - Node IP is not in valid format",
			RequestBody: `{
				"fqdn": "localhost",
				"node_type": "automate",
				"root_cert": "-----BEGIN CERTIFICATE-----\nVALID_CERTIFICATE\n-----END CERTIFICATE-----\n",
				"is_after_deployment": true,
				"nodes": [
					" "
				]
			}`,
			ExpectedCode: 400,
			ExpectedBody: `{
				"status": "FAILED",
				"result": null,
				"error": {
					"code": 400,
					"message": "Node IP is not valid, Please provide the valid format IP."
				}
			}`,
		},
		{
			TestName:     "400: Failure - Invalid Body",
			RequestBody:  "Wrong body",
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
	}

	fqdnCheckEndpoint := constants.FQDN_LOAD_BALANCER_CHECK
	// Setup the app as it is done in the main function
	app, err := SetupFqdnHandlers(SetupMockFqdnService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", fqdnCheckEndpoint, bodyReader)
			req.Header.Add(constants.CONTENT_TYPE, constants.TYPE_JSON)
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, test.ExpectedBody, string(body))
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
