package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalopensearchservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupExternalOpensearchMountHandler(eos externalopensearchservice.IExternalOpensearchService) (*fiber.App, error) {
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
		AddExternalOpensearchService(eos)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockExternalOpensearchService() externalopensearchservice.IExternalOpensearchService {
	return &externalopensearchservice.MockExternalOpensearchService{
		GetExternalOpensearchDetailsFunc: func(reqBody models.ExternalOS, port int) models.ExternalOpensearchResponse {
			return models.ExternalOpensearchResponse{
				Passed: true,
				Checks: []models.ExternalOpensearchCheck{
					{
						Title:         "Connection successfully tested",
						Passed:        true,
						Status:        constants.STATUS_PASS,
						SuccessMsg:    constants.EXTERNAL_OPENSEARCH_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
						DebugMsg:      "",
					},
				},
			}
		},
	}
}

func TestExternalOpensearch(t *testing.T) {
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
					"passed": true,
					"checks": [
						{
							"title": "Connection successfully tested",
							"passed": true,
							"status": "PASS",
							"success_msg": "Machine is able to connect with External Managed OpenSeach",
							"error_msg": "",
							"resolution_msg": "",
							"debug_msg": ""
						}
					]
				}
			}`,
			RequestBody: `{
				"opensearch_domain_name": "managed-services-os",
				"opensearch_domain_url": "search-managed-os-7drxn7jm4edbwzqaga.eu-west-3.es.amazonaws.com",
				"opensearch_username": "admin",
				"opensearch_user_password": "admin",
				"opensearch_root_cert": "-----BEGIN CERTIFICATE-----\nVALID_ROOT_CA-----END CERTIFICATE-----\n"
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
					"message": "OSDomainName, OSDomainURL, OSUsername, OSUserPassword or OSCert cannot be empty"
				}
			}`,
			RequestBody: `{
				"opensearch_domain_name": "managed-services-os",
				"opensearch_domain_url": "search-managed-os-7drxn7jm4edbwz3dukpshdqaga.eu-west-3.es.amazonaws.com",
				"opensearch_username": "admin",
				"opensearch_user_password": "",
				"opensearch_root_cert": "-----BEGIN CERTIFICATE-----VALID_ROOT_CA-----END CERTIFICATE-----\n"
			}`,
		},
	}

	ExternalOpensearchEndpoint := constants.EXTERNAL_OPENSEARCH_API_PATH

	app, err := SetupExternalOpensearchMountHandler(SetupMockExternalOpensearchService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.TestName, func(t *testing.T) {
			bodyReader := strings.NewReader(test.RequestBody)
			req := httptest.NewRequest("POST", ExternalOpensearchEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body) //nosemgrep
			assert.NoError(t, err, test.TestName)
			assert.JSONEq(t, string(body), test.ExpectedBody)
			assert.Equal(t, test.ExpectedCode, res.StatusCode)
		})
	}
}
