package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"

	"github.com/stretchr/testify/assert"
)

func SetupMockBatchCheckService() batchcheckservice.IBatchCheckService {
	return &batchcheckservice.MockBatchCheckService{
		BatchCheckFunc: func(checks []string, config models.Config) (models.BatchCheckResponse, error) {
			return models.BatchCheckResponse{
				Status: "SUCCESS",
				Result: []models.BatchCheckResult{
					{
						NodeType: "automate",
						Ip:       "1.2.3.4",
						Tests: []models.ApiResult{
							{
								Passed:  true,
								Message: "success",
								Check:   "hardware-resource-count",
								Checks: []models.Checks{
									{
										Title:      "hardware-resource-count-1",
										Passed:     true,
										SuccessMsg: "success",
									},
									{
										Title:      "hardware-resource-count-2",
										Passed:     true,
										SuccessMsg: "success",
									},
								},
							},
							{
								Passed:  true,
								Message: "success",
								Check:   "ssh-user",
								Checks: []models.Checks{
									{
										Title:      "ssh-user-1",
										Passed:     true,
										SuccessMsg: "success",
									},
									{
										Title:      "ssh-user-2",
										Passed:     true,
										SuccessMsg: "success",
									},
								},
							},
						},
					},
				},
			}, nil
		},
	}
}

func SetupHandlers(ss batchcheckservice.IBatchCheckService) (*fiber.App, error) {
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
		AddBatchCheckService(ss)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func TestBatchCheckAPI(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
		requestBody  string
	}{
		{
			description: "200:success batch check route",
			requestBody: `
			{
			"checks": ["hardware-resource-count", "ssh-user"],
			"config":{
				"ssh_user":{
					"user_name": "ubuntu",
					"private_key": "",
					"sudo_password": ""
					}
				}
			}`,
			expectedCode: 200,
			expectedBody: `{"status":"SUCCESS","result":[{"node_type":"automate","ip":"1.2.3.4","tests":[{"passed":true,"msg":"success","check":"hardware-resource-count","checks":[{"title":"hardware-resource-count-1","passed":true,"success_msg":"success","error_msg":"","resolution_msg":""},{"title":"hardware-resource-count-2","passed":true,"success_msg":"success","error_msg":"","resolution_msg":""}]},{"passed":true,"msg":"success","check":"ssh-user","checks":[{"title":"ssh-user-1","passed":true,"success_msg":"success","error_msg":"","resolution_msg":""},{"title":"ssh-user-2","passed":true,"success_msg":"success","error_msg":"","resolution_msg":""}]}]}]}`,
		},
		{
			description: "400:failure batch check route",
			requestBody: `
			{
			"checks": ["abc", "pqr"],
			"config":{
				"ssh_user":{
					"user_name": "ubuntu",
					"private_key": "",
					"sudo_password": ""
					}
				}
			}`,
			expectedCode: 400,
			expectedBody: "\"following checks are not supported [abc pqr]\"",
		},
		{
			description: "400:failure empty checks",
			requestBody: `
			{
			"checks": [],
			"config":{
				"ssh_user":{
					"user_name": "ubuntu",
					"private_key": "",
					"sudo_password": ""
					}
				}
			}`,
			expectedCode: 400,
			expectedBody: "\"check cannot be empty\"",
		},
		{
			description: "400:failure invalid request format",
			requestBody: `
			{
			"field1": [],
			"field2":`,
			expectedCode: 400,
			expectedBody: "\"batch check request body parsing failed: unexpected end of JSON input\"",
		},
	}
	batchCheckEndpoint := "/api/v1/checks/batch-checks"
	// Setup the app as it is done in the main function
	app, err := SetupHandlers(SetupMockBatchCheckService())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			bodyReader := strings.NewReader(test.requestBody)
			req := httptest.NewRequest("POST", batchCheckEndpoint, bodyReader)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, test.expectedCode, res.StatusCode)
		})
	}
}
