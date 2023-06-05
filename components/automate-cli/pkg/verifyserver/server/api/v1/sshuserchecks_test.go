package v1_test

import (
	"io"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/sshusercheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

const (
	SshUserSuccessMessageWithSudoPassword    = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"SSH user accessible","passed":true,"success_msg":"SSH user is accessible for the node: 1.1.1.1","error_msg":"","resolution_msg":""},{"title":"Sudo password valid","passed":true,"success_msg":"SSH user sudo password is valid for the node: 1.1.1.1","error_msg":"","resolution_msg":""}]}}`
	SshUserSuccessMessageWithoutSudoPassword = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"SSH user accessible","passed":true,"success_msg":"SSH user is accessible for the node: 1.1.1.1","error_msg":"","resolution_msg":""}]}}`
	SshUserResponseWithEmptyRequestBody      = `{"status":"FAILED","result":null,"error":{"code":400,"message":"Request Parameters that is 'ip', 'user_name', 'ssh_port' and 'private_key' cannot be empty"}}`
	SshUserResponseErrorBodyParser           = `{"status":"FAILED","result":null,"error":{"code":400,"message":"invalid character '}' looking for beginning of object key string"}}`
)

func SetupMockSshUserChecksService(responseBody *models.SshUserChecksResponse, err error) sshusercheckservice.SshUsercheckService {
	return &sshusercheckservice.MockSshUserCheckService{
		CheckSshUserDetailsFunc: func(*models.SshUserChecksRequest) (*models.SshUserChecksResponse, error) {
			return responseBody, err
		},
	}
}

func SetUpSshUserCheckHandlers(ssu sshusercheckservice.SshUsercheckService) (*fiber.App, error) {
	log, _ := logger.NewLogger("text", "debug")
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddSshUserCheckService(ssu)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func TestCheckSshUser(t *testing.T) {
	tests := []struct {
		description   string
		responseBody  *models.SshUserChecksResponse
		expectedCode  int
		expectedBody  string
		expectedError error
		body          string
	}{
		{
			description: "200:success status route with sudo password",
			responseBody: &models.SshUserChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "SSH user accessible",
						Passed:        true,
						SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Sudo password valid",
						Passed:        true,
						SuccessMsg:    "SSH user sudo password is valid for the node: 1.1.1.1",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedCode: 200,
			expectedBody: SshUserSuccessMessageWithSudoPassword,
			body: `{
				"ip":"1.1.1.1",
				"user_name": "admin",
				"ssh_port": "22",
				"private_key": "----- BEGIN PRIVATE RSA KEY ------",
				"sudo_password": "adminpass"
			}`,
		},
		{
			description: "200: success route without sudopassword",
			responseBody: &models.SshUserChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "SSH user accessible",
						Passed:        true,
						SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedCode: 200,
			expectedBody: SshUserSuccessMessageWithoutSudoPassword,
			body: `{
				"ip":"1.1.1.1",
				"user_name": "admin",
				"ssh_port": "22",
				"private_key": "----- BEGIN PRIVATE RSA KEY ------"
			}`,
		},
		{
			description:  "400: If one of the mandatory field is empty",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseWithEmptyRequestBody,
			body: `{
				"ip":"1.1.1.1",
				"user_name":"admin",
				"ssh_port": "22",
				"private_key":""
			}`,
		},
		{
			description:  "400: Body Parser Error",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseErrorBodyParser,
			body: `{
				"ip":"1.1.1.1",
				"user_name": "admin",
			}`,
		},
	}

	sshUserCheckEndpoint := "/api/v1/checks/ssh-users"

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			app, _ := SetUpSshUserCheckHandlers(SetupMockSshUserChecksService(test.responseBody, test.expectedError))
			req := httptest.NewRequest("POST", sshUserCheckEndpoint, strings.NewReader(test.body))
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			t.Log(body, "body")
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
