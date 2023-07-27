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
	SshUserSuccessMessageWithSudoPassword    = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"SSH user accessible","passed":true,"success_msg":"SSH user is accessible for the node: 1.1.1.1","error_msg":"","resolution_msg":"","skipped":false},{"title":"Sudo password valid","passed":true,"success_msg":"SSH user sudo password is valid for the node: 1.1.1.1","error_msg":"","resolution_msg":"","skipped":false}]}}`
	SshUserSuccessMessageWithoutSudoPassword = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"SSH user accessible","passed":true,"success_msg":"SSH user is accessible for the node: 1.1.1.1","error_msg":"","resolution_msg":"","skipped":false},{"title":"Sudo password valid","passed":true,"success_msg":"SSH user sudo password is valid for the node: 1.1.1.1","error_msg":"","resolution_msg":"","skipped":false}]}}`
	SshUserFailureMessageWithWrongPassword   = `{"status":"SUCCESS","result":{"passed":false,"checks":[{"title":"SSH user accessible","passed":true,"success_msg":"SSH user is accessible for the node: 1.1.1.1","error_msg":"","resolution_msg":"","skipped":false},{"title":"Sudo password invalid","passed":false,"success_msg":"","error_msg":"SSH user sudo password is invalid for the node with IP 1.1.1.1","resolution_msg":"Ensure you have provided the correct sudo password and the user has sudo access on the node: 1.1.1.1","skipped":false}]}}`
	SshUserResponseWithEmptyRequestBody      = `{"status":"FAILED","result":null,"error":{"code":400,"message":"Instance IPs, 'ssh_user', 'ssh_port', and 'ssh_key_file' cannot be empty in the config"}}`
	SshUserResponseErrorBodyParser           = `{"status":"FAILED","result":null,"error":{"code":400,"message":"invalid character '}' looking for beginning of object key string"}}`
	SshUserSuccessTitle                      = `SSH user accessible`
	SshUserSuccessMessage                    = `SSH user is accessible for the node: 1.1.1.1`
	SudoSuccessTitle                         = `Sudo password valid`
	SudoCheckSuccessMessage                  = `SSH user sudo password is valid for the node: 1.1.1.1`
)

func SetupMockSshUserChecksService(responseBody *models.ChecksResponse, err error) sshusercheckservice.SshUserCheckService {
	return &sshusercheckservice.MockSshUserCheckService{
		CheckSshUserDetailsFunc: func(sucr *models.SshUserChecksRequest) (*models.ChecksResponse, error) {
			return responseBody, err
		},
	}
}

func SetUpSshUserCheckHandlers(ssu sshusercheckservice.SshUserCheckService) (*fiber.App, error) {
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
		responseBody  *models.ChecksResponse
		expectedCode  int
		expectedBody  string
		expectedError error
		body          string
	}{
		{
			description: "200:Success status route with sudo password",
			responseBody: &models.ChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         SshUserSuccessTitle,
						Passed:        true,
						SuccessMsg:    SshUserSuccessMessage,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         SudoSuccessTitle,
						Passed:        true,
						SuccessMsg:    SudoCheckSuccessMessage,
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
			description: "200:Success route without sudopassword being set by the user",
			responseBody: &models.ChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         SshUserSuccessTitle,
						Passed:        true,
						SuccessMsg:    SshUserSuccessMessage,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         SudoSuccessTitle,
						Passed:        true,
						SuccessMsg:    SudoCheckSuccessMessage,
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
			description: "200:Success route with sudopassword being set by the user and entered incorrectly",
			responseBody: &models.ChecksResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         SshUserSuccessTitle,
						Passed:        true,
						SuccessMsg:    SshUserSuccessMessage,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Sudo password invalid",
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "SSH user sudo password is invalid for the node with IP 1.1.1.1",
						ResolutionMsg: "Ensure you have provided the correct sudo password and the user has sudo access on the node: 1.1.1.1",
					},
				},
			},
			expectedCode: 200,
			expectedBody: SshUserFailureMessageWithWrongPassword,
			body: `{
				"ip":"1.1.1.1",
				"user_name": "admin",
				"ssh_port": "22",
				"private_key": "----- BEGIN PRIVATE RSA KEY ------",
				"sudo_password":"12345"
			}`,
		},
		{
			description:  "400:One of the mandatory field is empty (ip)",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseWithEmptyRequestBody,
			body: `{
				"ip":"",
				"user_name":"admin",
				"ssh_port": "22",
				"private_key":"----- BEGIN PRIVATE RSA KEY ------"
			}`,
		},
		{
			description:  "400:One of the mandatory field is empty (username)",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseWithEmptyRequestBody,
			body: `{
				"ip":"1.1.1.1",
				"user_name":"",
				"ssh_port": "22",
				"private_key":"----- BEGIN PRIVATE RSA KEY ------"
			}`,
		},
		{
			description:  "400:One of the mandatory field is empty (privateKey)",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseWithEmptyRequestBody,
			body: `{
				"ip":"1.1.1.1",
				"user_name":"",
				"ssh_port": "22",
				"private_key":""
			}`,
		},
		{
			description:  "400:One of the mandatory field is empty (port)",
			responseBody: nil,
			expectedCode: 400,
			expectedBody: SshUserResponseWithEmptyRequestBody,
			body: `{
				"ip":"1.1.1.1",
				"user_name":"",
				"ssh_port": "",
				"private_key":"----- BEGIN PRIVATE RSA KEY ------"
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
