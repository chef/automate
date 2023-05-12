package v1_test

import (
	"errors"
	"io/ioutil"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockSoftwareVersionService(response models.SoftwareVersionDetails, err error) softwareversionservice.ISoftwareVersionService {
	return &softwareversionservice.MockSoftwareVersionService{
		GetSoftwareDetailsFunc: func(string) (*models.SoftwareVersionDetails, error) {
			return &response, err
		},
	}
}

func SetupHandler(sv softwareversionservice.ISoftwareVersionService) *fiber.App {
	log, _ := logger.NewLogger("text", "debug")
	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).AddSoftwareVersionService(sv)

	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App
}

func TestSoftwareVersionAPI(t *testing.T) {
	tests := []struct {
		description   string
		expectedCode  int
		responseBody  models.SoftwareVersionDetails
		expectedBody  string
		expectedError error
		query         string
	}{
		{
			description:  "200:success software version check route when query is opensearch",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "openssl availability",
						Passed:        true,
						SuccessMsg:    "openssl is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Linux Version Check",
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody:  `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"openssl availability","passed":true,"success_msg":"openssl is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":true,"success_msg":"Ubuntu version is 20.04","error_msg":"","resolution_msg":""}]}}`,
			expectedError: nil,
			query:         "opensearch",
		},
		{

			description:  "200:success software version check route when query is postgres",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "stat availability",
						Passed:        true,
						SuccessMsg:    "stat is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Linux Version Check",
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody:  `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"stat availability","passed":true,"success_msg":"stat is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":true,"success_msg":"Ubuntu version is 20.04","error_msg":"","resolution_msg":""}]}}`,
			expectedError: nil,
			query:         "postgres",
		},
		{

			description:  "200:success software version but OS is not supported",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "stat availability",
						Passed:        true,
						SuccessMsg:    "stat is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Linux Version Check",
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Kali Linux version is not supported by automate",
						ResolutionMsg: "Ensure Kali Linux correct version is installed on the node",
					},
				},
			},
			expectedBody:  `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"stat availability","passed":true,"success_msg":"stat is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":false,"success_msg":"","error_msg":"Kali Linux version is not supported by automate","resolution_msg":"Ensure Kali Linux correct version is installed on the node"}]}}`,
			expectedError: nil,
			query:         "postgres",
		},
		{
			description:   "400:If the query entered by the User is not supported",
			expectedCode:  400,
			responseBody:  models.SoftwareVersionDetails{},
			expectedBody:  `{"status":"FAILED","result":null,"error":{"code":400,"message":"The query wrongquery is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server"}}`,
			expectedError: errors.New("The query wrongquery is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server"),
			query:         "wrongquery",
		},
		{
			description:  "200:If the OS Version is wrong",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{},
		},
	}
	for _, test := range tests {

		t.Run(test.description, func(t *testing.T) {
			softwareversioncheckEndpoint := "/api/v1/checks/software-versions/?node_type=" + test.query
			app := SetupHandler(SetupMockSoftwareVersionService(test.responseBody, test.expectedError))
			req := httptest.NewRequest("GET", softwareversioncheckEndpoint, nil)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
