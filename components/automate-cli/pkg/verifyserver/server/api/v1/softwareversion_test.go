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

var (
	SuccessExpectedBodyForOpensearch          = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"openssl availability","passed":true,"success_msg":"openssl is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Kernel Version Check","passed":true,"success_msg":"Linux kernel version is 5.10","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":true,"success_msg":"Ubuntu version is 20.04","error_msg":"","resolution_msg":""}]}}`
	SuccessExpectedBodyForPostgres            = `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"stat availability","passed":true,"success_msg":"stat is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Kernel Version Check","passed":true,"success_msg":"Linux kernel version is 5.10","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":true,"success_msg":"Ubuntu version is 20.04","error_msg":"","resolution_msg":""}]}}`
	SuccessExpectedBodyForNonSupportingOS     = `{"status":"SUCCESS","result":{"passed":false,"checks":[{"title":"stat availability","passed":true,"success_msg":"stat is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Kernel Version Check","passed":true,"success_msg":"Linux kernel version is 5.10","error_msg":"","resolution_msg":""},{"title":"Linux Version Check","passed":false,"success_msg":"","error_msg":"Kali Linux version is not supported by automate","resolution_msg":"Ensure Kali Linux correct version is installed on the node"}]}}`
	SuccessExpectedBodyForNonSupportingKernel = `{"status":"SUCCESS","result":{"passed":false,"checks":[{"title":"stat availability","passed":true,"success_msg":"stat is available","error_msg":"","resolution_msg":""},{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""},{"title":"Kernel Version Check","passed":false,"success_msg":"","error_msg":"Linux kernel version is lower than 3.2","resolution_msg":"Use a linux version whose kernel version is greater than 3.2"},{"title":"Linux Version Check","passed":true,"success_msg":"Ubuntu version is 20.04","error_msg":"","resolution_msg":""}]}}`
	FailureResponseForWrongQuery              = `{"status":"FAILED","result":null,"error":{"code":400,"message":"The query wrongquery is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server"}}`
	FailureResponseForEmptyQuery              = `{"status":"FAILED","result":null,"error":{"code":400,"message":"Unsupported query or missing query. Expected value for query 'node_type' are bastion, automate, chef-server, postgres or opensearch."}}`
	LinuxVersionTitle                         = "Linux Version Check"
	KernelVersionTitle                        = "Kernel Version Check"
	MkdirTitle                                = "mkdir availability"
	OpensslTitle                              = "openssl availability"
	StatTitle                                 = "stat availability"
	MkdirIsAvailable                          = "mkdir is available"
	OpensslIsAvailable                        = "openssl is available"
	StatIsAvailable                           = "stat is available"
)

func SetupMockSoftwareVersionService(response models.SoftwareVersionDetails, err error) softwareversionservice.ISoftwareVersionService {
	return &softwareversionservice.MockSoftwareVersionService{
		GetSoftwareDetailsFunc: func(string) (*models.SoftwareVersionDetails, error) {
			return &response, err
		},
	}
}

func SetupCheckSoftwareVersionChecksHandler(sv softwareversionservice.ISoftwareVersionService) *fiber.App {
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
						Title:         OpensslTitle,
						Passed:        true,
						SuccessMsg:    OpensslIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernelVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernel version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody:  SuccessExpectedBodyForOpensearch,
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
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernelVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernel version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody:  SuccessExpectedBodyForPostgres,
			expectedError: nil,
			query:         "postgres",
		},
		{

			description:  "200:success in software version and kernel version but OS is not supported",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernelVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernel version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Kali Linux version is not supported by automate",
						ResolutionMsg: "Ensure Kali Linux correct version is installed on the node",
					},
				},
			},
			expectedBody:  SuccessExpectedBodyForNonSupportingOS,
			expectedError: nil,
			query:         "postgres",
		},
		{
			description:  "200:If query, cammand  and OS is supported but Kernel version is outdated",
			expectedCode: 200,
			responseBody: models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernelVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Linux kernel version is lower than 3.2",
						ResolutionMsg: "Use a linux version whose kernel version is greater than 3.2",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody:  SuccessExpectedBodyForNonSupportingKernel,
			expectedError: nil,
			query:         "postgres",
		},
		{
			description:  `400:If the passed query is empty or spelling of 'node-type' is incorrect`,
			expectedCode: 400,
			responseBody: models.SoftwareVersionDetails{},
			expectedBody: FailureResponseForEmptyQuery,
			expectedError: errors.New(`Unsupported query or missing query. Expected value for query 'node_type' are bastion, automate, chef-server, postgres or opensearch.`),
			query: "",
		},
		{
			description:   "400:If the query entered by the User is not supported",
			expectedCode:  400,
			responseBody:  models.SoftwareVersionDetails{},
			expectedBody:  FailureResponseForWrongQuery,
			expectedError: errors.New("The query wrongquery is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server"),
			query:         "wrongquery",
		},
	}
	for _, test := range tests {

		t.Run(test.description, func(t *testing.T) {
			softwareversioncheckEndpoint := "/api/v1/checks/software-versions/?node_type=" + test.query
			app := SetupCheckSoftwareVersionChecksHandler(SetupMockSoftwareVersionService(test.responseBody, test.expectedError))
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
