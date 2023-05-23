package v1_test

import (
	"errors"
	"fmt"
	"io"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemresourceservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockSystemResourceService(responseBody *models.ApiResult, err error) *systemresourceservice.MockSystemResourcesService {
	return &systemresourceservice.MockSystemResourcesService{
		GetSystemResourcesForDeploymentFunc: func(s1, s2 string) (*models.ApiResult, error) {
			return responseBody, err
		},
	}
}

func SetupSystemResourceServiceHandler(srs systemresourceservice.ISystemResourcesService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}

	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).AddSystemResourceService(srs)

	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func TestSystemResourceAPI(t *testing.T) {
	testCases := []struct {
		testCaseDescreption string
		responseBody        *models.ApiResult
		expectedCode        int
		expectedBody        string
		expectedError       error
		nodeType            string
		deploymentState     string
	}{
		{
			testCaseDescreption: "200:Success response no error",
			responseBody: &models.ApiResult{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "CPU count check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("CPU count is >=%d", constants.MIN_CPU_COUNT),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "CPU speed check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("CPU speed should be >=%vGhz", constants.MIN_CPU_SPEED),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Memory size check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Hab free space check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("/hab should have free space >=%vGB", constants.HAB_FREE_DISK_BEFORE_DEP_A2),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Temp free space check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("/tmp should have free space >=%vGB or %v%% of total size of /hab", constants.TMP_FREE_DISK_IN_GB, constants.TMP_FREE_DISK_IN_PER*100),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "/(root volume) free space check",
						Passed:        true,
						SuccessMsg:    fmt.Sprintf("/(root volume) should have free space >=%vGB or %v%% of total size of /hab", constants.ROOT_FREE_DISK_IN_GB, constants.ROOT_FREE_DISK_IN_PER*100),
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedCode:    200,
			expectedBody:    "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"msg\":\"\",\"check\":\"\",\"checks\":[{\"title\":\"CPU count check\",\"passed\":true,\"success_msg\":\"CPU count is \\u003e=4\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"CPU speed check\",\"passed\":true,\"success_msg\":\"CPU speed should be \\u003e=2Ghz\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Memory size check\",\"passed\":true,\"success_msg\":\"Memory should be \\u003e=14GB\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Hab free space check\",\"passed\":true,\"success_msg\":\"/hab should have free space \\u003e=80GB\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Temp free space check\",\"passed\":true,\"success_msg\":\"/tmp should have free space \\u003e=10GB or 5% of total size of /hab\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"/(root volume) free space check\",\"passed\":true,\"success_msg\":\"/(root volume) should have free space \\u003e=20GB or 20% of total size of /hab\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}}",
			nodeType:        constants.AUTOMATE,
			deploymentState: constants.PRE_DEPLOY,
		},
		{
			testCaseDescreption: "400:WrongQuery | deployment_state",
			responseBody:        nil,
			expectedCode:        400,
			expectedBody:        "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"given query deployment_state with value=InvalidDepState is not supported\"}}",
			expectedError:       errors.New("given query deployment_state with value=InvalidDepState is not supported"),
			nodeType:            "automate",
			deploymentState:     "InvalidDepState",
		},
		{
			testCaseDescreption: "400:WrongQuery | nodeType",
			responseBody:        nil,
			expectedCode:        400,
			expectedBody:        "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"given query node_type with value=wrongnode is not supported\"}}",
			expectedError:       errors.New("given query node_type with value=wrongnode is not supported"),
			nodeType:            "wrongnode",
			deploymentState:     "pre-deploy",
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescreption, func(t *testing.T) {
			app, err := SetupSystemResourceServiceHandler(SetupMockSystemResourceService(testCase.responseBody, testCase.expectedError))

			reqUrl := fmt.Sprintf("/api/v1/checks/system-resource?node_type=%s&deployment_state=%s", testCase.nodeType, testCase.deploymentState)
			assert.NoError(t, err)
			req := httptest.NewRequest("GET", reqUrl, nil)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)

			assert.NoError(t, err)
			assert.Equal(t, testCase.expectedBody, string(body))
			assert.Equal(t, res.StatusCode, testCase.expectedCode)
		})
	}
}
