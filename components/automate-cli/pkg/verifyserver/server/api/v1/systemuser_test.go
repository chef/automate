package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemuserservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupSystemUserHandlers(su systemuserservice.SystemUserService) (*fiber.App, error) {
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
		AddSystemUserService(su)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func SetupMockSystemUserService(response models.SystemUserResponse) systemuserservice.SystemUserService {
	return &systemuserservice.MockSystemUserService{
		GetSystemUserServiceDetailsFunc: func() *models.SystemUserResponse {
			return &response
		},
	}
}

func TestSystemUserService(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		responseBody models.SystemUserResponse
		expectedBody string
	}{
		{
			description:  "200:User and Group verified and mapping validated",
			expectedCode: 200,
			responseBody: models.SystemUserResponse{
				Passed: true,
				Checks: []*models.Checks{
					{
						Title:         constants.SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_USER_HAB_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_GROUP_HAB_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE,
						Passed:        true,
						SuccessMsg:    constants.SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedBody: "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"checks\":[{\"title\":\"User creation/validation check\",\"passed\":true,\"success_msg\":\"User is created or found successfully\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"Group creation/validation check\",\"passed\":true,\"success_msg\":\"Group is created or found successfully\",\"error_msg\":\"\",\"resolution_msg\":\"\"},{\"title\":\"User and group mapping successfully\",\"passed\":true,\"success_msg\":\"User and group mapping successful\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}}",
		},
	}

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			systemuserCheckEndpoint := "/api/v1/checks/system-user"
			app, err := SetupSystemUserHandlers(SetupMockSystemUserService(test.responseBody))
			assert.NoError(t, err)
			req := httptest.NewRequest("GET", systemuserCheckEndpoint, nil)
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
