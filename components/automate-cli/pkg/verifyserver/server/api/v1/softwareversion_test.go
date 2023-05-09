package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockSoftwareVersionService() softwareversionservice.ISoftwareVersionService {
	return &softwareversionservice.MockSoftwareVersionService{
		GetSoftwareServicesFunc: func() (models.SoftwareVersionDetails, error) {
			return models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title: "mkdir availability",
						Passed: true,
						Success_msg: "mkdir is available",
						Error_msg: "",
						Resolution_msg: "",
					},
				},
			},nil
		},
	}
}

func SetupHandlers(sv softwareversionservice.ISoftwareVersionService) (*fiber.App,error) {
	log := logger.NewLogger(true)
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
	vs.Setup()
	return vs.App,nil
}

func TestSoftwareVersionAPI(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
	}{
		{
			description:  "200:success software version check route",
			expectedCode: 200,
			expectedBody: `{"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"mkdir availability","passed":true,"success_msg":"mkdir is available","error_msg":"","resolution_msg":""}]}}`,
					
		},
	}
	sofftwareversioncheckEndpoint := "/api/v1/checks/software-versions"
	app,err := SetupHandlers(SetupMockSoftwareVersionService())
	assert.NoError(t, err)
	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			req := httptest.NewRequest("GET", sofftwareversioncheckEndpoint, nil)
			req.Header.Add("Content-Type" , "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}