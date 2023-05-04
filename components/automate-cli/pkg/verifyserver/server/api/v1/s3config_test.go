package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockS3ConfigService() s3configservice.S3Config {
	return &s3configservice.MockS3Config{
		GetS3ConnectionFunc: func(models.S3ConfigRequest) models.ServiceCheck {
			return models.ServiceCheck{
				Passed:        true,
				Title:         "S3 connection test",
				SuccessMsg:    "Machine is able to connect with S3 using the provided access key and secret key",
				ErrorMsg:      "",
				ResolutionMsg: "",
			}
		},
	}
}

func SetupS3ConfigHandlers(ss s3configservice.S3Config) *fiber.App {
	log := logger.NewLogger(true)
	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddS3ConfigService(ss)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup()
	return vs.App
}
func TestS3Config(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			// {"status":"SUCCESS","result":{"passed":true,"checks":[{"title":"","passed":"","success_msg":"","error_msg":"","resolution_msg":""},{"title":"","passed":"","success_msg":"","error_msg":"","resolution_msg":""}]}}
			expectedBody: "",
		},
	}
	statusEndpoint := "/api/v1/checks/s3-config"
	// Setup the app as it is done in the main function
	app := SetupDefaultHandlers(SetupMockStatusService())

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			req := httptest.NewRequest("POST", statusEndpoint, nil)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := ioutil.ReadAll(res.Body)
			t.Log(body, "body")
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
