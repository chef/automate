package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

var (
	s3ConnectionTitle                  = "S3 connection test"
	s3ConnectionErrorMsg               = "Machine is not able to connect with S3 using the provided access key and secret key"
	s3ConnectionResolutionMsg          = "Provide the correct S3 url or access or secret keys"
	s3ConnectionSuccessMsg             = "Machine is able to connect with S3 using the provided access key and secret key"
	s3BucketAccessTitle                = "S3 bucket access test"
	s3BucketAccessErrorMsg             = "Machine is not able to access the S3 bucket using the provided access key and secret key"
	s3BucketAccessResolutionMsg        = "Provide the necessary access to the S3 bucket"
	s3BucketAccessSuccessMsg           = "Machine is able to access the S3 bucket using the provided access key and secret key"
	successMessageS3Config             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"checks\":[{\"title\":\"S3 connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with S3 using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"S3 bucket access test\",\"passed\":true,\"success_msg\":\"Machine is able to access the S3 bucket using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false}]}}"
	failureMessageS3Config             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"S3 connection test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to connect with S3 using the provided access key and secret key\",\"resolution_msg\":\"Provide the correct S3 url or access or secret keys\",\"skipped\":false},{\"title\":\"S3 bucket access test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to access the S3 bucket using the provided access key and secret key\",\"resolution_msg\":\"Please check if the provided S3 bucket exists or not. If it exists then provide the bucket access to the snapshot user.\",\"skipped\":false}]}}"
	bucketAccessfailureMessageS3Config = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"S3 connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with S3 using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"S3 bucket access test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to access the S3 bucket using the provided access key and secret key\",\"resolution_msg\":\"Provide the necessary access to the S3 bucket\",\"skipped\":false}]}}"
	errorBodyParser                    = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"invalid character '}' looking for beginning of object key string\"}}"
	reqBody                            = `{
		"endpoint": "s3://example-s3.aws.region.com",
		"bucket_name": "backups",
		"base_path": "automate",
		"access_key": "VALID-ACCESS-KEY",
		"secret_key": "SECRET-KEY"
	}`
)

func SetupMockS3ConfigService(mockS3ConnectionModel, mockS3BucketAccessModel models.Checks) s3configservice.IS3Config {
	return &s3configservice.MockS3Config{
		GetS3ConnectionFunc: func(*models.S3ConfigRequest) *models.Checks {
			return &mockS3ConnectionModel
		},
		GetBucketAccessFunc: func(*models.S3ConfigRequest) *models.Checks {
			return &mockS3BucketAccessModel
		},
	}
}

func SetupS3ConfigHandlers(ss s3configservice.IS3Config) *fiber.App {
	log, _ := logger.NewLogger("text", "debug")
	fconf := fiber.Config{
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
	vs.SetupRoutes()
	return vs.App
}
func TestS3Config(t *testing.T) {
	tests := []struct {
		description             string
		expectedCode            int
		expectedBody            string
		body                    string
		mockS3ConnectionModel   models.Checks
		mockS3BucketAccessModel models.Checks
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			mockS3ConnectionModel: models.Checks{
				Title:         s3ConnectionTitle,
				Passed:        true,
				SuccessMsg:    s3ConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockS3BucketAccessModel: models.Checks{
				Title:         s3BucketAccessTitle,
				Passed:        true,
				SuccessMsg:    s3BucketAccessSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			body:         reqBody,
			expectedBody: successMessageS3Config,
		},
		{
			description:  "200:fail both s3connection and s3BucketAccess",
			expectedCode: 200,
			mockS3ConnectionModel: models.Checks{
				Title:         s3ConnectionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      s3ConnectionErrorMsg,
				ResolutionMsg: s3ConnectionResolutionMsg,
			},
			mockS3BucketAccessModel: models.Checks{
				Title:         s3BucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      s3BucketAccessErrorMsg,
				ResolutionMsg: s3BucketAccessResolutionMsg,
			},
			body:         reqBody,
			expectedBody: failureMessageS3Config,
		},
		{
			description:  "200:fail s3BucketAccess",
			expectedCode: 200,
			mockS3ConnectionModel: models.Checks{
				Title:         s3ConnectionTitle,
				Passed:        true,
				SuccessMsg:    s3ConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockS3BucketAccessModel: models.Checks{
				Title:         s3BucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      s3BucketAccessErrorMsg,
				ResolutionMsg: s3BucketAccessResolutionMsg,
			},
			body:         reqBody,
			expectedBody: bucketAccessfailureMessageS3Config,
		},
		{
			description:  "400: body parser error",
			expectedCode: 400,
			mockS3ConnectionModel: models.Checks{
				Title:         s3ConnectionTitle,
				Passed:        true,
				SuccessMsg:    s3ConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockS3BucketAccessModel: models.Checks{
				Title:         s3BucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      s3BucketAccessErrorMsg,
				ResolutionMsg: s3BucketAccessResolutionMsg,
			},
			body: `{
				"endpoint": "s3://example-s3.aws.region.com",
				"bucket_name": "backups",
				"base_path": "automate",
			}`,
			expectedBody: errorBodyParser,
		},
	}
	statusEndpoint := "/api/v1/checks/s3-config"
	for _, test := range tests {

		t.Run(test.description, func(t *testing.T) {
			// Setup the app as it is done in the main function
			app := SetupS3ConfigHandlers(SetupMockS3ConfigService(test.mockS3ConnectionModel, test.mockS3BucketAccessModel))
			req := httptest.NewRequest("POST", statusEndpoint, strings.NewReader(test.body))
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
