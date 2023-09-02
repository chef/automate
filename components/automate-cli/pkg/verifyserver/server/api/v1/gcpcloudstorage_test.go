package v1_test

import (
	"context"
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/gcpcloudstorageservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

var (
	gcpConnectionTitle                  = "GCP connection test"
	gcpConnectionErrorMsg               = "Machine is not able to connect with GCP using the provided access key and secret key"
	gcpConnectionResolutionMsg          = "Provide the correct GCP url or access or secret keys"
	gcpConnectionSuccessMsg             = "Machine is able to connect with GCP using the provided access key and secret key"
	gcpBucketAccessTitle                = "GCP bucket access test"
	gcpBucketAccessErrorMsg             = "Machine is not able to access the GCP bucket using the provided access key and secret key"
	gcpBucketAccessResolutionMsg        = "Provide the necessary access to the GCP bucket"
	gcpBucketAccessSuccessMsg           = "Machine is able to access the GCP bucket using the provided access key and secret key"
	successMessagegcpConfig             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with GCP using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"GCP bucket access test\",\"passed\":true,\"success_msg\":\"Machine is able to access the GCP bucket using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false}]}}"
	failureMessagegcpConfig             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to connect with GCP using the provided access key and secret key\",\"resolution_msg\":\"Provide the correct GCP url or access or secret keys\",\"skipped\":false}]}}"
	bucketAccessfailureMessagegcpConfig = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with GCP using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"GCP bucket access test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to access the GCP bucket using the provided access key and secret key\",\"resolution_msg\":\"Provide the necessary access to the GCP bucket\",\"skipped\":false}]}}"
)

func SetupMockgcpConfigService(mockGCPConnectionModel, mockGCPBucketAccessModel models.Checks) gcpcloudstorageservice.GCPCloudStorageConfig {
	return &gcpcloudstorageservice.MockGCPCloudStorageConfig{
		GetGCPConnectionFunc: func(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
			return &mockGCPConnectionModel
		},
		GetBucketAccessFunc: func(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
			return &mockGCPBucketAccessModel
		},
	}
}

func SetupgcpConfigHandlers(ss gcpcloudstorageservice.GCPCloudStorageConfig) *fiber.App {
	log, _ := logger.NewLogger("text", "debug")
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddGCSConfigService(ss)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App
}

func TestGetGCPCloudStorageConfig(t *testing.T) {
	tests := []struct {
		description              string
		expectedCode             int
		expectedBody             string
		body                     string
		mockgcpConnectionModel   models.Checks
		mockgcpBucketAccessModel models.Checks
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			mockgcpConnectionModel: models.Checks{
				Title:         gcpConnectionTitle,
				Passed:        true,
				SuccessMsg:    gcpConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockgcpBucketAccessModel: models.Checks{
				Title:         gcpBucketAccessTitle,
				Passed:        true,
				SuccessMsg:    gcpBucketAccessSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			body:         reqBody,
			expectedBody: successMessagegcpConfig,
		},
		{
			description:  "200:fail both gcpconnection and gcpBucketAccess",
			expectedCode: 200,
			mockgcpConnectionModel: models.Checks{
				Title:         gcpConnectionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      gcpConnectionErrorMsg,
				ResolutionMsg: gcpConnectionResolutionMsg,
			},
			mockgcpBucketAccessModel: models.Checks{
				Title:         gcpBucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      gcpBucketAccessErrorMsg,
				ResolutionMsg: gcpBucketAccessResolutionMsg,
			},
			body:         reqBody,
			expectedBody: failureMessagegcpConfig,
		},
		{
			description:  "200:fail gcpBucketAccess",
			expectedCode: 200,
			mockgcpConnectionModel: models.Checks{
				Title:         gcpConnectionTitle,
				Passed:        true,
				SuccessMsg:    gcpConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockgcpBucketAccessModel: models.Checks{
				Title:         gcpBucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      gcpBucketAccessErrorMsg,
				ResolutionMsg: gcpBucketAccessResolutionMsg,
			},
			body:         reqBody,
			expectedBody: bucketAccessfailureMessagegcpConfig,
		},
		{
			description:  "400: body parser error",
			expectedCode: 400,
			mockgcpConnectionModel: models.Checks{
				Title:         gcpConnectionTitle,
				Passed:        true,
				SuccessMsg:    gcpConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockgcpBucketAccessModel: models.Checks{
				Title:         gcpBucketAccessTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      gcpBucketAccessErrorMsg,
				ResolutionMsg: gcpBucketAccessResolutionMsg,
			},
			body: `{
				"endpoint": "gcp://example-gcp.region.com",
				"bucket_name": "backups",
				"base_path": "automate",
			}`,
			expectedBody: errorBodyParser,
		},
	}
	statusEndpoint := "/api/v1/checks/gcp-cloud-storage-config"
	for _, test := range tests {

		t.Run(test.description, func(t *testing.T) {
			// Setup the app as it is done in the main function
			app := SetupgcpConfigHandlers(SetupMockgcpConfigService(test.mockgcpConnectionModel, test.mockgcpBucketAccessModel))
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
