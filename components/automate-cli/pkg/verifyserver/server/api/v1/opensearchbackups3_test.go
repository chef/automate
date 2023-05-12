package v1_test

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/opensearchbackupservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/gofiber/fiber"

	"github.com/stretchr/testify/assert"
)

func SetupMockOpensearchBackupS3Service() opensearchbackupservice.IOSS3BackupService {

	return &opensearchbackupservice.MockOSS3BackupService{
		OSS3BackupVerifyFunc: func(request models.S3BackupDetails, ctx *fiber.Ctx) (models.S3BackupManagedResponse, error) {
			return models.S3BackupManagedResponse{
				Passed: true,
				Checks: []models.S3BackupChecks{
					{
						Title:         "Create test backup",
						Passed:        true,
						SuccessMsg:    "OpenSearch is able to create backup to provided S3",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			}, nil
		},
	}
}

func SetupOSBackupHandler(ss opensearchbackupservice.IOSS3BackupService) (*fiber.App, error) {
	log := logger.NewLogger(true)
	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddOSS3BackupService(ss)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup()
	return vs.App, nil
}

func TestOpensearchS3BackupAPI(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		requestBody  string
		expectedBody string
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			requestBody: `
			{
				"endpoint": "https://maangedos.es.amazonaws.com",
				"username": "admin",
				"password": "admin",
				"s3_bucket": "backup-test-bucket",
				"s3_basepath": "path/to/backup",
				"aws_access_key": "accesskey",
				"aws_secret_key": "secretkey",
				"aws_region": "eu-north-1",
				"aws_role_arn": "arn:to:the:backend:role"
			  }`,
			expectedBody: "{\"passed\":true,\"checks\":[{\"title\":\"Create test backup\",\"passed\":true,\"success_msg\":\"OpenSearch is able to create backup to provided S3\",\"error_msg\":\"\",\"resolution_msg\":\"\"}]}",
		},
	}
	osS3BackupEndpoint := "/api/v1/checks/aws-opensearch-s3-bucket-access"

	app, err := SetupOSBackupHandler(SetupMockOpensearchBackupS3Service())
	assert.NoError(t, err)

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			bodyReader := strings.NewReader(test.requestBody)
			req := httptest.NewRequest("POST", osS3BackupEndpoint, bodyReader)
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
