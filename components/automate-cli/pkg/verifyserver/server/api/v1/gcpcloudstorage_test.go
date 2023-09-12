package v1_test

import (
	"context"
	"errors"
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
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

	reqBodyGCS = `{
		"location": "gcs",
		"bucket_name": "backup-test",
		"gcp_service_account": {
			"type": "svc_account",
			"project_id": "backup-testing",
			"private_key_id": "lksdhksbdjkcbdsjhcds",
			"private_key": "-----BEGIN PRIVATE KEY----------END PRIVATE KEY-----\n",
			"client_email": "--testing..com",
			"client_id": "dfv",
			"auth_uri": "https://",
			"token_uri": "https://",
			"auth_provider_x509_cert_url": "https://",
			"client_x509_cert_url": "https://",
			"universe_domain": "google"
		}
	}`
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
			body:         reqBodyGCS,
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
			body:         reqBodyGCS,
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
			body:         reqBodyGCS,
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

func TestCredentialsCheck(t *testing.T) {
	t.Run("type not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				ProjectID: "project-id",
			},
		}

		// Test missing type
		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_TYPE_MISSING_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_TYPE_MISSING)
	})

	t.Run("project_id not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type: "svc-scc",
			},
		}

		// Test missing type
		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_PROJECT_ID_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_PROJECT_ID)
	})

	t.Run("private_key_id not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:      "svc-scc",
				ProjectID: "new-id",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_PRIVATE_KEY_ID_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_PRIVATE_KEY_ID)
	})

	t.Run("private_key not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_PRIVATE_KEY_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_PRIVATE_KEY)
	})

	t.Run("client_email not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
				PrivateKey:   "-----BEGIN PRIVATE KEY-----",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_CLIENT_EMAIL_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_CLIENT_EMAIL)
	})

	t.Run("client_id not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
				PrivateKey:   "-----BEGIN PRIVATE KEY-----",
				ClientEmail:  "newclient",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_CLIENT_ID_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_CLIENT_ID)
	})

	t.Run("auth_uri not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
				PrivateKey:   "-----BEGIN PRIVATE KEY-----",
				ClientEmail:  "newclient",
				ClientID:     "new client id",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_AUTH_URI_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_AUTH_URI)
	})

	t.Run("token_uri not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
				PrivateKey:   "-----BEGIN PRIVATE KEY-----",
				ClientEmail:  "newclient",
				ClientID:     "new client id",
				AuthURI:      "auth.auth",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_TOKEN_URI_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_TOKEN_URI)
	})

	t.Run("auth_provider_x509_cert_url not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:         "svc-scc",
				ProjectID:    "new-id",
				PrivateKeyID: "key-id",
				PrivateKey:   "-----BEGIN PRIVATE KEY-----",
				ClientEmail:  "newclient",
				ClientID:     "new client id",
				AuthURI:      "auth.auth",
				TokenURI:     "new token",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_AUTH_PROVIDER_x509_CERT_URL_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_AUTH_PROVIDER_x509_CERT_URL)
	})

	t.Run("client_x509_cert_url not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "svc-scc",
				ProjectID:               "new-id",
				PrivateKeyID:            "key-id",
				PrivateKey:              "-----BEGIN PRIVATE KEY-----",
				ClientEmail:             "newclient",
				ClientID:                "new client id",
				AuthURI:                 "auth.auth",
				TokenURI:                "new token",
				AuthProviderX509CertURL: "https://",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_CLIENT_x509_CERT_URL_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_CLIENT_x509_CERT_URL)
	})

	t.Run("universe_domain not available", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "svc-scc",
				ProjectID:               "new-id",
				PrivateKeyID:            "key-id",
				PrivateKey:              "-----BEGIN PRIVATE KEY-----",
				ClientEmail:             "newclient",
				ClientID:                "new client id",
				AuthURI:                 "auth.auth",
				TokenURI:                "new token",
				AuthProviderX509CertURL: "https://",
				ClientX509CertURL:       "https://",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, constants.GCS_UNIVERSAL_DOMAIN_RESOLUTION)
		assert.Equal(t, err.Error(), constants.GCS_UNIVERSAL_DOMAIN)
	})

	t.Run("no error", func(t *testing.T) {
		cred := &models.GCPCloudStorageConfigRequest{
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "svc-scc",
				ProjectID:               "new-id",
				PrivateKeyID:            "key-id",
				PrivateKey:              "-----BEGIN PRIVATE KEY-----",
				ClientEmail:             "newclient",
				ClientID:                "new client id",
				AuthURI:                 "auth.auth",
				TokenURI:                "new token",
				AuthProviderX509CertURL: "https://",
				ClientX509CertURL:       "https://",
				UniverseDomain:          "google.com",
			},
		}

		result, err := v1.CredentialsCheck(cred)
		assert.Equal(t, result, "")
		assert.NoError(t, err)
	})
}

func TestCredCheckErr(t *testing.T) {
	// Test error case
	err := errors.New("error message")
	result := v1.CredCheckErr("resolution message", err)
	assert.Equal(t, len(result), 1)
	assert.Equal(t, result[0].Title, constants.GCP_CONNECTION_TITLE)
	assert.Equal(t, result[0].Passed, false)
	assert.Equal(t, result[0].ErrorMsg, err.Error())
	assert.Equal(t, result[0].ResolutionMsg, "resolution message")
}
