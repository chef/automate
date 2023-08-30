package gcpcloudstorageservice_test

import (
	"context"
	"errors"
	"testing"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/gcpcloudstorageservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	gcpConnectionTitle                  = "GCP connection test"
	gcpConnectionErrorMsg               = "Machine is not able to connect with GCP using the provided credentials: "
	gcpConnectionResolutionMsg          = "Provide the correct GCP url or access or secret keys"
	gcpConnectionSuccessMsg             = "Machine is able to connect with GCP using the provided credentials"
	gcpBucketAccessTitle                = "GCP bucket access test"
	gcpBucketAccessErrorMsg             = "Machine is not able to access the GCP bucket using the provided access key and secret key"
	gcpBucketAccessResolutionMsg        = "Provide the necessary access to the GCP bucket"
	gcpBucketAccessSuccessMsg           = "Machine is able to access the GCP bucket using the provided access key and secret key"
	successMessagegcpConfig             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":true,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with GCP using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"GCP bucket access test\",\"passed\":true,\"success_msg\":\"Machine is able to access the GCP bucket using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false}]}}"
	failureMessagegcpConfig             = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to connect with GCP using the provided access key and secret key\",\"resolution_msg\":\"Provide the correct GCP url or access or secret keys\",\"skipped\":false},{\"title\":\"GCP bucket access test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to access the GCP bucket using the provided credentials\",\"resolution_msg\":\"Please check if the provided GCP bucket exists or not. If it exists then provide the correct credentials.\",\"skipped\":false}]}}"
	bucketAccessfailureMessagegcpConfig = "{\"status\":\"SUCCESS\",\"result\":{\"passed\":false,\"checks\":[{\"title\":\"GCP connection test\",\"passed\":true,\"success_msg\":\"Machine is able to connect with GCP using the provided access key and secret key\",\"error_msg\":\"\",\"resolution_msg\":\"\",\"skipped\":false},{\"title\":\"GCP bucket access test\",\"passed\":false,\"success_msg\":\"\",\"error_msg\":\"Machine is not able to access the GCP bucket using the provided access key and secret key\",\"resolution_msg\":\"Provide the necessary access to the GCP bucket\",\"skipped\":false}]}}"
	errorBodyParsergcp                  = "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"invalid character '}' looking for beginning of object key string\"}}"
	reqBodygcp                          = `{
		"location": "gcs",
		"endpoint": "endpoint",
		"bucket_name": "a2-backup-restore-test",
		"gcp_service_account": {
			"type": "service_account",
			"project_id": "automate-backup-unit-testing",
			"private_key_id": "oiwehfi8wibkewckjsd8w4r789346t394",
			"private_key": "-----BEGIN PRIVATE KEY-----dflkvdlkfndfkndfk-----END PRIVATE KEY-----\n",
			"client_email": "email@automate-backup-unit-testing.iam.gserviceaccount.com",
			"client_id": "65464654684864353165486",
			"auth_uri": "https://accounts.google.com",
			"token_uri": "https://accounts.google.com",
			"auth_provider_x509_cert_url": "https://accounts.google.com",
			"client_x509_cert_url": "https://accounts.google.com",
			"universe_domain": "https://accounts.google.com"
		}
	}`
)

func TestGetS3Connection(t *testing.T) {
	t.Run("No bucket exists", func(t *testing.T) {
		log, _ := logger.NewLogger("text", "debug")
		cs := gcpcloudstorageservice.NewGCPCloudStorageConfig(log, &gcputils.MockGCPUtils{
			NewSessionWithOptionsFunc: func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
				return &storage.Client{}, nil
			},
			BucketAttributesFunc: func(ctx context.Context, bucket *storage.BucketHandle) error {
				return errors.New("error")
			},
		})
		services := cs.GetGCPConnection(&models.GCPCloudStorageConfigRequest{
			BucketName:               "backups",
			GoogleServiceAccountFile: "dummy_file.json",
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "service_account",
				ProjectID:               "dev",
				PrivateKeyID:            "e123454a6668a89b970f703f",
				PrivateKey:              "-----BEGIN CERTIFICATE-----\nMIIEdTCCA12gAwIBAgIJAKcOSkw0grd/MA0GCSqGSIb3DQEBCwUAMGgxCzAJBgNV\nBAYTAlVTMSUwIwYDVQQKExxTdGFyZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTIw\nMAYDVQQLEylTdGFyZmllbGQgQ2xhc3MgMiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0\neTAeFw0wOTA5MDIwMDAwMDBaFw0zNDA2MjgxNzM5MTZaMIGYMQswCQYDVQQGEwJV\nUzEQMA4GA1UECBMHQXJpem9uYTETMBEGA1UEBxMKU2NvdHRzZGFsZTElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjE7MDkGA1UEAxMyU3RhcmZp\nZWxkIFNlcnZpY2VzIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IC0gRzIwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDVDDrEKvlO4vW+GZdfjohTsR8/\ny8+fIBNtKTrID30892t2OGPZNmCom15cAICyL1l/9of5JUOG52kbUpqQ4XHj2C0N\nTm/2yEnZtvMaVq4rtnQU68/7JuMauh2WLmo7WJSJR1b/JaCTcFOD2oR0FMNnngRo\nOt+OQFodSk7PQ5E751bWAHDLUu57fa4657wx+UX2wmDPE1kCK4DMNEffud6QZW0C\nzyyRpqbn3oUYSXxmTqM6bam17jQuug0DuDPfR+uxa40l2ZvOgdFFRjKWcIfeAg5J\nQ4W2bHO7ZOphQazJ1FTfhy/HIrImzJ9ZVGif/L4qL8RVHHVAYBeFAlU5i38FAgMB\nAAGjgfAwge0wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0O\nBBYEFJxfAN+qAdcwKziIorhtSpzyEZGDMB8GA1UdIwQYMBaAFL9ft9HO3R+G9FtV\nrNzXEMIOqYjnME8GCCsGAQUFBwEBBEMwQTAcBggrBgEFBQcwAYYQaHR0cDovL28u\nc3MyLnVzLzAhBggrBgEFBQcwAoYVaHR0cDovL3guc3MyLnVzL3guY2VyMCYGA1Ud\nHwQfMB0wG6AZoBeGFWh0dHA6Ly9zLnNzMi51cy9yLmNybDARBgNVHSAECjAIMAYG\nBFUdIAAwDQYJKoZIhvcNAQELBQADggEBACMd44pXyn3pF3lM8R5V/cxTbj5HD9/G\nVfKyBDbtgB9TxF00KGu+x1X8Z+rLP3+QsjPNG1gQggL4+C/1E2DUBc7xgQjB3ad1\nl08YuW3e95ORCLp+QCztweq7dp4zBncdDQh/U90bZKuCJ/Fp1U1ervShw3WnWEQt\n8jxwmKy6abaVd38PMV4s/KCHOkdp8Hlf9BRUpJVeEXgSYCfOn8J3/yNTd126/+pZ\n59vPr5KW7ySaNRB6nJHGDn2Z9j8Z3/VyVOEVqQdZe4O/Ui5GjLIAZHYcSNPYeehu\nVsyuLAOQ1xk4meTKCRlb/weWsKh/NEnfVqn3sF/tM+2MR7cwA130A4w=\n-----END CERTIFICATE-----\n\n",
				ClientEmail:             "email@automate-backup-unit-testing.iam.gserviceaccount.com",
				ClientID:                "65464654684864353165486",
				AuthURI:                 "https://accounts.google.com",
				TokenURI:                "https://accounts.google.com",
				AuthProviderX509CertURL: "https://accounts.google.com",
				ClientX509CertURL:       "https://accounts.google.com",
				UniverseDomain:          "https://accounts.google.com",
			},
		})

		assert.Contains(t, services.ErrorMsg, "Cannot find the Bucket in GCP cloud storage")
		assert.Equal(t, services.ResolutionMsg, "Create a bucket in GCP cloud storage")
	})

	t.Run("success", func(t *testing.T) {
		log, _ := logger.NewLogger("text", "debug")
		cs := gcpcloudstorageservice.NewGCPCloudStorageConfig(log, &gcputils.MockGCPUtils{
			NewSessionWithOptionsFunc: func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
				return &storage.Client{}, nil
			},
			BucketAttributesFunc: func(ctx context.Context, bucket *storage.BucketHandle) error {
				return nil
			},
		})
		services := cs.GetGCPConnection(&models.GCPCloudStorageConfigRequest{
			BucketName:               "backups",
			GoogleServiceAccountFile: "dummy_file.json",
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "service_account",
				ProjectID:               "dev",
				PrivateKeyID:            "e123454a6668a89b970f703f",
				PrivateKey:              "-----BEGIN CERTIFICATE-----\nMIIEdTCCA12gAwIBAgIJAKcOSkw0grd/MA0GCSqGSIb3DQEBCwUAMGgxCzAJBgNV\nBAYTAlVTMSUwIwYDVQQKExxTdGFyZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTIw\nMAYDVQQLEylTdGFyZmllbGQgQ2xhc3MgMiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0\neTAeFw0wOTA5MDIwMDAwMDBaFw0zNDA2MjgxNzM5MTZaMIGYMQswCQYDVQQGEwJV\nUzEQMA4GA1UECBMHQXJpem9uYTETMBEGA1UEBxMKU2NvdHRzZGFsZTElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjE7MDkGA1UEAxMyU3RhcmZp\nZWxkIFNlcnZpY2VzIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IC0gRzIwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDVDDrEKvlO4vW+GZdfjohTsR8/\ny8+fIBNtKTrID30892t2OGPZNmCom15cAICyL1l/9of5JUOG52kbUpqQ4XHj2C0N\nTm/2yEnZtvMaVq4rtnQU68/7JuMauh2WLmo7WJSJR1b/JaCTcFOD2oR0FMNnngRo\nOt+OQFodSk7PQ5E751bWAHDLUu57fa4657wx+UX2wmDPE1kCK4DMNEffud6QZW0C\nzyyRpqbn3oUYSXxmTqM6bam17jQuug0DuDPfR+uxa40l2ZvOgdFFRjKWcIfeAg5J\nQ4W2bHO7ZOphQazJ1FTfhy/HIrImzJ9ZVGif/L4qL8RVHHVAYBeFAlU5i38FAgMB\nAAGjgfAwge0wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0O\nBBYEFJxfAN+qAdcwKziIorhtSpzyEZGDMB8GA1UdIwQYMBaAFL9ft9HO3R+G9FtV\nrNzXEMIOqYjnME8GCCsGAQUFBwEBBEMwQTAcBggrBgEFBQcwAYYQaHR0cDovL28u\nc3MyLnVzLzAhBggrBgEFBQcwAoYVaHR0cDovL3guc3MyLnVzL3guY2VyMCYGA1Ud\nHwQfMB0wG6AZoBeGFWh0dHA6Ly9zLnNzMi51cy9yLmNybDARBgNVHSAECjAIMAYG\nBFUdIAAwDQYJKoZIhvcNAQELBQADggEBACMd44pXyn3pF3lM8R5V/cxTbj5HD9/G\nVfKyBDbtgB9TxF00KGu+x1X8Z+rLP3+QsjPNG1gQggL4+C/1E2DUBc7xgQjB3ad1\nl08YuW3e95ORCLp+QCztweq7dp4zBncdDQh/U90bZKuCJ/Fp1U1ervShw3WnWEQt\n8jxwmKy6abaVd38PMV4s/KCHOkdp8Hlf9BRUpJVeEXgSYCfOn8J3/yNTd126/+pZ\n59vPr5KW7ySaNRB6nJHGDn2Z9j8Z3/VyVOEVqQdZe4O/Ui5GjLIAZHYcSNPYeehu\nVsyuLAOQ1xk4meTKCRlb/weWsKh/NEnfVqn3sF/tM+2MR7cwA130A4w=\n-----END CERTIFICATE-----\n\n",
				ClientEmail:             "email@automate-backup-unit-testing.iam.gserviceaccount.com",
				ClientID:                "65464654684864353165486",
				AuthURI:                 "https://accounts.google.com",
				TokenURI:                "https://accounts.google.com",
				AuthProviderX509CertURL: "https://accounts.google.com",
				ClientX509CertURL:       "https://accounts.google.com",
				UniverseDomain:          "https://accounts.google.com",
			},
		})

		assert.Equal(t, gcpConnectionSuccessMsg, services.SuccessMsg)
	})

	t.Run("error", func(t *testing.T) {
		log, _ := logger.NewLogger("text", "debug")
		cs := gcpcloudstorageservice.NewGCPCloudStorageConfig(log, &gcputils.MockGCPUtils{
			NewSessionWithOptionsFunc: func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
				return &storage.Client{}, errors.New("")
			},
			BucketAttributesFunc: func(ctx context.Context, bucket *storage.BucketHandle) error {
				return nil
			},
		})
		services := cs.GetGCPConnection(&models.GCPCloudStorageConfigRequest{
			BucketName:               "backups",
			GoogleServiceAccountFile: "dummy_file.json",
			GcpServiceAccount: &models.GcpServiceAccount{
				Type:                    "service_account",
				ProjectID:               "dev",
				PrivateKeyID:            "e123454a6668a89b970f703f",
				PrivateKey:              "-----BEGIN CERTIFICATE-----\nMIIEdTCCA12gAwIBAgIJAKcOSkw0grd/MA0GCSqGSIb3DQEBCwUAMGgxCzAJBgNV\nBAYTAlVTMSUwIwYDVQQKExxTdGFyZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTIw\nMAYDVQQLEylTdGFyZmllbGQgQ2xhc3MgMiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0\neTAeFw0wOTA5MDIwMDAwMDBaFw0zNDA2MjgxNzM5MTZaMIGYMQswCQYDVQQGEwJV\nUzEQMA4GA1UECBMHQXJpem9uYTETMBEGA1UEBxMKU2NvdHRzZGFsZTElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjE7MDkGA1UEAxMyU3RhcmZp\nZWxkIFNlcnZpY2VzIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IC0gRzIwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDVDDrEKvlO4vW+GZdfjohTsR8/\ny8+fIBNtKTrID30892t2OGPZNmCom15cAICyL1l/9of5JUOG52kbUpqQ4XHj2C0N\nTm/2yEnZtvMaVq4rtnQU68/7JuMauh2WLmo7WJSJR1b/JaCTcFOD2oR0FMNnngRo\nOt+OQFodSk7PQ5E751bWAHDLUu57fa4657wx+UX2wmDPE1kCK4DMNEffud6QZW0C\nzyyRpqbn3oUYSXxmTqM6bam17jQuug0DuDPfR+uxa40l2ZvOgdFFRjKWcIfeAg5J\nQ4W2bHO7ZOphQazJ1FTfhy/HIrImzJ9ZVGif/L4qL8RVHHVAYBeFAlU5i38FAgMB\nAAGjgfAwge0wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0O\nBBYEFJxfAN+qAdcwKziIorhtSpzyEZGDMB8GA1UdIwQYMBaAFL9ft9HO3R+G9FtV\nrNzXEMIOqYjnME8GCCsGAQUFBwEBBEMwQTAcBggrBgEFBQcwAYYQaHR0cDovL28u\nc3MyLnVzLzAhBggrBgEFBQcwAoYVaHR0cDovL3guc3MyLnVzL3guY2VyMCYGA1Ud\nHwQfMB0wG6AZoBeGFWh0dHA6Ly9zLnNzMi51cy9yLmNybDARBgNVHSAECjAIMAYG\nBFUdIAAwDQYJKoZIhvcNAQELBQADggEBACMd44pXyn3pF3lM8R5V/cxTbj5HD9/G\nVfKyBDbtgB9TxF00KGu+x1X8Z+rLP3+QsjPNG1gQggL4+C/1E2DUBc7xgQjB3ad1\nl08YuW3e95ORCLp+QCztweq7dp4zBncdDQh/U90bZKuCJ/Fp1U1ervShw3WnWEQt\n8jxwmKy6abaVd38PMV4s/KCHOkdp8Hlf9BRUpJVeEXgSYCfOn8J3/yNTd126/+pZ\n59vPr5KW7ySaNRB6nJHGDn2Z9j8Z3/VyVOEVqQdZe4O/Ui5GjLIAZHYcSNPYeehu\nVsyuLAOQ1xk4meTKCRlb/weWsKh/NEnfVqn3sF/tM+2MR7cwA130A4w=\n-----END CERTIFICATE-----\n\n",
				ClientEmail:             "email@automate-backup-unit-testing.iam.gserviceaccount.com",
				ClientID:                "65464654684864353165486",
				AuthURI:                 "https://accounts.google.com",
				TokenURI:                "https://accounts.google.com",
				AuthProviderX509CertURL: "https://accounts.google.com",
				ClientX509CertURL:       "https://accounts.google.com",
				UniverseDomain:          "https://accounts.google.com",
			},
		})

		assert.Equal(t, gcpConnectionErrorMsg, services.ErrorMsg)
	})
}

// func TestGetS3Connection(t *testing.T) {
// 	t.Run("No bucket exists", func(t *testing.T) {
// 		log, _ := logger.NewLogger("text", "debug")
// 		cs := gcpcloudstorageservice.NewGCPCloudStorageConfig(log, &gcputils.MockGCPUtils{
// 			NewSessionWithOptionsFunc: func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
// 				return nil, nil
// 			},
// 		})
// 		services := cs.GetGCPConnection(&models.GCPCloudStorageConfigRequest{
// 			BucketName:               "backups",
// 			GoogleServiceAccountFile: "dummy_file.json",
// 			GcpServiceAccount: &models.GcpServiceAccount{
// 				Type:                    "service_account",
// 				ProjectID:               "dev",
// 				PrivateKeyID:            "e123454a6668a89b970f703f",
// 				PrivateKey:              "-----BEGIN CERTIFICATE-----\nMIIEdTCCA12gAwIBAgIJAKcOSkw0grd/MA0GCSqGSIb3DQEBCwUAMGgxCzAJBgNV\nBAYTAlVTMSUwIwYDVQQKExxTdGFyZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTIw\nMAYDVQQLEylTdGFyZmllbGQgQ2xhc3MgMiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0\neTAeFw0wOTA5MDIwMDAwMDBaFw0zNDA2MjgxNzM5MTZaMIGYMQswCQYDVQQGEwJV\nUzEQMA4GA1UECBMHQXJpem9uYTETMBEGA1UEBxMKU2NvdHRzZGFsZTElMCMGA1UE\nChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjE7MDkGA1UEAxMyU3RhcmZp\nZWxkIFNlcnZpY2VzIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IC0gRzIwggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDVDDrEKvlO4vW+GZdfjohTsR8/\ny8+fIBNtKTrID30892t2OGPZNmCom15cAICyL1l/9of5JUOG52kbUpqQ4XHj2C0N\nTm/2yEnZtvMaVq4rtnQU68/7JuMauh2WLmo7WJSJR1b/JaCTcFOD2oR0FMNnngRo\nOt+OQFodSk7PQ5E751bWAHDLUu57fa4657wx+UX2wmDPE1kCK4DMNEffud6QZW0C\nzyyRpqbn3oUYSXxmTqM6bam17jQuug0DuDPfR+uxa40l2ZvOgdFFRjKWcIfeAg5J\nQ4W2bHO7ZOphQazJ1FTfhy/HIrImzJ9ZVGif/L4qL8RVHHVAYBeFAlU5i38FAgMB\nAAGjgfAwge0wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0O\nBBYEFJxfAN+qAdcwKziIorhtSpzyEZGDMB8GA1UdIwQYMBaAFL9ft9HO3R+G9FtV\nrNzXEMIOqYjnME8GCCsGAQUFBwEBBEMwQTAcBggrBgEFBQcwAYYQaHR0cDovL28u\nc3MyLnVzLzAhBggrBgEFBQcwAoYVaHR0cDovL3guc3MyLnVzL3guY2VyMCYGA1Ud\nHwQfMB0wG6AZoBeGFWh0dHA6Ly9zLnNzMi51cy9yLmNybDARBgNVHSAECjAIMAYG\nBFUdIAAwDQYJKoZIhvcNAQELBQADggEBACMd44pXyn3pF3lM8R5V/cxTbj5HD9/G\nVfKyBDbtgB9TxF00KGu+x1X8Z+rLP3+QsjPNG1gQggL4+C/1E2DUBc7xgQjB3ad1\nl08YuW3e95ORCLp+QCztweq7dp4zBncdDQh/U90bZKuCJ/Fp1U1ervShw3WnWEQt\n8jxwmKy6abaVd38PMV4s/KCHOkdp8Hlf9BRUpJVeEXgSYCfOn8J3/yNTd126/+pZ\n59vPr5KW7ySaNRB6nJHGDn2Z9j8Z3/VyVOEVqQdZe4O/Ui5GjLIAZHYcSNPYeehu\nVsyuLAOQ1xk4meTKCRlb/weWsKh/NEnfVqn3sF/tM+2MR7cwA130A4w=\n-----END CERTIFICATE-----\n\n",
// 				ClientEmail:             "email@automate-backup-unit-testing.iam.gserviceaccount.com",
// 				ClientID:                "65464654684864353165486",
// 				AuthURI:                 "https://accounts.google.com",
// 				TokenURI:                "https://accounts.google.com",
// 				AuthProviderX509CertURL: "https://accounts.google.com",
// 				ClientX509CertURL:       "https://accounts.google.com",
// 				UniverseDomain:          "https://accounts.google.com",
// 			},
// 		})

// 		assert.Contains(t, services.ErrorMsg, "Cannot find the Bucket in GCP cloud storage")
// 		assert.Equal(t, services.ResolutionMsg, "Create a bucket in GCP cloud storage")
// 	})
// }
