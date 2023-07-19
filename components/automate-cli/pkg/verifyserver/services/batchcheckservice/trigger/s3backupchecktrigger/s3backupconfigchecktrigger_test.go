package s3backupchecktrigger

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

const (
	s3BackupConfigCheckResponseFromOneNodeSuccess = `{
		"status": "SUCCESS",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with S3 using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "S3 bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the S3 bucket using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`

	s3BackupCheckResponseExpectedSuccess = `[
	{
		"status": "SUCCESS",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with S3 using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "S3 bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the S3 bucket using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  },{
		"status": "SUCCESS",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with S3 using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "S3 bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the S3 bucket using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }
	]
	`
	s3BackupCheckResponseFailure = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with S3 using the provided access key and secret key",
			  "resolution_msg": "Provide the correct S3 url or access or secret keys"
			},
			{
			  "title": "S3 bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the S3 bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the S3 bucket"
			}
		  ]
		}
	  }
	  `

	s3BackupCheckResponseFailureExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"automate",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with S3 using the provided access key and secret key",
			  "resolution_msg": "Provide the correct S3 url or access or secret keys"
			},
			{
			  "title": "S3 bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the S3 bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the S3 bucket"
			}
		  ]
		}
	  },
	  {
		"status": "SUCCESS",
		"node_type" :"automate",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "S3 connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with S3 using the provided access key and secret key",
			  "resolution_msg": "Provide the correct S3 url or access or secret keys"
			},
			{
			  "title": "S3 bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the S3 bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the S3 bucket"
			}
		  ]
		}
	  }
	  ]
	  `
	endPoint   = "//s3-url-test.com"
	basePath   = "s3.url.test"
	BucketName = "test"
	accessKey  = "test-access-key"
	secretKey  = "test-secret-key"
	awsRegion  = "region"
)

func getRequest() models.S3ConfigRequest {
	return models.S3ConfigRequest{
		Endpoint:   endPoint,
		BucketName: BucketName,
		BasePath:   basePath,
		AccessKey:  accessKey,
		SecretKey:  secretKey,
		Region:     awsRegion,
	}
}

func TestS3BackupConfigCheck_Run(t *testing.T) {
	type args struct {
		config *models.Config
	}

	tests := []struct {
		name                   string
		isPassed               bool
		args                   args
		response               string
		httpStatusCode         int
		isError                bool
		requiredStatusResponse string
	}{
		{
			name:           "All the s3 checks passed",
			isPassed:       true,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: &models.Backup{
						ObjectStorage: &models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
							AWSRegion:  awsRegion,
						},
					},
				},
			},
			response: s3BackupCheckResponseExpectedSuccess,
		},
		{
			name:           "All the s3 checks failed",
			isPassed:       false,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 1,
					},
					Backup: &models.Backup{
						ObjectStorage: &models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
							AWSRegion:  awsRegion,
						},
					},
				},
			},
			response: s3BackupCheckResponseFailureExpected,
		},
		{
			name:           "Internal Server Error",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusInternalServerError,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: &models.Backup{
						ObjectStorage: &models.ObjectStorage{
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
							AWSRegion:  awsRegion,
						},
					},
				},
			},
			response:               "Internal Server Error",
			requiredStatusResponse: `{"error":{"code":500,"message":"Internal Server Error"}}`,
		},
		{
			name:           "Gateway Timeout",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusGatewayTimeout,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
						AutomateNodeIps:   []string{"1.1.1.1", "2.2.2.2"},
					},
					Backup: &models.Backup{
						ObjectStorage: &models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
							AWSRegion:  "ap-south",
						},
					},
				},
			},
			requiredStatusResponse: `{"error":{"code":504,"message":"context deadline exceeded"}}`,
			response:               "context deadline exceeded",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed, tt.requiredStatusResponse)
			defer server.Close()

			svc := NewS3BackupConfigCheck(
				logger.NewLogrusStandardLogger(),
				port,
			)

			tt.args.config.Hardware.AutomateNodeIps = []string{host, host}

			json.Unmarshal([]byte(tt.response), &want)
			for i := range want {
				want[i].Host = host
			}

			got := svc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, tt.args.config.Hardware.AutomateNodeCount)
				assert.NotNil(t, got[0].Result.Error)
				assert.Equal(t, "automate", got[0].NodeType)
				assert.Equal(t, got[0].Result.Error.Code, tt.httpStatusCode)
				assert.Equal(t, tt.response, got[0].Result.Error.Error())
			} else {
				assert.Equal(t, want, got)
			}

		})
	}
}

func TestRunS3BackupCheck(t *testing.T) {
	t.Run("NIl Hardware", func(t *testing.T) {
		svc := NewS3BackupConfigCheck(
			logger.NewLogrusStandardLogger(),
			"8081",
		)
		config := &models.Config{
			Hardware: nil,
		}

		got := svc.Run(config)

		assert.Len(t, got, 1)
		assert.Equal(t, "-", got[0].Host)
		assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
		assert.Equal(t, constants.S3_BACKUP_CONFIG, got[0].CheckType)
		assert.True(t, got[0].Result.Skipped)
		assert.Equal(t, constants.SKIP_MISSING_HARDWARE_MESSAGE, got[0].Result.SkipMessage)

	})

	t.Run("Empty Object storage", func(t *testing.T) {
		svc := NewS3BackupConfigCheck(
			logger.NewLogrusStandardLogger(),
			"8081",
		)
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{constants.LOCALHOST},
			},
			Backup: &models.Backup{
				ObjectStorage: &models.ObjectStorage{},
			},
		}

		got := svc.Run(config)

		assert.Len(t, got, 1)
		assert.Equal(t, constants.LOCALHOST, got[0].Host)
		assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
		assert.Equal(t, constants.S3_BACKUP_CONFIG, got[0].CheckType)
		assert.Equal(t, http.StatusBadRequest, got[0].Result.Error.Code)
		assert.Equal(t, constants.S3_BACKUP_MISSING, got[0].Result.Error.Message)
		assert.False(t, got[0].Result.Skipped)
	})
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.S3ConfigRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)

			wantReq := getRequest()

			assert.NotNil(t, got)
			assert.Equal(t, got, wantReq)
			if r.URL.Path == constants.S3_BACKUP_CHECK_API_PATH {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(s3BackupConfigCheckResponseFromOneNodeSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(s3BackupCheckResponseFailure))
				}
			}
		}))

		// Extract IP and port from the server's URL
		address := server.URL[strings.Index(server.URL, "//")+2:]
		colonIndex := strings.Index(address, ":")
		ip := address[:colonIndex]
		port := address[colonIndex+1:]

		return server, ip, port
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(requiredStatusCode)
		w.Write([]byte(requiredStatusResponse))
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewS3BackupConfigCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}

func TestS3ConfigSkippedResponse(t *testing.T) {
	type args struct {
		config    *models.Config
		checkType string
	}
	tests := []struct {
		name string
		args args
		want []models.CheckTriggerResponse
	}{
		{
			name: "Make the skip Response",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount:        1,
						AutomateNodeIps:          []string{constants.LOCALHOST},
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{constants.LOCALHOST},
					},
				},
				checkType: "s3-backup-config",
			},
			want: []models.CheckTriggerResponse{
				{
					NodeType:  "automate",
					CheckType: "s3-backup-config",
					Result: models.ApiResult{
						Passed:      false,
						Skipped:     true,
						Check:       "s3-backup-config",
						SkipMessage: constants.SKIP_BACKUP_TEST_MESSAGE_S3,
					},
					Host: constants.LOCALHOST,
				},
				{
					NodeType:  "chef-infra-server",
					CheckType: "s3-backup-config",
					Result: models.ApiResult{
						Passed:      false,
						Skipped:     true,
						Check:       "s3-backup-config",
						SkipMessage: constants.SKIP_BACKUP_TEST_MESSAGE_S3,
					},
					Host: constants.LOCALHOST,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := s3ConfigSkippedResponse(tt.args.config, tt.args.checkType, constants.SKIP_BACKUP_TEST_MESSAGE_S3)
			assert.Equal(t, tt.want, got)
		})
	}
}
