package s3backupchecktrigger

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
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
	  },
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
)

func TestS3BackupConfigCheck_Run(t *testing.T) {
	type args struct {
		config models.Config
	}

	tests := []struct {
		name           string
		isPassed       bool
		args           args
		response       string
		httpStatusCode int
		isError        bool
	}{
		{
			name:           "All the s3 checks passed",
			isPassed:       true,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: models.Backup{
						ObjectStorage: models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
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
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: models.Backup{
						ObjectStorage: models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
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
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: models.Backup{
						ObjectStorage: models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
						},
					},
				},
			},
			response: "error while connecting to the endpoint, received invalid status code",
		},
		{
			name:           "Gateway Timeout",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusGatewayTimeout,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					Backup: models.Backup{
						ObjectStorage: models.ObjectStorage{
							Endpoint:   endPoint,
							BucketName: BucketName,
							BasePath:   basePath,
							AccessKey:  accessKey,
							SecretKey:  secretKey,
						},
					},
				},
			},
			response: "error while connecting to the endpoint, received invalid status code",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed)
			defer server.Close()

			svc := &S3BackupConfigCheck{
				log:  logger.NewLogrusStandardLogger(),
				port: port,
			}

			tt.args.config.Hardware.AutomateNodeIps = []string{host, host}

			json.Unmarshal([]byte(tt.response), &want)
			for i, _ := range want {
				want[i].Host = host
			}

			got := svc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, tt.args.config.Hardware.AutomateNodeCount)
				assert.NotNil(t, got[0].Error)
				assert.Equal(t, "automate", got[0].NodeType)
				assert.Equal(t, got[0].Error.Code, tt.httpStatusCode)
				assert.Equal(t, tt.response, got[0].Error.Error())
			} else {
				assert.Equal(t, want, got)
			}

		})
	}
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
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
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}
