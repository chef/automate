package gcsbackupchecktrigger

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
	GCPBackupConfigCheckResponseFromOneNodeSuccess = `{
		"status": "SUCCESS",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "GCP connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with GCP using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "GCP bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the GCP bucket using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`

	GCPBackupCheckResponseExpectedSuccess = `[
	{
		"status": "SUCCESS",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "GCP connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with GCP using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "GCP bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the GCP bucket using the provided access key and secret key",
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
			  "title": "GCP connection test",
			  "passed": true,
			  "success_msg": "Machine is able to connect with GCP using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "GCP bucket access test",
			  "passed": true,
			  "success_msg": "Machine is able to access the GCP bucket using the provided access key and secret key",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }
	]
	`
	GCPBackupCheckResponseFailure = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "GCP connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with GCP using the provided access key and secret key",
			  "resolution_msg": "Provide the correct GCP url or access or secret keys"
			},
			{
			  "title": "GCP bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the GCP bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the GCP bucket"
			}
		  ]
		}
	  }
	  `

	GCPBackupCheckResponseFailureExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"automate",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "GCP connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with GCP using the provided access key and secret key",
			  "resolution_msg": "Provide the correct GCP url or access or secret keys"
			},
			{
			  "title": "GCP bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the GCP bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the GCP bucket"
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
			  "title": "GCP connection test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to connect with GCP using the provided access key and secret key",
			  "resolution_msg": "Provide the correct GCP url or access or secret keys"
			},
			{
			  "title": "GCP bucket access test",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "Machine is not able to access the GCP bucket using the provided access key and secret key",
			  "resolution_msg": "Provide the necessary access to the GCP bucket"
			}
		  ]
		}
	  }
	  ]
	  `
	GCPBackupConfigCheckResponseSkipped = `{
		"status": "",
		"node_type": "automate",
		"result": {
		  "passed": true,
		  "checks": [
			{}
		  ]
		  "Skipped": true,
		  "skip_message":"- Backup configuration not set to object_storage/gcs"
		}
	  }`
	bucketName             = "test"
	accountServiceFilepath = "/test/account.json"
	typeAccount            = "servicefile"
	projectID              = "dev-project"
	location               = "gcs"
)

func getRequest() models.GCPCloudStorageConfigRequest {
	return models.GCPCloudStorageConfigRequest{
		BucketName:               bucketName,
		GoogleServiceAccountFile: accountServiceFilepath,
		GcpServiceAccount: &models.GcpServiceAccount{
			Type:      typeAccount,
			ProjectID: projectID,
		},
	}
}

func TestGCPBackupConfigCheck_Run(t *testing.T) {
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
			name:           "All the GCP checks passed",
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
							Location:                 location,
							BucketName:               bucketName,
							GoogleServiceAccountFile: accountServiceFilepath,
							GcpServiceAccount: &models.GcpServiceAccount{
								Type:      typeAccount,
								ProjectID: projectID,
							},
						},
					},
				},
			},
			response: GCPBackupCheckResponseExpectedSuccess,
		},
		{
			name:           "All the GCP checks failed",
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
							Location:                 location,
							BucketName:               bucketName,
							GoogleServiceAccountFile: accountServiceFilepath,
							GcpServiceAccount: &models.GcpServiceAccount{
								Type:      typeAccount,
								ProjectID: projectID,
							},
						},
					},
				},
			},
			response: GCPBackupCheckResponseFailureExpected,
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
							Location:                 location,
							BucketName:               bucketName,
							GoogleServiceAccountFile: accountServiceFilepath,
							GcpServiceAccount: &models.GcpServiceAccount{
								Type:      typeAccount,
								ProjectID: projectID,
							},
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
							Location:                 location,
							BucketName:               bucketName,
							GoogleServiceAccountFile: accountServiceFilepath,
							GcpServiceAccount: &models.GcpServiceAccount{
								Type:      typeAccount,
								ProjectID: projectID,
							},
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

			svc := NewGcsBackupConfigCheck(
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

func TestRunGCPBackupCheck(t *testing.T) {
	t.Run("NIl Hardware", func(t *testing.T) {
		svc := NewGcsBackupConfigCheck(
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
		assert.Equal(t, constants.GCP_BACKUP_CONFIG, got[0].CheckType)
		assert.True(t, got[0].Result.Skipped)
		assert.Equal(t, constants.SKIP_MISSING_HARDWARE_MESSAGE, got[0].Result.SkipMessage)

	})

	t.Run("Empty Object storage", func(t *testing.T) {
		svc := NewGcsBackupConfigCheck(
			logger.NewLogrusStandardLogger(),
			"8081",
		)
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{constants.LOCALHOST},
			},
			Backup: &models.Backup{
				ObjectStorage: &models.ObjectStorage{
					Location: "gcs",
				},
			},
		}

		got := svc.Run(config)

		assert.Len(t, got, 1)
		assert.Equal(t, constants.LOCALHOST, got[0].Host)
		assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
		assert.Equal(t, constants.GCP_BACKUP_CONFIG, got[0].CheckType)
		assert.Equal(t, http.StatusBadRequest, got[0].Result.Error.Code)
		assert.Equal(t, constants.GCS_BACKUP_MISSING, got[0].Result.Error.Message)
		assert.False(t, got[0].Result.Skipped)
	})

	t.Run("Empty", func(t *testing.T) {
		svc := NewGcsBackupConfigCheck(
			logger.NewLogrusStandardLogger(),
			"8081",
		)
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{constants.LOCALHOST},
			},
			Backup: &models.Backup{
				ObjectStorage: &models.ObjectStorage{
					Location: "",
				},
			},
		}

		got := svc.Run(config)
		want := &models.ApiResult{
			SkipMessage: "- Backup configuration not set to object_storage/gcs",
		}
		assert.Equal(t, got[0].Result.SkipMessage, want.SkipMessage)
	})
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.GCPCloudStorageConfigRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)

			wantReq := getRequest()

			assert.NotNil(t, got)
			assert.Equal(t, got, wantReq)
			if r.URL.Path == constants.GCP_CLOUD_STORAGE_CONFIG_API_PATH {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(GCPBackupConfigCheckResponseFromOneNodeSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(GCPBackupCheckResponseFailure))
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
	fwc := NewGcsBackupConfigCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}

func TestGCPConfigSkippedResponse(t *testing.T) {
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
				checkType: "GCP-backup-config",
			},
			want: []models.CheckTriggerResponse{
				{
					NodeType:  "automate",
					CheckType: "GCP-backup-config",
					Result: models.ApiResult{
						Passed:      false,
						Skipped:     true,
						Check:       "GCP-backup-config",
						SkipMessage: constants.SKIP_BACKUP_TEST_MESSAGE_GCS,
					},
					Host: constants.LOCALHOST,
				},
				{
					NodeType:  "chef-infra-server",
					CheckType: "GCP-backup-config",
					Result: models.ApiResult{
						Passed:      false,
						Skipped:     true,
						Check:       "GCP-backup-config",
						SkipMessage: constants.SKIP_BACKUP_TEST_MESSAGE_GCS,
					},
					Host: constants.LOCALHOST,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := GcsConfigSkippedResponse(tt.args.config, tt.args.checkType, constants.SKIP_BACKUP_TEST_MESSAGE_GCS)
			assert.Equal(t, tt.want, got)
		})
	}
}
