package opensearchs3bucketaccesschecktrigger

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

var (
	externalOs = &models.ExternalOS{
		OSDomainName:   "Name of the domain",
		OSDomainURL:    "open-search-url",
		OSRoleArn:      "Role-ARN",
		OSUsername:     "username",
		OSUserPassword: "password",
		OSCert:         "___CERT____",
	}

	s3Properties = &models.ObjectStorage{
		Endpoint:   "s3-url-com",
		BucketName: "test",
		BasePath:   "tt.com",
		AccessKey:  "access-kkey",
		SecretKey:  "secret-key",
		AWSRegion:  "ap-east",
	}
)

const (
	apiResponseSuccess = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Create test backup",
			  "passed": true,
			  "success_msg": "OpenSearch is able to create backup to provided S3",
			  "error_msg": "",
			  "resolution_msg": "",
			  "Skipped":false
			}
		  ]
		}
	  }
	`

	apiResponseFailure = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Create test backup",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "OpenSearch is not able to create backup to provided S3",
			  "resolution_msg": "Setup OpenSearch with valid configurations for S3 backup",
			  "Skipped":false
			}
		  ]
		}
	  }
	`

	apiTriggerResponseSuccess = `
	[
		{
			"status": "SUCCESS",
			"host": "open-search-url",
			"node_type": "opensearch",
			"result": {
				"passed": true,
				"checks": [
					{
						"title": "Create test backup",
						"passed": true,
						"success_msg": "OpenSearch is able to create backup to provided S3",
						"error_msg": "",
						"resolution_msg": "",
						"Skipped":false
					}
				]
			}
		}
	]
 `

	apiTriggerResponseFailure = `[
		{
		"status": "SUCCESS",
		"host": "open-search-url",
		"node_type": "opensearch",
		"result": {
			"passed": false,
			"checks": [
				{
					"title": "Create test backup",
					"passed": false,
					"success_msg": "",
					"error_msg": "OpenSearch is not able to create backup to provided S3",
					"resolution_msg": "Setup OpenSearch with valid configurations for S3 backup",
					"Skipped":false
				}
			]
		}
	}
	]
 `
)

func TestOpensearchS3BucketAccessCheck_Run(t *testing.T) {

	type args struct {
		config *models.Config
	}
	tests := []struct {
		name             string
		args             args
		httpResponseCode int
		isPassed         bool
		response         string
		isError          bool
	}{
		{
			name: "Success Response",
			args: args{
				config: &models.Config{
					ExternalOS: externalOs,
					Backup: &models.Backup{
						ObjectStorage: s3Properties,
					},
				},
			},
			httpResponseCode: http.StatusOK,
			isPassed:         true,
			response:         apiTriggerResponseSuccess,
			isError:          false,
		},
		{
			name: "Failure Response",
			args: args{
				config: &models.Config{
					ExternalOS: externalOs,
					Backup: &models.Backup{
						ObjectStorage: s3Properties,
					},
				},
			},
			httpResponseCode: http.StatusOK,
			isPassed:         false,
			response:         apiTriggerResponseFailure,
			isError:          false,
		},
		{
			name: "Internal Server Error",
			args: args{
				config: &models.Config{
					ExternalOS: externalOs,
					Backup: &models.Backup{
						ObjectStorage: s3Properties,
					},
				},
			},
			httpResponseCode: http.StatusInternalServerError,
			isPassed:         false,
			response:         "error while connecting to the endpoint, received invalid status code",
			isError:          true,
		},
		{
			name: "Nil OS and Object storage",
			args: args{
				config: &models.Config{
					ExternalOS: nil,
					Backup: &models.Backup{
						ObjectStorage: nil,
					},
				},
			},
			isError: false,
		},
		{
			name: "Empty OS and Object storage",
			args: args{
				config: &models.Config{
					ExternalOS: &models.ExternalOS{
						OSDomainURL: "dave.com",
					},
					Backup: &models.Backup{
						ObjectStorage: &models.ObjectStorage{},
					},
				},
			},
			httpResponseCode: http.StatusBadRequest,
			isError:          false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			json.Unmarshal([]byte(tt.response), &want)
			server, host, port := createDummyServer(t, tt.httpResponseCode, tt.isPassed)
			defer server.Close()

			osb := NewOpensearchS3BucketAccessCheck(logger.NewLogrusStandardLogger(), port)
			osb.host = host

			got := osb.Run(tt.args.config)
			if tt.isError {
				assert.Len(t, got, 1)
				assert.NotNil(t, got[0].Result.Error.Error)
				assert.Equal(t, constants.OPENSEARCH, got[0].NodeType)
				assert.Equal(t, tt.httpResponseCode, got[0].Result.Error.Code)
				assert.Equal(t, "open-search-url", got[0].Host)
				assert.Equal(t, tt.response, got[0].Result.Error.Error())
			} else {
				if tt.name == "Nil OS and Object storage" {
					assert.Len(t, got, 1)
					assert.Equal(t, "-", got[0].Host)
					assert.Equal(t, constants.OPENSEARCH, got[0].NodeType)
					assert.Equal(t, constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS, got[0].CheckType)
					assert.True(t, got[0].Result.Skipped)
				} else if tt.name == "Empty OS and Object storage" {
					assert.Len(t, got, 1)
					assert.Equal(t, "dave.com", got[0].Host)
					assert.Equal(t, constants.OPENSEARCH, got[0].NodeType)
					assert.Equal(t, constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS, got[0].CheckType)
					assert.Equal(t, constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS, got[0].Result.Check)
					assert.Equal(t, http.StatusBadRequest, got[0].Result.Error.Code)
					assert.Equal(t, constants.OBJECT_STORAGE_MISSING, got[0].Result.Error.Message)
					assert.False(t, got[0].Result.Skipped)
				} else {
					assert.Equal(t, want, got)
					assert.NotNil(t, got)
					assert.Nil(t, got[0].Result.Error)
					assert.Equal(t, "open-search-url", got[0].Host)
				}
			}

		})
	}
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			want := getS3BackupOSRequest()
			var got models.S3BackupDetails
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)
			assert.Equal(t, want, got)

			if r.URL.Path == constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS_API_PATH {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(apiResponseSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(apiResponseFailure))
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

func getS3BackupOSRequest() models.S3BackupDetails {
	return models.S3BackupDetails{
		Endpoint:   externalOs.OSDomainURL,
		Username:   externalOs.OSUsername,
		Password:   externalOs.OSUserPassword,
		S3Bucket:   s3Properties.BucketName,
		S3BasePath: s3Properties.BasePath,
		AccessKey:  s3Properties.AccessKey,
		SecretKey:  s3Properties.SecretKey,
		AWSRegion:  s3Properties.AWSRegion,
		AWSRoleArn: externalOs.OSRoleArn,
	}
}

func Test_setHostAsOpensearchInResponse(t *testing.T) {
	type args struct {
		response      []models.CheckTriggerResponse
		osExternalUrl string
	}
	tests := []struct {
		name string
		args args
		want []models.CheckTriggerResponse
	}{
		{
			name: "correct host",
			args: args{
				osExternalUrl: "test",
				response: []models.CheckTriggerResponse{
					{
						Status:   "Passed",
						NodeType: constants.OPENSEARCH,
					},
				},
			},
			want: []models.CheckTriggerResponse{
				{
					Status:   "Passed",
					NodeType: constants.OPENSEARCH,
					Host:     "test",
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := setHostAsOpensearchInResponse(tt.args.response, tt.args.osExternalUrl)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewOpensearchS3BucketAccessCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}
