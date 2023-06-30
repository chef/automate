package nfsmountbackupchecktrigger

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
	hardware = &models.Hardware{
		AutomateNodeCount:        1,
		AutomateNodeIps:          []string{"172.154.0.1"},
		ChefInfraServerNodeCount: 1,
		ChefInfraServerNodeIps:   []string{"172.154.0.3"},
		OpenSearchNodeCount:      1,
		OpenSearchNodeIps:        []string{"172.154.0.8"},
		PostgresqlNodeCount:      1,
		PostgresqlNodeIps:        []string{"172.154.0.5"},
	}
)

const (
	mountLocation = "test-mount"
	TYPE          = "self-managed"

	nfsMountResponse = `{
		"status": "SUCCESS",
		"result": [
		  {
			"ip": "172.154.0.1",
			"node_type": "automate",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.3",
			"node_type": "chef-infra-server",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.5",
			"node_type": "postgresql",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.8",
			"node_type": "opensearch",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  }
		]
	  }
	`

	nfsForSameFrontendNodes = `{
		"status": "SUCCESS",
		"result": [
		  {
			"ip": "172.154.0.1",
			"node_type": "automate",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.1",
			"node_type": "chef-infra-server",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.5",
			"node_type": "postgresql",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  },
		  {
			"ip": "172.154.0.8",
			"node_type": "opensearch",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location is shared across given nodes",
				"error_msg": "",
				"resolution_msg": ""
			  }
			]
		  }
		]
	  }`

	nfsMountResponseFailureMountFailure = `
	{
		"status": "SUCCESS",
		"result": [
		  {
			"ip": "172.154.0.1",
			"node_type": "automate",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": false,
				"success_msg": "",
				"error_msg": "NFS mount location not found",
				"resolution_msg": "NFS volume should be mounted on <mount_location>"
			  },
			  {
				"title": "NFS mount",
				"passed": false,
				"success_msg": "",
				"error_msg": "NFS mount location (not found) is not shared across all given nodes",
				"resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
			  }
			]
		  },
		  {
			"ip": "172.154.0.3",
			"node_type": "chef-infra-server",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": false,
				"success_msg": "",
				"error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
				"resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
			  }
			]
		  },
		  {
			"ip": "172.154.0.5",
			"node_type": "postgresql",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": false,
				"success_msg": "",
				"error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
				"resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
			  }
			]
		  },
		  {
			"ip": "172.154.0.8",
			"node_type": "opensearch",
			"checks": [
			  {
				"title": "NFS mount",
				"passed": true,
				"success_msg": "NFS mount location found",
				"error_msg": "",
				"resolution_msg": ""
			  },
			  {
				"title": "NFS mount",
				"passed": false,
				"success_msg": "",
				"error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
				"resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
			  }
			]
		  }
		]
	  }
	`

	nfsMountTriggerResponse = `
	[
    {
        "status": "SUCCESS",
        "host": "172.154.0.1",
        "node_type": "automate",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
    {
        "status": "SUCCESS",
        "host": "172.154.0.3",
        "node_type": "chef-infra-server",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
	{
        "status": "SUCCESS",
        "host": "172.154.0.5",
        "node_type": "postgresql",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
	{
        "status": "SUCCESS",
        "host": "172.154.0.8",
        "node_type": "opensearch",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    }
]`

	nfsMountResponseFailureMountFailureTriggerResponse = `
[
    {
        "status": "SUCCESS",
        "host": "172.154.0.1",
        "node_type": "automate",
        "result": {
            "passed": false,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": false,
                    "success_msg": "",
                    "error_msg": "NFS mount location not found",
                    "resolution_msg": "NFS volume should be mounted on <mount_location>"
                },
                {
                    "title": "NFS mount",
                    "passed": false,
                    "success_msg": "",
                    "error_msg": "NFS mount location (not found) is not shared across all given nodes",
                    "resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
                }
            ]
        }
    },
    {
        "status": "SUCCESS",
        "host": "172.154.0.3",
        "node_type": "chef-infra-server",
        "result": {
            "passed": false,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": false,
                    "success_msg": "",
                    "error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
                    "resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
                }
            ]
        }
    },
    {
        "status": "SUCCESS",
        "host": "172.154.0.5",
        "node_type": "postgresql",
        "result": {
            "passed": false,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": false,
                    "success_msg": "",
                    "error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
                    "resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
                }
            ]
        }
    },
    {
        "status": "SUCCESS",
        "host": "172.154.0.8",
        "node_type": "opensearch",
        "result": {
            "passed": false,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": false,
                    "success_msg": "",
                    "error_msg": "NFS mount location (<nfs>) is not shared across all given nodes",
                    "resolution_msg": "NFS volume (<nfs>) should be common across all given nodes at mount location: <mount_location>"
                }
            ]
        }
    }
]
`

	parseErrorApiResponse = `
{
	"status": "SUCCESS",
	"result": [
	  {
		"ip": "172.154.0.1",
		"node_type": "automate",
		"checks": [
		  {
			"title": "NFS mount",
			"passed": true,
			"success_msg": "NFS mount location found",
			"error_msg": "",
			"resolution_msg": ""
		  },
		  {
			"title": "NFS mount",
			"passed": true,
			"success_msg": "NFS mount location is shared across given nodes",
			"error_msg": "",
			"resolution_msg": ""
		  }`

	nfsMountResponseForSameFrontEnd = `[
    {
        "status": "SUCCESS",
        "host": "172.154.0.1",
        "node_type": "automate",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
    {
        "status": "SUCCESS",
        "host": "172.154.0.1",
        "node_type": "chef-infra-server",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
	{
        "status": "SUCCESS",
        "host": "172.154.0.5",
        "node_type": "postgresql",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    },
	{
        "status": "SUCCESS",
        "host": "172.154.0.8",
        "node_type": "opensearch",
        "result": {
            "passed": true,
            "checks": [
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location found",
                    "error_msg": "",
                    "resolution_msg": ""
                },
                {
                    "title": "NFS mount",
                    "passed": true,
                    "success_msg": "NFS mount location is shared across given nodes",
                    "error_msg": "",
                    "resolution_msg": ""
                }
            ]
        }
    }
]
	`
)

func TestNfsBackupConfigCheck_Run(t *testing.T) {
	type args struct {
		config *models.Config
	}
	tests := []struct {
		name                  string
		args                  args
		response              string
		isPassed              bool
		httpStatusCode        int
		isError               bool
		parseErrorApiResponse bool
		wantRequest           models.NFSMountRequest
		isSameFrontEnd        bool
	}{
		{
			name: "Passed Response for chef-server,automate,opensearch and postgresql",
			args: args{
				config: &models.Config{
					ExternalDbType: TYPE,
					Hardware:       hardware,
					Backup: &models.Backup{
						FileSystem: &models.FileSystem{
							MountLocation: mountLocation,
						},
					},
				},
			},
			response:       nfsMountTriggerResponse,
			isPassed:       true,
			httpStatusCode: http.StatusOK,
			isError:        false,
			wantRequest:    getRequest(),
			isSameFrontEnd: false,
		},
		{
			name: "Failure Mount Response for chef-server,automate,opensearch and postgresql",
			args: args{
				config: &models.Config{
					ExternalDbType: TYPE,
					Hardware:       hardware,
					Backup: &models.Backup{
						FileSystem: &models.FileSystem{
							MountLocation: mountLocation,
						},
					},
				},
			},
			response:       nfsMountResponseFailureMountFailureTriggerResponse,
			isPassed:       false,
			httpStatusCode: http.StatusOK,
			isError:        false,
			wantRequest:    getRequest(),
			isSameFrontEnd: false,
		},

		{
			name: "Recived Internal Server Error From the API",
			args: args{
				config: &models.Config{
					ExternalDbType: TYPE,
					Hardware:       hardware,
					Backup: &models.Backup{
						FileSystem: &models.FileSystem{
							MountLocation: mountLocation,
						},
					},
				},
			},
			response:       "unexpected end of JSON input",
			isPassed:       false,
			httpStatusCode: http.StatusInternalServerError,
			isError:        true,
			wantRequest:    getRequest(),
			isSameFrontEnd: false,
		},
		{
			name: "Invalid Response from service",
			args: args{
				config: &models.Config{
					ExternalDbType: TYPE,
					Hardware:       hardware,
					Backup: &models.Backup{
						FileSystem: &models.FileSystem{
							MountLocation: mountLocation,
						},
					},
				},
			},
			response:              "Error while triggering NFS Mount API from Batch Check API: unexpected end of JSON input",
			parseErrorApiResponse: true,
			isPassed:              false,
			httpStatusCode:        http.StatusOK,
			isError:               true,
			wantRequest:           getRequest(),
			isSameFrontEnd:        false,
		},
		{
			name: "Checking for Same Front end Nodes",
			args: args{
				config: &models.Config{
					ExternalDbType: TYPE,
					Hardware: &models.Hardware{
						AutomateNodeIps:        hardware.AutomateNodeIps,
						ChefInfraServerNodeIps: hardware.AutomateNodeIps,
						PostgresqlNodeIps:      hardware.PostgresqlNodeIps,
						OpenSearchNodeIps:      hardware.OpenSearchNodeIps,
					},
					Backup: &models.Backup{
						FileSystem: &models.FileSystem{
							MountLocation: mountLocation,
						},
					},
				},
			},
			response:              nfsMountResponseForSameFrontEnd,
			parseErrorApiResponse: false,
			isPassed:              true,
			httpStatusCode:        http.StatusOK,
			isError:               false,
			wantRequest: models.NFSMountRequest{
				ExternalDbType:         TYPE,
				AutomateNodeIPs:        hardware.AutomateNodeIps,
				ChefInfraServerNodeIPs: hardware.AutomateNodeIps,
				PostgresqlNodeIPs:      hardware.PostgresqlNodeIps,
				OpensearchNodeIPs:      hardware.OpenSearchNodeIps,
				MountLocation:          mountLocation,
			},
			isSameFrontEnd: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {

			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed, tt.parseErrorApiResponse, tt.isSameFrontEnd, tt.wantRequest)
			defer server.Close()

			nbc := NewNfsBackupConfigCheck(
				logger.NewLogrusStandardLogger(),
				port,
			)
			nbc.host = host

			got := nbc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, 4)
				assert.NotNil(t, got[0].Result.Error)
				assert.NotNil(t, got[0].Host)
				assert.Contains(t, tt.response, got[0].Result.Error.Message)

			} else {
				var want []models.CheckTriggerResponse
				json.Unmarshal([]byte(tt.response), &want)
				assert.Equal(t, want, got)
			}

		})
	}
}

func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, parseErrorResponse bool, sameFrontend bool, wantRequest models.NFSMountRequest) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.NFSMountRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)
			assert.Equal(t, wantRequest, got)
			if r.URL.Path == constants.NFS_MOUNT_API_PATH {
				if isPassed && sameFrontend {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(nfsForSameFrontendNodes))
				} else if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(nfsMountResponse))
				} else if !isPassed && !parseErrorResponse {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(nfsMountResponseFailureMountFailure))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(parseErrorApiResponse))
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

func getRequest() models.NFSMountRequest {
	return models.NFSMountRequest{
		ExternalDbType:         TYPE,
		AutomateNodeIPs:        hardware.AutomateNodeIps,
		ChefInfraServerNodeIPs: hardware.ChefInfraServerNodeIps,
		PostgresqlNodeIPs:      hardware.PostgresqlNodeIps,
		OpensearchNodeIPs:      hardware.OpenSearchNodeIps,
		MountLocation:          mountLocation,
	}
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewNfsBackupConfigCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}

func TestHardwareResourceCountCheck_TriggerHardwareResourceCountCheck(t *testing.T) {
	t.Run("Nil Hardware", func(t *testing.T) {
		config := &models.Config{
			Hardware:   nil,
			ExternalOS: nil,
		}

		newOS := NewNfsBackupConfigCheck(logger.NewLogrusStandardLogger(), "8080")
		got := newOS.Run(config)
		assert.Len(t, got, 4)
		for _, v := range got {
			if v.CheckType == constants.BASTION {
				assert.Equal(t, constants.LOCALHOST, v.Host)
			}
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.CheckType)
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.Result.Check)
			assert.True(t, v.Result.Skipped)
		}
	})

	t.Run("Nil FileSystem", func(t *testing.T) {
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount:        1,
				AutomateNodeIps:          []string{"12.12.1.6"},
				ChefInfraServerNodeCount: 1,
				ChefInfraServerNodeIps:   []string{"12.12.1.7"},
				PostgresqlNodeCount:      1,
				PostgresqlNodeIps:        []string{"12.12.1.8"},
				OpenSearchNodeCount:      1,
				OpenSearchNodeIps:        []string{"12.12.1.9"},
			},
			Backup: &models.Backup{
				FileSystem: nil,
			},
		}

		newOS := NewNfsBackupConfigCheck(logger.NewLogrusStandardLogger(), "8080")
		got := newOS.Run(config)
		assert.Len(t, got, 4)
		assert.Equal(t, "12.12.1.6", got[0].Host)
		for _, v := range got {
			if v.CheckType == constants.BASTION {
				assert.Equal(t, constants.LOCALHOST, v.Host)
			}
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.CheckType)
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.Result.Check)
			assert.True(t, v.Result.Skipped)
		}
	})
	t.Run("Empty FileSystem", func(t *testing.T) {
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount:        1,
				AutomateNodeIps:          []string{"12.12.1.6"},
				ChefInfraServerNodeCount: 1,
				ChefInfraServerNodeIps:   []string{"12.12.1.7"},
				PostgresqlNodeCount:      1,
				PostgresqlNodeIps:        []string{"12.12.1.8"},
				OpenSearchNodeCount:      1,
				OpenSearchNodeIps:        []string{"12.12.1.9"},
			},
			Backup: &models.Backup{
				FileSystem: &models.FileSystem{},
			},
		}

		newOS := NewNfsBackupConfigCheck(logger.NewLogrusStandardLogger(), "8080")
		got := newOS.Run(config)
		assert.Len(t, got, 4)
		for _, v := range got {
			assert.Equal(t, "12.12.1.6", got[0].Host)
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.CheckType)
			assert.Equal(t, constants.NFS_BACKUP_CONFIG, v.Result.Check)
			assert.Equal(t, http.StatusBadRequest, v.Result.Error.Code)
			assert.Equal(t, constants.MOUNT_LOCATION_MISSING, v.Result.Error.Message)
			assert.False(t, v.Result.Skipped)
		}
	})

}
