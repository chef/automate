package nfsmountbackupchecktrigger

import (
	"encoding/json"
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
	automateIps   = []string{"172.154.0.1"}
	chefserverIps = []string{"172.154.0.3"}
	hardware      = models.Hardware{
		AutomateNodeCount:        1,
		AutomateNodeIps:          automateIps,
		ChefInfraServerNodeCount: 1,
		ChefInfraServerNodeIps:   chefserverIps,
		OpenSearchNodeCount:      1,
		OpenSearchNodeIps:        []string{"172.154.0.8"},
		PostgresqlNodeIps:        []string{"172.154.0.5"},
	}
)

const (
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
)

func TestNfsBackupConfigCheck_Run(t *testing.T) {
	type args struct {
		config models.Config
	}
	tests := []struct {
		name                  string
		args                  args
		response              string
		isPassed              bool
		httpStatusCode        int
		isError               bool
		parseErrorApiResponse bool
	}{
		{
			name: "Passed Response for chef-server,automate,opensearch and postgresql",
			args: args{
				config: models.Config{
					Hardware: hardware,
				},
			},
			response:       nfsMountTriggerResponse,
			isPassed:       true,
			httpStatusCode: http.StatusOK,
			isError:        false,
		},
		{
			name: "Failure Mount Response for chef-server,automate,opensearch and postgresql",
			args: args{
				config: models.Config{
					Hardware: hardware,
				},
			},
			response:       nfsMountResponseFailureMountFailureTriggerResponse,
			isPassed:       false,
			httpStatusCode: http.StatusOK,
			isError:        false,
		},

		{
			name: "Recived Internal Server Error From the API",
			args: args{
				config: models.Config{
					Hardware: hardware,
				},
			},
			response:       "Error while triggering NFS Mount API from Batch Check API: 500 Internal Server Error",
			isPassed:       false,
			httpStatusCode: http.StatusInternalServerError,
			isError:        true,
		},
		{
			name: "Invalid Response from service",
			args: args{
				config: models.Config{
					Hardware: hardware,
				},
			},
			response:              "Error while triggering NFS Mount API from Batch Check API: unexpected end of JSON input",
			parseErrorApiResponse: true,
			isPassed:              false,
			httpStatusCode:        http.StatusOK,
			isError:               true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {

			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed, tt.parseErrorApiResponse)
			defer server.Close()

			nbc := &NfsBackupConfigCheck{
				log:  logger.NewLogrusStandardLogger(),
				port: port,
				host: host,
			}

			got := nbc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, 4)
				assert.NotNil(t, got[0].Error)
				assert.Equal(t, "automate", got[0].NodeType)
				assert.Equal(t, got[0].Error.Code, http.StatusInternalServerError)
				assert.Equal(t, tt.response, got[0].Error.Error())

			} else {
				var want []models.CheckTriggerResponse
				json.Unmarshal([]byte(tt.response), &want)
				assert.Equal(t, want, got)
			}

		})
	}
}

func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, parseErrorResponse bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.URL.Path == constants.NFS_MOUNT_API_PATH {
				if isPassed {
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
