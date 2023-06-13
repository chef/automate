package externalpostgresqlchecktrigger

import (
	"encoding/json"
	"io"
	"net"
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
	externalPostgresqlResponseSuccess = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Connection successfully tested",
			  "passed": true,
			  "status": "PASS",
			  "success_msg": "Machine is able to connect with External Managed Postgres",
			  "error_msg": "",
			  "resolution_msg": "",
			  "debug_msg": ""
			}
		  ]
		}
	}`

	externalPostgresqlResponseSuccessAutomateExpected = `[
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": true,
			  "checks": [
				{
				  "title": "Connection successfully tested",
				  "passed": true,
				  "status": "PASS",
				  "success_msg": "Machine is able to connect with External Managed Postgres",
				  "error_msg": "",
				  "resolution_msg": "",
				  "debug_msg": ""
				}
			  ]
			}
		},
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": true,
			  "checks": [
				{
				  "title": "Connection successfully tested",
				  "passed": true,
				  "status": "PASS",
				  "success_msg": "Machine is able to connect with External Managed Postgres",
				  "error_msg": "",
				  "resolution_msg": "",
				  "debug_msg": ""
				}
			  ]
			}
		}
		]
	  `
	externalPostgresqlResponseSuccessChefServerExpected = `[
	  {
		  "status": "SUCCESS",
		  "node_type" :"chef-infra-server",
		  "result": {
			"passed": true,
			"checks": [
			  {
				"title": "Connection successfully tested",
				"passed": true,
				"status": "PASS",
				"success_msg": "Machine is able to connect with External Managed Postgres",
				"error_msg": "",
				"resolution_msg": "",
				"debug_msg": ""
			  }
			]
		  }
	  },
	  {
		  "status": "SUCCESS",
		  "node_type" :"chef-infra-server",
		  "result": {
			"passed": true,
			"checks": [
			  {
				"title": "Connection successfully tested",
				"passed": true,
				"status": "PASS",
				"success_msg": "Machine is able to connect with External Managed Postgres",
				"error_msg": "",
				"resolution_msg": "",
				"debug_msg": ""
			  }
			]
		  }
	  }]`
	externalPostgresqlResponseFailure = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Postgres Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed Postgres",
			  "resolution_msg": "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	}
	
	`
	externalPostgresqlResponseFailureAutomateExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"automate",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Postgres Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed Postgres",
			  "resolution_msg": "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
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
			  "title": "Postgres Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed Postgres",
			  "resolution_msg": "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	}
	]`

	externalPostgresqlResponseFailureChefServerExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Postgres Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed Postgres",
			  "resolution_msg": "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	},
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "Postgres Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed Postgres",
			  "resolution_msg": "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	}]`
	postgresqlInstanceUrl       = "https://abc.com:5432"
	postgresqlSuperUserUserName = "postgres"
	postgresqlSuperUserPassword = "Chefautomate"
	postgresqlDbUserUserName    = "postgres"
	postgresqlDbUserPassword    = "Chefautomate"
	postgresqlRootCert          = ""
)

func getRequest() models.ExternalPgRequest {
	host, port, _ := net.SplitHostPort(postgresqlInstanceUrl)
	return models.ExternalPgRequest{
		PostgresqlInstanceUrl:       host,
		PostgresqlInstancePort:      port,
		PostgresqlSuperUserUserName: postgresqlSuperUserUserName,
		PostgresqlSuperUserPassword: postgresqlSuperUserPassword,
		PostgresqlDbUserUserName:    postgresqlDbUserUserName,
		PostgresqlDbUserPassword:    postgresqlDbUserPassword,
		PostgresqlRootCert:          postgresqlRootCert,
	}

}

func TestPostgresCheckAutomate_Run(t *testing.T) {
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
			name:           "Postgres Checks are passed",
			isPassed:       true,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					ExternalPG: models.ExternalPG{
						PGInstanceURL:       postgresqlInstanceUrl,
						PGSuperuserName:     postgresqlSuperUserUserName,
						PGSuperuserPassword: postgresqlSuperUserPassword,
						PGDbUserName:        postgresqlDbUserUserName,
						PGDbUserPassword:    postgresqlDbUserPassword,
						PGRootCert:          postgresqlRootCert,
					},
				},
			},
			response: externalPostgresqlResponseSuccessAutomateExpected,
		},

		{
			name:           "Postgres checks are failed",
			isPassed:       false,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 2,
					},
					ExternalPG: models.ExternalPG{
						PGInstanceURL:       postgresqlInstanceUrl,
						PGSuperuserName:     postgresqlSuperUserUserName,
						PGSuperuserPassword: postgresqlSuperUserPassword,
						PGDbUserName:        postgresqlDbUserUserName,
						PGDbUserPassword:    postgresqlDbUserPassword,
						PGRootCert:          postgresqlRootCert,
					},
				},
			},
			response: externalPostgresqlResponseFailureAutomateExpected,
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
					ExternalPG: models.ExternalPG{
						PGInstanceURL:       postgresqlInstanceUrl,
						PGSuperuserName:     postgresqlSuperUserUserName,
						PGSuperuserPassword: postgresqlSuperUserPassword,
						PGDbUserName:        postgresqlDbUserUserName,
						PGDbUserPassword:    postgresqlDbUserPassword,
						PGRootCert:          postgresqlRootCert,
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
					ExternalPG: models.ExternalPG{
						PGInstanceURL:       postgresqlInstanceUrl,
						PGSuperuserName:     postgresqlSuperUserUserName,
						PGSuperuserPassword: postgresqlSuperUserPassword,
						PGDbUserName:        postgresqlDbUserUserName,
						PGDbUserPassword:    postgresqlDbUserPassword,
						PGRootCert:          postgresqlRootCert,
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

			svc := NewExternalPostgresCheck(
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
				assert.NotNil(t, got[0].Result.Error)
				assert.Equal(t, constants.LOCALHOST, got[0].Host)
				assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
				assert.Equal(t, tt.httpStatusCode, got[0].Result.Error.Code)
				assert.Equal(t, tt.response, got[0].Result.Error.Error())
			} else {
				assert.Nil(t, got[0].Result.Error)
				assert.Equal(t, constants.LOCALHOST, got[0].Host)
				assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
				assert.Equal(t, want, got)
			}

		})
	}
}

func TestForChefserverPostgres(t *testing.T) {
	t.Run("ChefServer Postgres check pass", func(t *testing.T) {
		var want []models.CheckTriggerResponse

		config := models.Config{
			Hardware: models.Hardware{
				ChefInfraServerNodeCount: 2,
			},
			ExternalPG: models.ExternalPG{
				PGInstanceURL:       postgresqlInstanceUrl,
				PGSuperuserName:     postgresqlSuperUserUserName,
				PGSuperuserPassword: postgresqlSuperUserPassword,
				PGDbUserName:        postgresqlDbUserUserName,
				PGDbUserPassword:    postgresqlDbUserPassword,
				PGRootCert:          postgresqlRootCert,
			},
		}
		isError := false
		server, host, port := createDummyServer(t, http.StatusOK, true)
		defer server.Close()
		svc := NewExternalPostgresCheck(
			logger.NewLogrusStandardLogger(),
			port,
		)
		config.Hardware.ChefInfraServerNodeIps = []string{host, host}

		json.Unmarshal([]byte(externalPostgresqlResponseSuccessChefServerExpected), &want)
		for i := range want {
			want[i].Host = host
		}

		got := svc.Run(config)
		isError = false
		if isError {
			assert.NotNil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, http.StatusOK, got[0].Result.Error.Code)
			assert.Equal(t, externalPostgresqlResponseSuccessChefServerExpected, got[0].Result.Error.Error())
		} else {
			assert.Nil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, want, got)
		}
	})
	t.Run("ChefServer Postgres check fail", func(t *testing.T) {
		var want []models.CheckTriggerResponse

		config := models.Config{
			Hardware: models.Hardware{
				ChefInfraServerNodeCount: 2,
			},
			ExternalPG: models.ExternalPG{
				PGInstanceURL:       postgresqlInstanceUrl,
				PGSuperuserName:     postgresqlSuperUserUserName,
				PGSuperuserPassword: postgresqlSuperUserPassword,
				PGDbUserName:        postgresqlDbUserUserName,
				PGDbUserPassword:    postgresqlDbUserPassword,
				PGRootCert:          postgresqlRootCert,
			},
		}
		isError := false
		server, host, port := createDummyServer(t, http.StatusOK, false)
		defer server.Close()
		svc := NewExternalPostgresCheck(
			logger.NewLogrusStandardLogger(),
			port,
		)
		config.Hardware.ChefInfraServerNodeIps = []string{host, host}

		json.Unmarshal([]byte(externalPostgresqlResponseFailureChefServerExpected), &want)
		for i := range want {
			want[i].Host = host
		}

		got := svc.Run(config)
		isError = false
		if isError {
			assert.NotNil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, http.StatusOK, got[0].Result.Error.Code)
			assert.Equal(t, externalPostgresqlResponseFailureChefServerExpected, got[0].Result.Error.Error())
		} else {
			assert.Nil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, want, got)
		}
	})
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.ExternalPgRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)

			wantReq := getRequest()

			assert.NotNil(t, got)
			assert.Equal(t, wantReq, got)
			if r.URL.Path == constants.EXTERNAL_POSTGRESQL_API_PATH {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(externalPostgresqlResponseSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(externalPostgresqlResponseFailure))
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

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewExternalPostgresCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}
