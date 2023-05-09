package systemuserchecktrigger

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/require"
)

const (
	softwareVersionResp = `{
       "status": "success",
       "result": {
         "passed": true,
         "msg": "API result message",
         "check": "API check",
         "checks": [
           {
             "title": "Title 1",
             "passed": true,
             "success_msg": "Title 1 passed",
             "error_msg": "",
             "resolution_msg": "not required"
           },
           {
             "title": "Title 2",
             "passed": false,
             "success_msg": "",
             "error_msg": "Title 2 failed",
             "resolution_msg": "Check the log or conf"
           }
         ]
       },
       "host": ""
     }`
)

func TestSystemUserCheck_Run(t *testing.T) {
	t.Run("System User Check", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Define the response based on the test case
			w.WriteHeader(http.StatusOK)
			err := json.NewEncoder(w).Encode(json.RawMessage(softwareVersionResp))
			require.NoError(t, err)
		}))
		defer server.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount:   1,
				AutomateNodeIps:     []string{server.URL},
				PostgresqlNodeCount: 1,
				PostgresqlNodeIps:   []string{"127.0.0.1"},
			},
		}

		suc := NewSystemUserCheck(logger.NewLogrusStandardLogger(), "8080")
		ctr := suc.Run(config)
		// Assert the expected result
		require.Len(t, ctr, 2) // Modify the count based on your configuration
		require.Equal(t, ctr[server.URL].Error, &fiber.Error{Code: 500, Message: "error while connecting to the endpoint"})
		require.Error(t, ctr["127.0.0.1"].Error)
	})
}
