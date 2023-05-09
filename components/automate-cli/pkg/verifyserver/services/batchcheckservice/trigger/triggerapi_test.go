package trigger

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/gofiber/fiber"

	"github.com/stretchr/testify/require"
)

const (
	softwareVersionResp = `{
      "passed": true,
      "checks": [
        {
          "title": "sysctl availability",
          "passed": true,
          "success_msg": "sysctl is available",
          "error_msg": "",
          "resolution_msg": ""
        },
        {
          "title": "systemd availability",
          "passed": true,
          "success_msg": "systemd is available and used as init system service",
          "error_msg": "",
          "resolution_msg": ""
        }
      ]
    }
    `
)

// mockTransport is a mock implementation of the http.RoundTripper interface
type mockTransport struct{}

// RoundTrip returns an error for every request
func (m *mockTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	return nil, fmt.Errorf("mock error")
}

func TestTriggerCheckAPI(t *testing.T) {
	t.Run("cannot reach", func(t *testing.T) {
		// create channels to receive the output and error messages
		outputCh := make(chan models.CheckTriggerResponse, 1)

		// make the HTTP request to an invalid URL
		go triggerCheckAPI("invalid-url", outputCh)

		// expect an error to be returned
		select {
		case result := <-outputCh:
			require.NotEmpty(t, result.Error)
			require.Equal(t, result.Error, &fiber.Error{Code: 400, Message: "error triggering the API invalid-url: Get \"invalid-url\": unsupported protocol scheme \"\""})

		}
	})

	t.Run("Bad request", func(t *testing.T) {
		// create channels to receive the output and error messages
		outputCh := make(chan models.CheckTriggerResponse, 1)

		// call the function being tested with an endpoint that returns an error status code
		mockServer2 := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusBadRequest)
		}))
		defer mockServer2.Close()

		go triggerCheckAPI(mockServer2.URL, outputCh)

		select {
		case result := <-outputCh:
			errMsg := &fiber.Error{Code: 400, Message: "error triggering the API http://127.0.0.1:54594: status code 400 "}
			require.NotEmpty(t, result.Host)
			require.NotEmpty(t, result.Error)
			require.Equal(t, errMsg.Code, result.Error.Code)
		}

	})

	t.Run("Invalid JSON", func(t *testing.T) {

		outputCh := make(chan models.CheckTriggerResponse, 1)

		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(`invalid JSON`))
		}))
		defer mockServer.Close()

		go triggerCheckAPI(mockServer.URL, outputCh)

		select {
		case result := <-outputCh:
			require.NotEmpty(t, result.Error)
			require.Equal(t, result.Error.Code, http.StatusInternalServerError)

		}
	})

	t.Run("Returns OK", func(t *testing.T) {
		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(softwareVersionResp))
		}))
		defer mockServer.Close()

		// create channels to receive the output and error messages
		outputCh := make(chan models.CheckTriggerResponse, 1)

		// call the function being tested
		go triggerCheckAPI(mockServer.URL, outputCh)

		// verify that the function produces the expected result
		select {
		case result := <-outputCh:
			mockURL, err := fiberutils.GetHostFormEndPoint(mockServer.URL)
			require.NoError(t, err)
			require.Equal(t, mockURL, result.Host)
			fmt.Printf("result: %+v\n", result)
		}
	})
}

func TestRunCheck(t *testing.T) {
	t.Run("Returns OK", func(t *testing.T) {
		mockServer1 := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(softwareVersionResp))
		}))
		defer mockServer1.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:     []string{mockServer1.URL},
				AutomateNodeCount:   1,
				PostgresqlNodeIps:   []string{"http://psql-fqdn.com"},
				PostgresqlNodeCount: 1,
			},
		}

		resp := runCheck(config, "/abc/def", "8080")
		fmt.Printf("resp: %+v\n", resp)
		require.NotNil(t, resp)
		require.NotNil(t, resp["http://psql-fqdn.com"])
		require.NotNil(t, resp[mockServer1.URL])
		require.Equal(t, 2, len(resp))
	
	})
	t.Run("Incorrect API", func(t *testing.T) {
		mockServer1 := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(softwareVersionResp))
		}))
		defer mockServer1.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:   []string{"not-a-valid-api"},
				AutomateNodeCount: 1,
			},
		}

		resp := runCheck(config, "/abc/def", "8080")
		fmt.Printf("resp: %+v\n", resp)
		require.NotNil(t, resp)
		require.NotNil(t, resp["not-a-valid-api"])
		require.NotNil(t, resp[mockServer1.URL])
		require.Equal(t, 1, len(resp))
		require.Error(t, resp[mockServer1.URL].Error)
	})

}
