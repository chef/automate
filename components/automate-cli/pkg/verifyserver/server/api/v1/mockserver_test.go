package v1_test

import (
	"bytes"
	"errors"
	"io"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/mockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"
)

func SetupMockServerService(protocol string) *mockserverservice.MockServersServiceMock {
	return &mockserverservice.MockServersServiceMock{
		StartFunc: func(cfg *models.StartMockServerRequestBody) error {
			if protocol == constants.UDP || protocol == constants.HTTPS {
				return nil
			}
			if protocol == constants.TCP || protocol == constants.HTTP {
				return errors.New("port unavailable")
			}
			return errors.New("unsupported protocol")
		},
		StopFunc: func(cfg *models.StopMockServerRequestBody) error {
			if protocol == constants.TCP {
				return errors.New("Error while shutting down the server")
			}
			if protocol == constants.HTTPS {
				return errors.New("no mock server found on port")
			}
			return nil
		},
	}
}

func SetupMockServerHandlers(ss *mockserverservice.MockServersServiceMock) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}
	fconf := fiber.Config{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddMockServerService(ss)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}

func TestStartMockServer(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
		reqBody      string
		protocol     string
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			protocol:     "udp",
			reqBody: `{
				"port": 3001,
				"protocol": "udp",
				"key": "",
				"cert": ""
		}`,
		},
		{
			description:  "400:invalid request body",
			expectedCode: 400,
			protocol:     "udp",
			reqBody: `{
				"port": true,
				"proto": "udp",
				"key": "",
				"cert": ""
		}`,
		},
		{
			description:  "409:port already in use",
			expectedCode: 409,
			protocol:     "tcp",
			reqBody: `{
				"port": 3000,
				"protocol": "tcp",
				"key": "",
				"cert": ""
		}`,
		},
		{
			description:  "400:port invalid",
			expectedCode: 400,
			protocol:     "http",
			reqBody: `{
				"port": 65536,
				"protocol": "http",
				"key": "",
				"cert": ""
		}`,
		},
		{
			description:  "400:port invalid",
			expectedCode: 400,
			protocol:     "udp",
			reqBody: `{
				"port": -1,
				"key": "",
				"cert": ""
		}`,
		},
		{
			description:  "500:internal server error",
			expectedCode: 500,
			protocol:     "dp",
			reqBody: `{
				"port": 4200,
				"protocol": "dp",
				"key": "",
				"cert": ""
		}`,
		},
	}
	statusEndpoint := "/api/v1/start/mock-server"

	for _, test := range tests {
		app, err := SetupMockServerHandlers(SetupMockServerService(test.protocol))
		assert.NoError(t, err)
		reqBody := bytes.NewBufferString(test.reqBody)
		req := httptest.NewRequest("POST", statusEndpoint, reqBody)
		req.Header.Add("Content-Type", "application/json")
		res, err := app.Test(req, -1)
		assert.NoError(t, err)
		body, err := io.ReadAll(res.Body)
		assert.NoError(t, err, test.description)
		assert.Contains(t, string(body), test.expectedBody)
		assert.Equal(t, res.StatusCode, test.expectedCode)
	}
}

func TestStopMockServerAPI(t *testing.T) {
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
		reqBody      string
		protocol     string
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			expectedBody: "{\"status\":\"SUCCESS\",\"result\":\"Server stopped successfully\"}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 5431,
				"protocol": "udp"
			  }`,
		},
		{
			description:  "400:bad request",
			expectedCode: 400,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Invalid request body\"}}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 5431,
				"p
			  }`,
		},
		{
			description:  "400:bad request port number out of range",
			expectedCode: 400,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Invalid port number\"}}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 65539,
				"protocol": "udp"
			  }`,
		},
		{
			description:  "404:No mock server running",
			expectedCode: 404,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":404,\"message\":\"No Mock server is running on port 443 with protocol https\"}}",
			protocol:     constants.HTTPS,
			reqBody: `{
				"port": 443,
				"protocol": "https"
			  }`,
		},
		{
			description:  "500:error while shutting server down",
			expectedCode: 500,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Error while shutting down the server\"}}",
			protocol:     constants.TCP,
			reqBody: `{
				"port": 1234,
				"protocol": "tcp"
			  }`,
		},
	}
	statusEndpoint := "/api/v1/stop/mock-server"

	for _, test := range tests {

		// Setup the app as it is done in the main function
		app, err := SetupMockServerHandlers(SetupMockServerService(test.protocol))
		assert.NoError(t, err)

		reqBody := bytes.NewBufferString(test.reqBody)
		req := httptest.NewRequest("POST", statusEndpoint, reqBody)
		req.Header.Add("Content-Type", "application/json")
		res, err := app.Test(req, -1)
		assert.NoError(t, err)
		body, err := io.ReadAll(res.Body)
		assert.NoError(t, err, test.description)
		assert.Contains(t, string(body), test.expectedBody)
		assert.Equal(t, test.expectedCode, res.StatusCode)

	}
}
