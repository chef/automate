package v1_test

import (
	"bytes"
	"errors"
	"io/ioutil"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/startmockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/stopmockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

func SetupMockServices(protocol string) (stopmockserverservice.IStopMockServerService, startmockserverservice.IStartMockServersService) {
	stopmock := &stopmockserverservice.MockStopMockServerService{
		StopMockServerFunc: func(server *models.Server) error {
			if protocol == constants.TCP {
				return errors.New("Error while shutting down the server")
			}
			return nil
		},
	}

	servers := []*models.Server{
		{
			Port:     5431,
			Protocol: constants.UDP,
		},
		{
			Port:     1234,
			Protocol: constants.TCP,
		},
	}
	startmock := &startmockserverservice.MockServersService{
		GetMockServersFunc: func() []*models.Server {
			if protocol == constants.HTTPS {
				return []*models.Server{}
			}
			return servers
		},
		SetMockServersFunc: func(s []*models.Server) {
			servers = s
		},
	}
	return stopmock, startmock
}

func SetupStopMockServerHandlers(sm stopmockserverservice.IStopMockServerService, ss startmockserverservice.IStartMockServersService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}
	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddMockServerServices(ss).
		AddStopMockServerService(sm)
	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
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
			expectedBody: "{\"status\":\"SUCCESS\",\"result\":\"Server stop successfully\"}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 5431,
				"protocol": "udp"
			  }`,
		},
		{
			description:  "400:bad request",
			expectedCode: 400,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Stop mock server request body parsing failed: invalid character '\\\\n' in string literal\"}}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 5431,
				"p
			  }`,
		},
		{
			description:  "400:bad request port number out of range",
			expectedCode: 400,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":400,\"message\":\"Port number 65539 not within range 0-65535.\"}}",
			protocol:     constants.UDP,
			reqBody: `{
				"port": 65539,
				"protocol": "udp"
			  }`,
		},
		{
			description:  "200:No mock server running",
			expectedCode: 200,
			expectedBody: "{\"status\":\"SUCCESS\",\"result\":\"No Mock Server running\"}",
			protocol:     constants.HTTPS,
			reqBody: `{
				"port": 443,
				"protocol": "https"
			  }`,
		},
		{
			description:  "500:request proctocol does not match with running server proctocol",
			expectedCode: 500,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Request protocol tcp does not match with running server protocol udp\"}}",
			protocol:     constants.TCP,
			reqBody: `{
				"port": 5431,
				"protocol": "tcp"
			  }`,
		},
		{
			description:  "500:error while shutting server down",
			expectedCode: 500,
			expectedBody: "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Error while stoppping server: Error while shutting down the server\"}}",
			protocol:     constants.TCP,
			reqBody: `{
				"port": 1234,
				"protocol": "tcp"
			  }`,
		},
		{
			description:  "200:No mock server running on 5432",
			expectedCode: 200,
			expectedBody: "{\"status\":\"SUCCESS\",\"result\":\"No Mock server is running on port 5432\"}",
			protocol:     constants.TCP,
			reqBody: `{
				"port": 5432,
				"protocol": "tcp"
			  }`,
		},
	}
	statusEndpoint := "/api/v1/stop/mock-server"

	for _, test := range tests {

		// Setup the app as it is done in the main function
		app, err := SetupStopMockServerHandlers(SetupMockServices(test.protocol))
		assert.NoError(t, err)

		reqBody := bytes.NewBufferString(test.reqBody)
		req := httptest.NewRequest("POST", statusEndpoint, reqBody)
		req.Header.Add("Content-Type", "application/json")
		res, err := app.Test(req, -1)
		assert.NoError(t, err)
		body, err := ioutil.ReadAll(res.Body)
		assert.NoError(t, err, test.description)
		assert.Contains(t, string(body), test.expectedBody)
		assert.Equal(t, test.expectedCode, res.StatusCode)

	}
}
