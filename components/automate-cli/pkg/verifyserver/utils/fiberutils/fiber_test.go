package fiberutils_test

import (
	"errors"
	"io"
	"net/http"
	"testing"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/gofiber/fiber/v2"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
)

func TestLogResgisteredRoutes(t *testing.T) {
	// Create a buffer to capture log output
	cw := majorupgrade_utils.NewCustomWriter()
	logger, err := logger.NewLoggerWithOut("text", "info", cw.CliWriter)
	assert.NoError(t, err)

	// Create a mock stack of registered routes
	stack := [][]*fiber.Route{
		{
			{Method: "GET", Path: "/foo"},
			{Method: "POST", Path: "/foo"},
			{Method: "GET", Path: "/bar"},
		},
		{
			{Method: "POST", Path: "/baz"},
			{Method: "PUT", Path: "/baz"},
		},
	}

	// Call the function under test
	fiberutils.LogResgisteredRoutes(stack, logger)

	// Check the log output
	assert.Contains(t, cw.Output(), "GET /foo")
	assert.Contains(t, cw.Output(), "POST /foo", cw.Output())
	assert.Contains(t, cw.Output(), "GET /bar", cw.Output())
	assert.Contains(t, cw.Output(), "POST /baz", cw.Output())
	assert.Contains(t, cw.Output(), "PUT /baz", cw.Output())
}

func TestCustomErrorHandler(t *testing.T) {
	// Replace the default error handler with CustomErrorHandler
	app := fiber.New(fiber.Config{
		ErrorHandler: fiberutils.CustomErrorHandler,
	})

	// Register a route that always returns an error
	app.Get("/panic", func(ctx *fiber.Ctx) error {
		defer func() error {
			if err := recover(); err != nil {
				return errors.New(err.(string))
			}
			return nil
		}()
		panic("test panic")
	})

	// Make a request to the error route and assert that the response is correct
	req, _ := http.NewRequest(http.MethodGet, "/panic", nil)
	resp, err := app.Test(req)

	assert.NoError(t, err, "app.Test should not return an error")
	assert.Equal(t, http.StatusInternalServerError, resp.StatusCode, "response status should be 500")
	body, err := io.ReadAll(resp.Body)
	assert.NoError(t, err)
	assert.Contains(t, "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Internal Server Error\"}}", string(body))
}
