package fiberutils_test

import (
	"errors"
	"io/ioutil"
	"net/http"
	"testing"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/gofiber/fiber"
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
	app := fiber.New()

	// Register a route that always returns an error
	app.Get("/panic", func(ctx *fiber.Ctx) {
		defer func() {
			if err := recover(); err != nil {
				ctx.Next(errors.New(err.(string)))
			}
		}()
		panic("test panic")
	})

	// Replace the default error handler with CustomErrorHandler
	app.Settings.ErrorHandler = func(ctx *fiber.Ctx, err error) {
		fiberutils.CustomErrorHandler(ctx, err)
	}

	// Make a request to the error route and assert that the response is correct
	req, _ := http.NewRequest(http.MethodGet, "/panic", nil)
	resp, err := app.Test(req)

	assert.NoError(t, err, "app.Test should not return an error")
	assert.Equal(t, http.StatusInternalServerError, resp.StatusCode, "response status should be 500")
	body, err := ioutil.ReadAll(resp.Body)
	assert.NoError(t, err)
	assert.Contains(t, "{\"status\":\"FAILED\",\"result\":null,\"error\":{\"code\":500,\"message\":\"Internal Server Error\"}}", string(body))
}
