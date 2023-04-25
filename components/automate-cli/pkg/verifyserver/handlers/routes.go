package handlers

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	status_handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	fiber_utils "github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiber"
	"github.com/gofiber/fiber"
)

func SetupRoutes(app *fiber.App, st status_handler.IStatusHandler, h checks.IChecks, logger logger.ILogger) {
	// Status
	app.Get("/status", st.Status)

	// API routes
	// apiGroup := app.Group("/api")

	// API/V1 Routes
	// apiV1Group := apiGroup.Group("/v1")

	fiber_utils.LogResgisteredRoutes(app.Stack(), logger)
}
