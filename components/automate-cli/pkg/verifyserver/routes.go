package verifyserver

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	fiber_utils "github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiber"
	"github.com/gofiber/fiber"
)

func SetupRoutes(app *fiber.App, h *handlers.Groups, logger logger.ILogger) {
	// Status
	app.Get("/status", h.Status.GetStatus)

	// API routes
	// apiGroup := app.Group("/api")

	// API/V1 Routes
	// apiV1Group := apiGroup.Group("/v1")
	// apiV1Group.Get("/check1", h.Check1)

	fiber_utils.LogResgisteredRoutes(app.Stack(), logger)
}
