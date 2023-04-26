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

	// apiGroup := app.Group("/api")
	// apiV1Group := apiGroup.Group("/v1")
	// apiChecksGroup := apiV1Group.Group("/checks")
	// apiChecksGroup.Get("/check1", h.Checks.Check1)

	fiber_utils.LogResgisteredRoutes(app.Stack(), logger)
}
