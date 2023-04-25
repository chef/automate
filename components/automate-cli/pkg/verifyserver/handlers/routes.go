package handlers

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	status_handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	fiber_utils "github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiber"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func SetupRoutes(app *fiber.App, st *status_handler.Deps, h *checks.Checks, logger *logrus.Logger) {
	// Status
	app.Get("/status", st.Status)

	// API routes
	// apiGroup := app.Group("/api")

	// API/V1 Routes
	// apiV1Group := apiGroup.Group("/v1")

	fiber_utils.LogResgisteredRoutes(app.Stack(), logger)
}
