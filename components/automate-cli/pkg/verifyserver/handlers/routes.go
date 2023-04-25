package handlers

import (
	statusHandler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func SetupRoutes(app *fiber.App, logger *logrus.Logger) {
	app.Get("/status", statusHandler.NewStatusHandler(logger).Status)

	// Log all registered routes
	logger.Info("List of Routes registered:")
	registerdRoutes := map[string]*fiber.Route{}
	for _, rl := range app.Stack() {
		for _, r := range rl {
			key := r.Method + " " + r.Path
			if _, ok := registerdRoutes[key]; !ok {
				registerdRoutes[key] = r
			}
		}
	}

	for r := range registerdRoutes {
		logger.Info(r)
	}
}
