package fiber_utils

import (
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func LogResgisteredRoutes(stack [][]*fiber.Route, logger *logrus.Logger) {
	// Log all registered routes
	logger.Info("List of Routes registered:")
	registerdRoutes := map[string]*fiber.Route{}
	for _, rl := range stack {
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
