package fiber_utils

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
	"github.com/gofiber/utils"
)

func LogResgisteredRoutes(stack [][]*fiber.Route, logger logger.ILogger) {
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

func CustomErrorHandler(ctx *fiber.Ctx, err error) {
	// Status code defaults to 500
	code := fiber.StatusInternalServerError
	msg := utils.StatusMessage(fiber.StatusInternalServerError)
	// Retrieve the custom status code if it's an fiber.*Error
	if e, ok := err.(*fiber.Error); ok {
		code = e.Code
		msg = e.Message
	}
	ctx.Status(code).JSON(response.BuildFailedResponse(fiber.NewError(code, msg)))
}
