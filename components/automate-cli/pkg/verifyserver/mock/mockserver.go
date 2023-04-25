package mock

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	status_handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func SetupWithDefaultHandlers(log *logrus.Logger) *fiber.App {
	app := fiber.New()
	st := status_handler.NewStatusHandler(log)
	h := checks.NewHandler(log)
	handlers.SetupRoutes(app, st, h, log)
	return app
}
