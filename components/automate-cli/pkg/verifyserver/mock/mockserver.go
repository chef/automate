package mock

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

func Setup(log *logrus.Logger) *fiber.App {
	app := fiber.New()
	handlers.SetupRoutes(app, log)
	return app
}
