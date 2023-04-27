package handlers

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks/fqdn"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/fiber"
)

type Handlers struct {
	Status func(c *fiber.Ctx)
	Fqdn   func(c *fiber.Ctx)
}

func NewHandlersList(logger logger.ILogger) *Handlers {
	return &Handlers{
		Status: status.NewHandler(logger).GetStatus,
		Fqdn:   fqdn.NewHandler(logger).Run,
	}
}
