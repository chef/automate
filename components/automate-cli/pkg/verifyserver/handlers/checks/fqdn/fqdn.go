package fqdn

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/fiber"
)

type Fqdn struct {
	Logger logger.ILogger
}

func NewHandler(logger logger.ILogger) *Fqdn {
	return &Fqdn{Logger: logger}
}

func (f *Fqdn) Run(c *fiber.Ctx) {
}
