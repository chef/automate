package status_handler

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/fiber"
)

type Deps struct {
	Logger logger.ILogger
}

type IStatusHandler interface {
	Status(c *fiber.Ctx)
}

type StatusServices struct {
	ServiceName string `json:"service_name"`
	Status      string `json:"status"`
	Version     string `json:"version"`
}

type StatusResponse struct {
	Status   string           `json:"status"`
	Services []StatusServices `json:"services"`
}

func NewStatusHandler(Logger logger.ILogger) *Deps {
	return &Deps{
		Logger: Logger,
	}
}

func (d *Deps) Status(c *fiber.Ctx) {
	services := []StatusServices{}
	c.Status(fiber.StatusOK).JSON(&StatusResponse{
		Status:   "ok",
		Services: services,
	})
}
