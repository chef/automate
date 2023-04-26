package status

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/fiber"
)

type Status struct {
	Logger logger.ILogger
}

type IStatus interface {
	GetStatus(c *fiber.Ctx)
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

func NewHandler(logger logger.ILogger) IStatus {
	return &Status{
		Logger: logger,
	}
}

func (s *Status) GetStatus(c *fiber.Ctx) {
	services := []StatusServices{}
	c.JSON(&StatusResponse{
		Status:   "ok",
		Services: services,
	})
}
