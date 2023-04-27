package status

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

type Status struct {
	Logger logger.ILogger
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

func NewHandler(logger logger.ILogger) *Status {
	return &Status{
		Logger: logger,
	}
}

func (s *Status) GetStatus(c *fiber.Ctx) {
	services := []StatusServices{}
	c.JSON(response.BuildSuccessResponse(&StatusResponse{
		Status:   "ok",
		Services: services,
	}))
}
