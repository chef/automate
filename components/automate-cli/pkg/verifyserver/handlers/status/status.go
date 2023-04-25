package status_handler

import (
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

type Deps struct {
	Logger *logrus.Logger
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

func NewStatusHandler(Logger *logrus.Logger) *Deps {
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
