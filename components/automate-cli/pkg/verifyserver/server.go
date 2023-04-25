package verifyserver

import (
	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/gofiber/cors"
	"github.com/gofiber/fiber"
	"github.com/sirupsen/logrus"
)

const (
	SERVICE = "verify-server"
	PORT    = "7799"
)

type VerifyServer struct {
	Port string
	Log  *logrus.Logger
}

type IVerifyServer interface {
	Start() error
}

func NewVerifyServer(Port string, Log *logrus.Logger) IVerifyServer {
	return &VerifyServer{
		Port: Port,
		Log:  Log,
	}
}

func (vs *VerifyServer) Start() error {
	app := vs.setup()
	err := app.Listen(":" + PORT)
	if err != nil {
		vs.Log.Error("Service could not start on port " + PORT + " as it is already in use.")
		return err
	}
	return nil
}

func (vs *VerifyServer) setup() *fiber.App {
	app := fiber.New()
	app.Use(cors.New())

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(app, "/metrics")
	app.Use(prometheus.Middleware)

	handlers.SetupRoutes(app, vs.Log)
	return app
}
