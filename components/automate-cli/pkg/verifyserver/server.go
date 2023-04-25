package verifyserver

import (
	"strings"

	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	status_handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/cors"
	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
)

const (
	SERVICE = "verify-server"
	PORT    = "7799"
)

type VerifyServer struct {
	Port string
	Log  logger.ILogger
}

type IVerifyServer interface {
	Start() error
}

func NewVerifyServer(Port string, debug bool) IVerifyServer {
	log := logger.NewLogger(debug)
	vs := &VerifyServer{
		Port: Port,
		Log:  log,
	}
	return vs
}

func (vs *VerifyServer) Start() error {
	st := status_handler.NewStatusHandler(vs.Log)
	h := checks.NewHandler(vs.Log)
	app := vs.setup(st, h)
	err := app.Listen(":" + PORT)
	if err != nil {
		if strings.Contains(err.Error(), "address already in use") {
			vs.Log.Error("Service could not start on port " + PORT + " as it is already in use.")
		}
		return err
	}
	return nil
}

func (vs *VerifyServer) setup(st status_handler.IStatusHandler, h checks.IChecks) *fiber.App {
	app := fiber.New()
	app.Use(cors.New())

	// Define middleware to log all requests
	app.Use(middleware.Logger(middleware.LoggerConfig{
		TimeFormat: "2006-01-02 15:04:05.000",
	}))

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(app, "/metrics")
	app.Use(prometheus.Middleware)

	handlers.SetupRoutes(app, st, h, vs.Log)
	return app
}
