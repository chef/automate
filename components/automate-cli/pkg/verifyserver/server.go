package verifyserver

import (
	"strings"

	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	fiber_utils "github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiber"
	"github.com/gofiber/cors"
	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
)

const (
	SERVICE      = "verify-server"
	DEFAULT_PORT = "7799"
)

type VerifyServer struct {
	Port string
	Log  logger.ILogger
}

type IVerifyServer interface {
	Start() error
}

func NewVerifyServer(port string, debug bool) IVerifyServer {
	log := logger.NewLogger(debug)
	vs := &VerifyServer{
		Port: port,
		Log:  log,
	}
	return vs
}

func (vs *VerifyServer) Start() error {
	handlersGroup := handlers.NewHandlersGroup(vs.Log)
	app := vs.setup(handlersGroup)
	err := app.Listen(":" + DEFAULT_PORT)
	if err != nil {
		if strings.Contains(err.Error(), "address already in use") {
			vs.Log.Error("Service could not start on port " + DEFAULT_PORT + " as it is already in use.")
		}
		return err
	}
	return nil
}

func (vs *VerifyServer) setup(h *handlers.Groups) *fiber.App {
	fconf := &fiber.Settings{
		ServerHeader: SERVICE,
		ErrorHandler: fiber_utils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	app.Use(cors.New())

	// Define middleware to log all requests
	app.Use(middleware.Logger(fiber_utils.GetLogConfig(vs.Log)))

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(app, "/metrics")
	app.Use(prometheus.Middleware)

	SetupRoutes(app, h, vs.Log)
	return app
}
