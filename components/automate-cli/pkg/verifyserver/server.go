package verifyserver

import (
	"strings"

	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
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
	Port    string
	Log     logger.ILogger
	App     *fiber.App
	Handler *handler.Handler
}

func StartVerifyServer(port string, debug bool) {
	log := logger.NewLogger(debug)
	vs := NewVerifyServer(port, debug, log)
	vs.Start()
}

func NewVerifyServer(port string, debug bool, log logger.ILogger) *VerifyServer {
	fconf := &fiber.Settings{
		ServerHeader: SERVICE,
		ErrorHandler: fiber_utils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := handler.NewHandler(log)
	vs := &VerifyServer{
		Port:    port,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	return vs
}

func (vs *VerifyServer) Start() error {
	vs.Setup()
	err := vs.App.Listen(":" + vs.Port)
	if err != nil {
		if strings.Contains(err.Error(), "address already in use") {
			vs.Log.Error("Service could not start on port " + DEFAULT_PORT + " as it is already in use.")
		}
		return err
	}
	return nil
}

func (vs *VerifyServer) Setup() {
	vs.App.Use(cors.New())

	// Define middleware to log all requests
	vs.App.Use(middleware.Logger(fiber_utils.GetLogConfig(vs.Log)))

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(vs.App, "/metrics")
	vs.App.Use(prometheus.Middleware)

	vs.SetupRoutes()
}
