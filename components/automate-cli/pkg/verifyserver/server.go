package verifyserver

import (
	"os"
	"strings"

	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	status_handler "github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/gofiber/cors"
	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
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

func NewVerifyServer(Port string, debug bool) IVerifyServer {
	level := logrus.InfoLevel
	if debug {
		level = logrus.DebugLevel
	}
	return &VerifyServer{
		Port: Port,
		Log: &logrus.Logger{
			Out:       os.Stderr,
			Formatter: &logrus.TextFormatter{TimestampFormat: "2006-01-02 15:04:05.000", FullTimestamp: true},
			Hooks:     make(logrus.LevelHooks),
			Level:     level,
		},
	}
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

func (vs *VerifyServer) setup(st *status_handler.Deps, h *checks.Checks) *fiber.App {
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
