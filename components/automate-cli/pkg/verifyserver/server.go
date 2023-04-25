package verifyserver

import (
	"os"

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

func Start() {
	var log = &logrus.Logger{
		Out:       os.Stdout,
		Formatter: &logrus.TextFormatter{TimestampFormat: "2006-01-02 15:04:05.000", FullTimestamp: true},
		Hooks:     make(logrus.LevelHooks),
		Level:     logrus.DebugLevel,
	}
	app := Setup(log)
	err := app.Listen(":" + PORT)
	if err != nil {
		log.WithError(err).Error("Service could not start on port " + PORT + " as it is already in use.")
	}
}

func Setup(log *logrus.Logger) *fiber.App {

	app := fiber.New()
	app.Use(cors.New())

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(app, "/metrics")
	app.Use(prometheus.Middleware)

	handlers.SetupRoutes(app, log)
	return app
}
