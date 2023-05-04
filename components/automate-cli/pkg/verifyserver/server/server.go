package server

import (
	"strings"

	"github.com/ansrivas/fiberprometheus"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
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
	Handler *v1.Handler
}

func NewVerifyServer(port string, debug bool) *VerifyServer {
	log := logger.NewLogger(debug)
	fconf := &fiber.Settings{
		ServerHeader: SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).
		AddStatusService(
			statusservice.NewStatusService()).
		AddBatchCheckService(
			batchcheckservice.NewBatchCheckService(trigger.NewCheckTrigger(
				trigger.NewHardwareResourceCountCheck(),
				trigger.NewSshUserAccessCheck(),
				trigger.NewCertificateCheck(),
				trigger.NewExternalOpensearchCheck(),
				trigger.NewExternalPostgresCheck(),
				trigger.NewFirewallCheck(),
				trigger.NewFqdnCheck(),
				trigger.NewNfsBackupConfigCheck(),
				trigger.NewOpensearchS3BucketAccessCheck(),
				trigger.NewS3BackupConfigCheck(),
				trigger.NewSoftwareVersionCheck(),
				trigger.NewSystemResourceCheck(),
				trigger.NewSshUserAccessCheck(),
			)))
	vs := &VerifyServer{
		Port:    port,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.Setup()
	return vs
}

func (vs *VerifyServer) Start() error {
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
	vs.App.Use(middleware.Logger(fiberutils.GetLogConfig(vs.Log)))

	prometheus := fiberprometheus.New(SERVICE)
	prometheus.RegisterAt(vs.App, "/metrics")
	vs.App.Use(prometheus.Middleware)

	vs.SetupRoutes()
}
