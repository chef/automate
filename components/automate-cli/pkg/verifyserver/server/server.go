package server

import (
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/hardwareresourcechecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/softwareversionchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/systemresourcechecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/systemuserchecktrigger"

	"github.com/ansrivas/fiberprometheus"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/cors"
	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
	"github.com/sirupsen/logrus"
)

const (
	SERVICE      = "verify-server"
	DEFAULT_PORT = "7799"
	INFO_LEVEL   = "info"
	DEBUG_LEVEL  = "debug"
)

type VerifyServer struct {
	Port    string
	Log     logger.Logger
	App     *fiber.App
	Handler *v1.Handler
}

func NewVerifyServer(port string, debug bool) (*VerifyServer, error) {
	defaultLevel := INFO_LEVEL
	if debug {
		defaultLevel = DEBUG_LEVEL
	}
	l, err := logger.NewLogger("text", defaultLevel)
	if err != nil {
		return nil, err
	}

	l.NewEntry().Logger.SetFormatter(&logrus.TextFormatter{
		TimestampFormat: time.RFC3339,
		FullTimestamp:   true,
	})

	l.Info("Using TimeZone: " + fiberutils.CfgLogTimeZone())

	fconf := &fiber.Settings{
		ServerHeader: SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(l).
		AddStatusService(
			statusservice.NewStatusService(fiberutils.ExecuteShellCommand, l)).
		AddBatchCheckService(
			batchcheckservice.NewBatchCheckService(trigger.NewCheckTrigger(
				hardwareresourcechecktrigger.NewHardwareResourceCountCheck(l, port),
				trigger.NewSshUserAccessCheck(),
				trigger.NewCertificateCheck(),
				trigger.NewExternalOpensearchCheck(),
				trigger.NewExternalPostgresCheck(),
				trigger.NewFirewallCheck(),
				trigger.NewFqdnCheck(),
				trigger.NewNfsBackupConfigCheck(),
				trigger.NewOpensearchS3BucketAccessCheck(),
				trigger.NewS3BackupConfigCheck(),
				softwareversionchecktrigger.NewSoftwareVersionCheck(l, port),
				systemresourcechecktrigger.NewSystemResourceCheck(l, port),
				systemuserchecktrigger.NewSystemUserCheck(l, port),
			)))
	vs := &VerifyServer{
		Port:    port,
		Log:     l,
		App:     app,
		Handler: handler,
	}
	vs.Setup(true)
	return vs, nil
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

func (vs *VerifyServer) Setup(isMetricsRequired bool) {
	vs.App.Use(cors.New())

	// Define middleware to log all requests
	vs.App.Use(middleware.Logger(fiberutils.GetLogConfig(vs.Log)))

	// Added this condition to avoid error "duplicate metrics collector registration attempted"
	if isMetricsRequired {
		prometheus := fiberprometheus.New(SERVICE)
		prometheus.RegisterAt(vs.App, "/metrics")
		vs.App.Use(prometheus.Middleware)
	}

	vs.SetupRoutes()
}
