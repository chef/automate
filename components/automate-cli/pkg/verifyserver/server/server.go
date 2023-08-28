package server

import (
	"strings"
	"time"

	"github.com/ansrivas/fiberprometheus/v2"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/fqdnservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/gcpcloudstorageservice"

	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/certificatechecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/externalopensearchchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/externalpostgresqlchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/firewallchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/fqdnchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/hardwareresourcechecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/nfsmountbackupchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/opensearchs3bucketaccesschecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/s3backupchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/softwareversionchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/sshuseraccesschecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/systemresourcechecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger/systemuserchecktrigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/certificatevalidation"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalopensearchservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/firewallservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/hardwareresourcecount"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/mockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/nfsmountservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/opensearchbackupservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/portreachableservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/sshusercheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemresourceservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemuserservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/awsutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/db"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/executil"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
	"github.com/chef/automate/lib/systemresource"
	"github.com/chef/automate/lib/userutils"
	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/cors"
	fiberlogger "github.com/gofiber/fiber/v2/middleware/logger"
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

	fconf := fiber.Config{
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
				sshuseraccesschecktrigger.NewSshUserAccessCheck(l, fileutils.NewFileSystemUtils(), port),
				certificatechecktrigger.NewCertificateCheck(l, port),
				externalopensearchchecktrigger.NewExternalOpensearchCheck(l, port),
				externalpostgresqlchecktrigger.NewExternalPostgresCheck(l, port),
				firewallchecktrigger.NewFirewallCheck(l, port),
				fqdnchecktrigger.NewFqdnCheck(l, port),
				nfsmountbackupchecktrigger.NewNfsBackupConfigCheck(l, port),
				opensearchs3bucketaccesschecktrigger.NewOpensearchS3BucketAccessCheck(l, port),
				s3backupchecktrigger.NewS3BackupConfigCheck(l, port),
				softwareversionchecktrigger.NewSoftwareVersionCheck(l, port),
				systemresourcechecktrigger.NewSystemResourceCheck(l, port),
				systemuserchecktrigger.NewSystemUserCheck(l, port),
			), l, port)).
		AddNFSMountService(nfsmountservice.NewNFSMountService(l, port, httputils.NewClient(l), systemresource.NewSystemResourceInfoImpl())).
		AddHardwareResourceCountService(hardwareresourcecount.NewHardwareResourceCountService(l)).
		AddSoftwareVersionService(softwareversionservice.NewSoftwareVersionService(l, fiberutils.CheckPath)).
		AddSystemResourceService(systemresourceservice.NewSystemResourceService(l, systemresource.NewSystemResourceInfoImpl(), &fileutils.FileSystemUtils{})).
		AddMockServerService(mockserverservice.NewMockServersServiceImp(l)).
		AddS3ConfigService(s3configservice.NewS3ConfigService(l, awsutils.NewAwsUtils())).
		AddOSS3BackupService(opensearchbackupservice.NewOSS3BackupService(l)).
		AddPortReachableService(portreachableservice.NewPortReachableService(l, constants.TIMEOUT)).
		AddExternalPostgresqlService(externalpostgresqlservice.NewExternalPostgresqlService(db.NewDBImpl(), fileutils.NewFileSystemUtils(), l)).
		AddSystemUserService(systemuserservice.NewSystemUserService(l, executil.NewExecCmdServiceImp(), userutils.NewUserUtilImp())).
		AddExternalOpensearchService(externalopensearchservice.NewExternalOpensearchService(l, constants.TIMEOUT)).
		AddFqdnService(fqdnservice.NewFqdnService(l, constants.TIMEOUT)).
		AddFirewallService(firewallservice.NewFirewallService(l, constants.TIMEOUT, port)).
		AddCertificateValidation(certificatevalidation.NewValidateCertificateService(l)).
		AddSshUserCheckService(sshusercheckservice.NewSshUserCheckService(l, fileutils.NewFileSystemUtils(), sshutils.NewSSHUtil(sshutils.NewSshClient(), l))).
		AddGCSConfigService(gcpcloudstorageservice.NewGCPCloudStorageConfig(l))
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
	vs.App.Use(fiberlogger.New(fiberutils.GetLogConfig(vs.Log)))

	// Added this condition to avoid error "duplicate metrics collector registration attempted"
	if isMetricsRequired {
		prometheus := fiberprometheus.New(SERVICE)
		prometheus.RegisterAt(vs.App, "/metrics")
		vs.App.Use(prometheus.Middleware)
	}

	vs.SetupRoutes()
}
