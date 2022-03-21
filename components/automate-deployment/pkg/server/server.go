package server

import (
	"bytes"
	"context"
	"crypto/sha1"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"path"
	"syscall"
	"time"

	"github.com/boltdb/bolt"
	"github.com/golang/protobuf/ptypes"
	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	dc "github.com/chef/automate/api/config/deployment"
	papi "github.com/chef/automate/api/config/platform"
	platformconf "github.com/chef/automate/api/config/platform"
	config "github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/components/automate-deployment/pkg/usermgmt"
	usermgmt_client "github.com/chef/automate/components/automate-deployment/pkg/usermgmt/client"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/io/chunks"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/secrets"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

type server struct {
	deployment           *deployment.Deployment         // set in NewDeployment, read for access to target and channel config
	deploymentStore      persistence.DeploymentStore    // A place to load the deployment from
	serverConfig         *Config                        // set in StartServer, read for access in NewDeployment
	convergeLoop         *Looper                        // set in StartServer,
	lcClient             lc.LicenseControlServiceClient // set when cachedLCClient is called
	umClient             usermgmt.UserMgmt
	converger            converge.Converger
	senderStore          eventSenderStore
	ensureStatusTimeout  time.Duration
	ensureStatusInterval time.Duration
	backupRunner         *backup.Runner

	releaseManifestProvider manifest.CachingReleaseManifestProvider

	connFactory *secureconn.Factory
	secretStore secrets.SecretStore
}

// DataDir is the path where data is stored. It's only variable for testing
// purposes.
var DataDir = "/hab/svc/deployment-service/data/"

const (
	// DBName is the basename of the boltdb database file
	DBName                = "bolt.db"
	deploymentServiceName = "deployment-service"
	automateCLIName       = "automate-cli"
)

// convergeErrorHandler is a function that takes an error that occurs during
// the converge and does any required error handling before the goroutine
// terminates.
type convergeErrorHandler func(error)

// convergeOperation is a function that occurs before a converge request
// that is done while under lock.
type convergeOperation func(*server) error

const (
	// The default amount of time between invocations of the
	// deployment converge.
	defaultConvergeInterval = 30 * time.Second
	// The default amount of time to wait for services to become
	// ready during a Deploy request.
	defaultEnsureStatusTimeout = 600 * time.Second
	// The amount of time to wait for the shutdown request to complete
	defaultWaitForDownTimeout = 200 * time.Second
	// The default amount of time to wait between calls to the
	// status endpoints during a Deploy request.
	defaultEnsureStatusInterval = 2 * time.Second
	// The default size of "chunks" for gRPC calls that return
	// large streams of data.
	defaultChunkSize = 2 << 17 // 256k

	// The environmental variable to track the upgrade is major or not
	isUpgradeMajorEnv = "IS_MAJOR_UPGRADE"
)

var (
	// ErrorNoDeployment is returned when a command that requires
	// a configured deployment is called when deployment is
	// nil. We shouldn't ever return this anymore as we now always
	// create a deployment.
	ErrorNoDeployment = status.Error(codes.FailedPrecondition, "No deployment has been created yet.")
	// ErrorNotConfigured is returned when a command that requires
	// a configured deployment is called before
	// ConfigureDeployment has been called.
	ErrorNotConfigured = status.Error(codes.FailedPrecondition, "The deployment has not been configured.")
	// ErrorNoSuchService is returned when a command that takes a
	// service has been given a service identifier that is not recognized.
	ErrorNoSuchService = status.Error(codes.NotFound, "No service with the given name was found.")
	// ErrorNoSuchTask is returned when a command that takes a
	// TaskID has been given an ID that doesn't exist.
	ErrorNoSuchTask = status.Error(codes.NotFound, "No task with the given TaskID was found.")
	// ErrorServiceNameInvalid is returned when a command that
	// takes a service has been given a service identifier that
	// could not be parsed.
	ErrorServiceNameInvalid = status.Error(codes.InvalidArgument, "Service name had invalid format.")
	// ErrorNoReleaseManifest is returned when a command requires
	// a release manifest but we do not yet have one.
	ErrorNoReleaseManifest = status.Error(codes.FailedPrecondition, "No release manifest has been fetched for this deployment.")
	// ErrorNoBackupDirectory is returned by backup commands when
	// a backup directory is not configured but one is needed by
	// the command.
	ErrorNoBackupDirectory = status.Error(codes.FailedPrecondition, "No backup directory has been configured for this Chef Automate deployment.")
)

func durationFromSecs(configValue uint32, defaultDuration time.Duration) time.Duration {
	if configValue == 0 {
		return defaultDuration
	}

	return time.Duration(configValue) * time.Second
}

// This helper struct makes error handling for the sequence of steps
// of a deployment easier. See: https://blog.golang.org/errors-are-values
type errDeployer struct {
	*server
	err    error
	sender events.EventSender
}

// newEventSender returns an event sender
func (s *server) newEventSender() events.EventSender {
	return events.NewMemoryEventSender(s.deployment.ID)
}

func (s *server) HasConfiguredDeployment() bool {
	return s.deployment != nil && s.deployment.Config != nil
}

func (s *server) Deploy(ctx context.Context,
	request *api.DeployRequest) (*api.DeployResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	sender := s.newEventSender()
	task, err := s.doDeploySome(s.deployment.ServiceNames(), sender, true, request.UsedBootstrapBundle)
	if err != nil {
		return nil, err
	}
	return &api.DeployResponse{TaskId: task.ID.String()}, nil
}

// Preload configures services and installs their packages but does not start them.
// Used during upgrades only
func (s *server) Preload(ctx context.Context,
	request *api.DeployRequest) (*api.DeployResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	sender := s.newEventSender()
	task, err := s.doPreload(s.deployment.ServiceNames(), sender)
	if err != nil {
		return nil, err
	}
	return &api.DeployResponse{TaskId: task.ID.String()}, nil
}

// StartNonDataServices starts everything except the data services
// Used in A1 upgrades only.
func (s *server) StartNonDataServices(ctx context.Context,
	request *api.DeployRequest) (*api.DeployResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	sender := s.newEventSender()

	svcsToStart := make([]string, 0, len(s.deployment.ExpectedServices)-2)
	for _, v := range s.deployment.ExpectedServices {
		if !services.IsDataService(v.Name()) {
			svcsToStart = append(svcsToStart, v.Name())
		}
	}

	task, err := s.doDeploySome(svcsToStart, sender, true, request.UsedBootstrapBundle)
	if err != nil {
		return nil, err
	}
	return &api.DeployResponse{TaskId: task.ID.String()}, nil
}

func (s *server) DeployDataServices(ctx context.Context,
	request *api.DeployRequest) (*api.DeployResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	sender := s.newEventSender()

	dataServices := make([]string, 0, len(s.deployment.ExpectedServices)-2)
	for _, v := range s.deployment.ExpectedServices {
		if services.IsDataService(v.Name()) {
			dataServices = append(dataServices, v.Name())
		}
	}

	task, err := s.doDeploySome(dataServices, sender, false, false)
	if err != nil {
		return nil, err
	}
	return &api.DeployResponse{TaskId: task.ID.String()}, nil
}

// We return a pointer to a DeployedService do we can never accidentally
// apply an empty desired state because we forgot to check the error
func (s *server) buildDesiredState() (*converge.DesiredState, error) {
	if s.deployment.CurrentReleaseManifest == nil {
		return nil, errors.New("Cannot converge deployment without a release manifest")
	}

	// We need to make sure that deployment service is also expected so that it can be
	// kept up-to-date.
	expectedServices := make([]*deployment.Service, len(s.deployment.ExpectedServices)+2)
	dsPkg := habpkg.New("chef", deploymentServiceName)
	expectedServices[0] = &deployment.Service{
		Installable:     &dsPkg,
		DeploymentState: deployment.Running,
	}
	// TODO(jaym): We need to keep automate-cli up-to-date, however the naming on the
	// converger is misleading. automate-cli is not a service, but we do want it installed
	cliPkg := habpkg.New("chef", automateCLIName)
	expectedServices[1] = &deployment.Service{
		Installable:     &cliPkg,
		DeploymentState: deployment.Installed,
	}

	var orderedRemainingServices []*deployment.Service

	wantReverse := s.deployment.Config.GetDeployment().GetV1().GetSvc().GetEnableDeploymentOrderStressMode().GetValue()

	if wantReverse {
		orderedRemainingServices = make([]*deployment.Service, len(s.deployment.ExpectedServices))
		lastIdx := len(s.deployment.ExpectedServices) - 1
		for i, svc := range s.deployment.ExpectedServices {
			orderedRemainingServices[lastIdx-i] = svc
		}
	} else {
		orderedRemainingServices = s.deployment.ExpectedServices
	}

	// Copy over the rest of the expected services
	copy(expectedServices[2:], orderedRemainingServices)

	topology := make(converge.Topology)
	desiredServices := make([]converge.Service, len(expectedServices))
	topology[s.target()] = desiredServices

	m := s.deployment.CurrentReleaseManifest
	if m == nil {
		return nil, errors.New("Manifest not available")
	}

	configRenderer, err := s.configRenderer()
	if err != nil {
		return nil, err
	}

	enableExternalPg := s.deployment.Config.GetGlobal().GetV1().GetExternal().GetPostgresql().GetEnable().GetValue()
	logrus.Debugln("Is External PG enabled : ", enableExternalPg)

	enableExternalES := s.deployment.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
	logrus.Debugln("Is External ES enabled : ", enableExternalES)

	for i, service := range expectedServices {
		var convergeState converge.ServiceConvergeState
		pkg := manifest.InstallableFromManifest(m, service.Name())
		if pkg == nil {
			logrus.WithFields(logrus.Fields{
				"origin": service.Origin(),
				"name":   service.Name(),
			}).Warn("Service not found in manifest")
			// We will not continue here because information in the binary and
			// that in the manifest is inconsistent
			return nil, errors.Errorf("Service %q not found in manifest %+v", service.Name(), m)
		}

		if hart, ok := pkg.(*habpkg.Hart); ok {
			logrus.WithFields(logrus.Fields{
				"origin": service.Origin(),
				"name":   service.Name(),
				"hart":   hart.Path(),
			}).Debug("Found hart override")
		}

		if service.Name() == constants.AutomatePGService {
			if enableExternalPg {
				service.DeploymentState = deployment.Removed
			} else if service.DeploymentState == deployment.Removed {
				service.DeploymentState = deployment.Running
			}
		}

		if service.Name() == constants.AutomateSearchService {
			if enableExternalES {
				service.DeploymentState = deployment.Removed
			} else if service.DeploymentState == deployment.Removed {
				service.DeploymentState = deployment.Running
			}
		}

		// logrus.Debugln("BUILD_DESIRED_STATE SERVICE_NAME ::: ", service.Name())
		// logrus.Debugln("BUILD_DESIRED_STATE SERVICE_STATE ::: ", service.DeploymentState)

		switch service.DeploymentState {
		case deployment.Skip:
			convergeState = converge.Skip()
		case deployment.Installed:
			convergeState = converge.Installed(pkg)
		case deployment.Running:
			userToml, err := configRenderer(service)
			if err != nil {
				return nil, err
			}

			// This code is responsible for determining a service's binds and bind modes
			// from `services.AllBinds`, which is populated from assets.BindData
			//
			// This design is based on the following reasoning:
			//
			// * the set of "all binds known to a service" belongs to that service's
			//   package via plan.sh
			// * we've adopted a simplifying invariant that we always bind all binds a
			//   service knows about
			// * the bind mode belongs to the package, for a2 at least (via custom variable
			//   in plan.sh)
			// * given the above, changes to binds/bind mode imply a new package must be
			//   built/published
			// * we are okay with representing bind changes as package upgrades in our code's
			//   model of the universe
			bindInfo, err := services.AllBinds.DefaultsForService(service.Name())
			if err != nil {
				return nil, err
			}

			convergeState = converge.Running(
				pkg,
				converge.BindMode(bindInfo.Mode),
				converge.Binds(bindInfo.Specs),
				converge.UserTOML(userToml),
			)
		case deployment.Removed:
			// Things we don't know about are removed by default
		}

		desiredServices[i] = converge.Service{
			Name:          service.Name(),
			ConvergeState: convergeState,
		}
	}

	var desiredSupState converge.SupervisorState
	if os.Getenv("CHEF_AUTOMATE_SKIP_SYSTEMD") == "true" {
		logrus.Info("Not generating desired supervisor state since CHEF_AUTOMATE_SKIP_SYSTEMD is set")
		desiredSupState = converge.NewSkipSupervisorState()
	} else {
		found, habP := m.PackageForServiceName("hab")
		if !found {
			return nil, errors.New("could not find hab in release manifest")
		}

		found, habSupP := m.PackageForServiceName("hab-sup")
		if !found {
			return nil, errors.New("could not find hab-sup in release manifest")
		}

		found, habLauncherP := m.PackageForServiceName("hab-launcher")
		if !found {
			return nil, errors.New("could not find hab-launcher in release manifest")
		}

		desiredSupState = converge.NewSupervisorState(habSupP, habP, habLauncherP,
			s.deployment.Config.GetDeployment().SystemdProxyConfig())
	}

	desiredState := converge.NewDesiredState(topology, desiredSupState, m.ListPackages(), s.getPackageCleanupMode())
	return &desiredState, nil
}

type ConfigRenderer func(service *deployment.Service) (string, error)

func (s *server) configRenderer() (ConfigRenderer, error) {
	platformConfig := struct {
		Platform *platformconf.Config_Platform `toml:"_a2_platform"`
	}{
		Platform: &platformconf.Config_Platform{},
	}
	platformConfig.Platform.ExternalPostgresql = s.deployment.Config.GetGlobal().GetV1().GetExternal().GetPostgresql()
	platformConfigToml, err := toml.Marshal(platformConfig)
	if err != nil {
		logrus.WithError(err).Error("Could not render platform config")
		return nil, errors.Wrap(err, "Could not render platform config")
	}

	productConfig := &config.ProductConfig{
		Products: deployment.CollectionsForConfig(s.deployment.Config.GetDeployment()),
	}
	return func(service *deployment.Service) (string, error) {
		rootCert := s.deployment.CA().RootCert()
		creds := &config.TLSCredentials{
			RootCertContents: rootCert,
			KeyContents:      service.SSLKey,
			CertContents:     service.SSLCert,
		}

		preparableCfg, found := s.deployment.Config.PlatformServiceConfigForService(service.Name())
		if !found {
			logrus.WithField("service", service.Name()).WithError(err).Warnf("unable to render configuration for unknown service %q", service.Name())
			return "", nil
		}

		if cfg_v2, ok := preparableCfg.(config.PlatformServiceConfigurableV2); ok {
			cfg_v2.ConfigureProduct(productConfig)
		}

		preparedCfg, err := preparableCfg.PrepareSystemConfig(creds)
		if err != nil {
			logrus.WithField("service", service.Name()).WithError(err).Error("unable to prepare configuration for rendering")
			return "", err
		}

		bytes, err := toml.Marshal(preparedCfg)
		if err != nil {
			return "", errors.Wrapf(err, "could not converge %s configuration to TOML", service.Name())
		}

		if usesPlatformScaffolding(service) {
			return fmt.Sprintf("%s\n%s", string(bytes), platformConfigToml), nil
		} else {
			return string(bytes), nil
		}
	}, nil
}

func usesPlatformScaffolding(service *deployment.Service) bool {
	metadata := services.MetadataForPackage(service.Name())
	return metadata != nil && metadata.UsesPlatformScaffolding
}

func (s *errDeployer) convergeServices(task *converge.Task, eventSink converge.EventSink) {
	s.startConverge(task, eventSink)
	s.waitForConverge(task)
}

func (s *errDeployer) startConverge(task *converge.Task, eventSink converge.EventSink) {
	if s.err != nil {
		return
	}

	desiredState, err := s.buildDesiredState()
	if err != nil {
		s.err = err
		return
	}

	err = s.converger.Converge(task, *desiredState, eventSink)

	if err != nil {
		s.err = err
		return
	}

	err = s.reloadBackupRunner()
	if err != nil {
		s.err = err
		return
	}
}

func (s *errDeployer) waitForConverge(task *converge.Task) {
	if s.err != nil {
		return
	}
	s.err = <-task.C
}

func (s *errDeployer) ensureCerts() {
	if s.err != nil {
		return
	}
	s.err = s.deployment.EnsureCerts()
}

func (s *server) doDeploySome(serviceNames []string,
	sender events.EventSender,
	isDeployed bool, usedBootstrapBundle bool) (*converge.Task, error) {

	s.deployment.Lock()
	defer s.deployment.Unlock()

	task, err := converge.NewTask()
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to generate converge task for deploy: %s", err.Error())
	}

	s.senderStore.Set(task.ID.String(), sender)
	eDeploy := &errDeployer{server: s, err: nil, sender: sender}

	s.deployment.Deployed = isDeployed
	err = s.deployment.SetRunningExpectations(serviceNames)
	if err != nil {
		return nil, ErrorNoSuchService
	}

	eDeploy.ensureCerts()
	err = s.persistDeployment()
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to persist deployment database: %s", err.Error())
	}

	sender.Deploy(api.Running)
	eDeploy.startConverge(task, newEventAdapter(sender))

	go func(serviceNames []string) {
		defer sender.TaskComplete()
		eDeploy.waitForConverge(task)

		// NOTE(ssd) 2018-01-25: We don't use the timeout from
		// the request because a deploy outlives the request

		eDeploy.ensureStatus(context.Background(), serviceNames, s.ensureStatusTimeout)
		if !usedBootstrapBundle {
			eDeploy.maybeCreateInitialUser(serviceNames)
			eDeploy.maybeApplyLicense(serviceNames)
		}

		completionMsg := api.CompleteOk
		logMsg := "OK"
		if eDeploy.err != nil {
			completionMsg = api.CompleteFail
			logMsg = "FAIL"
			logrus.WithError(eDeploy.err).Error()
		}

		// TODO: "DEPLOY COMPLETE" is incorrect/misleading if we run this outside
		// of the context of a full install.
		logrus.WithFields(logrus.Fields{
			"status": logMsg,
		}).Debug("DEPLOY COMPLETE")

		sender.Deploy(completionMsg)
	}(serviceNames)

	return task, nil
}

func (s *server) doPreload(servicesToDeploy []string, sender events.EventSender) (*converge.Task, error) {
	s.deployment.Lock()
	defer s.deployment.Unlock()

	task, err := converge.NewTask()
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to generate converge task for preload: %s", err.Error())
	}

	s.senderStore.Set(task.ID.String(), sender)
	eDeploy := &errDeployer{server: s, err: nil, sender: sender}
	err = s.deployment.SetInstalledExpectations(servicesToDeploy)
	if err != nil {
		return nil, ErrorNoSuchService
	}

	eDeploy.ensureCerts()
	err = s.persistDeployment()
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to persist deployment database: %s", err.Error())
	}

	sender.Deploy(api.Running)
	eDeploy.startConverge(task, newEventAdapter(sender))

	go func() {
		defer sender.TaskComplete()
		eDeploy.waitForConverge(task)

		logMsg := "OK"
		if eDeploy.err != nil {
			sender.Deploy(api.CompleteFail)
			logMsg = "FAIL"
			logrus.WithError(eDeploy.err).Error()
		}

		logrus.WithFields(logrus.Fields{
			"status": logMsg,
		}).Debug("PRELOAD COMPLETE")

	}()

	return task, nil
}

func (s *server) doRemoveSome(oldServices []string) error {
	s.deployment.Lock()
	defer s.deployment.Unlock()

	task, err := converge.NewTask()
	if err != nil {
		return status.Errorf(codes.Internal, "failed to generate converge task for remove-some: %s", err.Error())
	}

	sender := s.newEventSender()
	err = s.deployment.SetRemovedExpectations(oldServices)
	if err != nil {
		return ErrorNoSuchService
	}

	err = s.persistDeployment()
	if err != nil {
		return status.Errorf(codes.Internal, "failed to persist deployment database: %s", err.Error())
	}

	eDeploy := &errDeployer{server: s, err: nil, sender: sender}
	eDeploy.convergeServices(task, newEventAdapter(sender))
	if eDeploy.err != nil {
		return errors.Errorf("Unable to remove services")
	}

	return nil
}

func (s *server) DeploySome(ctx context.Context,
	req *api.DeployRequest) (*api.DeployResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	sender := s.newEventSender()
	servicesToDeploy, err := s.parseRequestedServices(req.Services)
	if err != nil {
		return nil, err
	}

	task, err := s.doDeploySome(servicesToDeploy, sender, true, req.UsedBootstrapBundle)
	if err != nil {
		return nil, err
	}

	return &api.DeployResponse{TaskId: task.ID.String()}, nil
}

func (s *server) RemoveSome(ctx context.Context,
	req *api.RemoveRequest) (*api.RemoveResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	servicesToRemove, err := s.parseRequestedServices(req.Services)
	if err != nil {
		return nil, err
	}

	err = s.doRemoveSome(servicesToRemove)
	if err != nil {
		return nil, err
	}

	return &api.RemoveResponse{}, nil
}

func (s *server) parseRequestedServices(requestedServices []string) ([]string, error) {
	ret := make([]string, len(requestedServices))
	for i, sName := range requestedServices {
		pkg, err := habpkg.FromString(sName)
		if err != nil {
			return ret, ErrorServiceNameInvalid
		}

		_, found := s.deployment.ServiceByName(pkg.Name())
		if !found {
			return ret, ErrorNoSuchService
		}

		ret[i] = pkg.Name()
	}

	return ret, nil
}

func (s *server) NewDeployment(ctx context.Context, request *api.NewDeploymentRequest) (*api.DeploymentID, error) {
	logrus.Warn("GRPC request to deprecated NewDeployment endpoint")
	resp, err := s.ConfigureDeployment(ctx, &api.ConfigureDeploymentRequest{Config: request.Config})
	if err != nil {
		return nil, err
	}

	return resp.DeploymentId, nil
}

func (s *server) ConfigureDeployment(ctx context.Context,
	request *api.ConfigureDeploymentRequest) (*api.ConfigureDeploymentResponse, error) {

	overrideConfig := request.Config
	logrus.Debug("attempting to configure deployment")

	if err := overrideConfig.ValidateWithGlobalAndDefaultsAndCredentials(); err != nil {
		// Don't include the override config in the log message in case it has passwords in it
		logrus.WithError(err).Error("config validation failure")
		return nil, err
	}

	// NOTE: at this point, we have a config passed in from the user, that if we
	// applied the defaults to it then we should be able to deploy all the
	// services correctly based on validation
	err := s.deployment.UpdateWithUserOverrideConfig(overrideConfig, s.secretStore)
	if err != nil {
		logrus.WithError(err).Error("failed to store updated configuration")
		return nil, err
	}

	s.deployment.LastAction = "configured"
	err = s.persistDeployment()
	if err != nil {
		return nil, err
	}

	//TODO(jaym) data race, this should probably be on deployment
	s.releaseManifestProvider = s.initializeManifestProvider(s.deployment.Config)

	// This will make sure that deployment service is up-to-date
	// TODO(jaym): We need to teach the server how to gracefully exit, otherwise
	// its possible for the server to be killed before responding
	err = s.convergeDeployment()
	if err != nil {
		return nil, err
	}

	deploymentStatus := s.deployment.Status()
	configSHA1 := sha1.Sum([]byte(overrideConfig.String())) // nosemgrep

	logrus.WithFields(logrus.Fields{
		"config_sha1": fmt.Sprintf("%x", configSHA1),
		"id":          deploymentStatus.Id,
		"created_at":  ptypes.TimestampString(deploymentStatus.CreatedAt),
		"last_action": deploymentStatus.LastAction,
	}).Info("configured deployment")

	return &api.ConfigureDeploymentResponse{
		DeploymentId: &deploymentStatus,
		Config:       s.deployment.Config,
	}, nil
}

func (s *server) persistDeployment() error {
	s.runConfigMigrations(s.deployment)

	_, err := s.deploymentStore.UpdateDeployment(func(d *deployment.Deployment) error {
		// TODO(jaym) This is not the right way to interact with deployment store. We should
		//            modify the deployment it gives us with the updates we want and not
		//            keep around a copy of it in memory. It turns out that doing that is
		//            still too hard because there are objects that need to be initialized
		//            one the deployment after deserialization.
		*d = *s.deployment // nolint: govet
		return nil
	})

	if err != nil {
		logrus.WithError(err).Error("Failed to persist deployment")
		return err
	}
	return nil
}

func (s *server) Status(ctx context.Context, d *api.StatusRequest) (*api.StatusResponse, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	nonSkippedServices := make([]string, 0, len(s.deployment.ExpectedServices)+1)
	nonSkippedServices = append(nonSkippedServices, deploymentServiceName)
	for _, svcString := range s.deployment.ServiceNames() {
		if svc, found := s.deployment.ServiceByName(svcString); found {
			if svc.DeploymentState == deployment.Running {
				nonSkippedServices = append(nonSkippedServices, svcString)
			}
		}
	}
	serviceStatus := s.target().Status(ctx, nonSkippedServices)

	return &api.StatusResponse{
		ServiceStatus:    serviceStatus,
		DeploymentConfig: s.deployment.Config.Deployment,
	}, nil
}

func (s *server) ServiceVersions(ctx context.Context,
	d *api.ServiceVersionsRequest) (*api.ServiceVersionsResponse, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	c := s.target().HabAPIClient()
	svcList, err := c.ListServices(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "Habitat API error: %s", err.Error())
	}

	resp := &api.ServiceVersionsResponse{}
	resp.Services = make([]*api.ServiceVersion, 0, len(s.deployment.ExpectedServices)+1)
	for _, svc := range s.deployment.ExpectedServices {
		v := serviceVersionForService(svcList, svc.Name())
		if v == nil {
			logrus.WithField("svc_name", svc.Name()).Warn("no version information")
			continue
		}

		resp.Services = append(resp.Services, v)
	}

	v := serviceVersionForService(svcList, deploymentServiceName)
	if v == nil {
		logrus.WithField("svc_name", deploymentServiceName).Warn("no version information")
	} else {
		resp.Services = append(resp.Services, v)
	}

	return resp, nil
}

func serviceVersionForService(svcList []habapi.ServiceInfo, name string) *api.ServiceVersion {
	info, found := habapi.ServiceInfoByName(svcList, name)
	if !found {
		return nil
	}
	return &api.ServiceVersion{
		Name:    info.Pkg.Name,
		Origin:  info.Pkg.Origin,
		Release: info.Pkg.Release,
		Version: info.Pkg.Version,
	}
}

// ensure that all services return a successful status
func (s *errDeployer) ensureStatus(ctx context.Context, serviceList []string, timeout time.Duration) {
	if s.err != nil {
		return
	}

	e := s.sender

	logctx := logrus.WithFields(logrus.Fields{
		"mod":     "server.ensureStatus",
		"timeout": timeout,
	})

	//To remove internal services from health check in case External ES or PG is enabled

	serviceList = skipServicesForHealthCheck(serviceList, s, logctx)

	var status *api.ServiceStatus
	e.Phase(api.Running, events.CheckingServiceHealth)
	logctx.Debug("waiting for all services to be healthy")
	elapsedTime := time.Duration(0)
	for attempts := 0; ; attempts++ {
		logctx.Debugf("health check attempt %d (%.1fs elapsed) (%.1fs interval)",
			attempts, elapsedTime.Seconds(), s.ensureStatusInterval.Seconds())
		if elapsedTime > timeout {
			err := &deployment.StatusTimeoutError{
				Status:  status,
				Timeout: timeout,
			}
			// TODO its still weird that we format this status on the server side
			e.PhaseStep(api.CompleteFail, events.CheckingServiceHealth, "", err.Error())
			e.Phase(api.CompleteFail, events.CheckingServiceHealth)
			logctx.Debug("timeout")
			s.err = err
			return
		}

		status = s.target().Status(ctx, serviceList)

		success := status.AllHealthy()

		if success {
			break
		} else {
			e.PhaseStep(api.Running, events.CheckingServiceHealth, ".", "")
			time.Sleep(s.ensureStatusInterval)
			elapsedTime = elapsedTime + s.ensureStatusInterval
		}
	}

	e.PhaseStep(api.CompleteOk, events.CheckingServiceHealth, "all services healthy", "")
	e.Phase(api.CompleteOk, events.CheckingServiceHealth)
	logctx.Debug("all services healthy")
}

func skipServicesForHealthCheck(serviceList []string, s *errDeployer, logctx *logrus.Entry) []string {
	servicesToSkip := make(map[string]bool)

	enableExternalPg := s.deployment.Config.GetGlobal().GetV1().GetExternal().GetPostgresql().GetEnable().GetValue()
	if enableExternalPg {
		logctx.Debug("External PG is enabled.")
		servicesToSkip[constants.AutomatePGService] = true
	}
	enableExternalEs := s.deployment.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()

	if enableExternalEs {
		logctx.Debug("External ES is enabled.")
		servicesToSkip[constants.AutomateSearchService] = true
	}

	for i, v := range serviceList {
		if _, ok := servicesToSkip[v]; ok {
			logctx.Debug("Removed service " + v + " from Health Check")
			serviceList = append(serviceList[:i], serviceList[i+1:]...)
		}
	}
	return serviceList
}

func (s *server) Ping(context.Context, *api.PingRequest) (*api.PingResponse, error) {
	return &api.PingResponse{}, nil
}

func (s *server) DeployStatus(
	req *api.DeployStatusRequest,
	stream api.Deployment_DeployStatusServer) error {

	if !s.HasConfiguredDeployment() {
		return ErrorNotConfigured
	}

	mySender := func(event *api.DeployEvent) (err error) {
		return stream.Send(event)
	}

	sender, found := s.senderStore.Get(req.TaskId)
	if !found {
		return ErrorNoSuchTask
	}

	err := sender.StreamTo(mySender)
	logrus.Debugf("DeployStatus complete: %v", err)
	return err
}

func (s *server) SystemLogs(_ *api.SystemLogsRequest,
	stream api.Deployment_SystemLogsServer) error {
	logrus.Debug("starting a system logs command")

	cmd := exec.Command("journalctl", "-u", "chef-automate", "-f")
	stdout, err := cmd.StdoutPipe()

	if err != nil {
		logrus.Errorf("Error getting journalctl stdout pipe: %v", err)
		return err
	}
	if err = cmd.Start(); err != nil {
		logrus.Errorf("Error starting journalctl command: %v", err)
		return err
	}

	go func() {
		_, err := cmd.Process.Wait()
		if err != nil {
			// If wait fails, there isn't much we can do
			// about it from here other than log.
			logrus.WithError(err).Error("wait failed on journalctl process")
		}
	}()

	defer func() {
		// NOTE(ssd) 2018-06-05: We think this is safe to call
		// unconditionally since the go runtime provides
		// synchronization around Wait() and Kill()
		err := cmd.Process.Kill()
		if err != nil {
			logrus.WithError(err).Error("kill failed on journalctl process")
		}
	}()

	buf := make([]byte, 50)
	for {
		bytesRead, err := stdout.Read(buf)
		if bytesRead > 0 {
			logLine := api.LogLine{Line: string(buf[:bytesRead])}
			sendErr := stream.Send(&logLine)
			if sendErr != nil {
				// When client does a ctrl+c we get this
				logrus.Debugf("Stream appears to be closed: %v", sendErr)
				return sendErr
			}
		}
		if err == io.EOF {
			logrus.Info("journalctl sent EOF while we were tailing")
			return nil
		}
		if err != nil {
			logrus.Errorf("Error trying to read from stdout pipe: %v", err)
			return err
		}
	}
}

func (s *server) SetLogLevel(_ context.Context, _ *api.SetLogLevelRequest) (*api.SetLogLevelResponse, error) {
	return nil, status.Error(codes.Unimplemented, "the SetLogLevel method has been replaced by the debug set-log-level command")
}

// StartServer starts the automate deployment gRPC server
func StartServer(config *Config) error {
	address := config.getAddressString()
	setLogrusLevel(config.LogLevel)
	setAndLogProcessState()

	server := &server{
		serverConfig:         config,
		ensureStatusTimeout:  durationFromSecs(config.EnsureStatusTimeoutSecs, defaultEnsureStatusTimeout),
		ensureStatusInterval: durationFromSecs(config.EnsureStatusIntervalSecs, defaultEnsureStatusInterval),
	}

	database, err := initDatabase()
	if err != nil {
		return errors.Wrap(err, "could not initiate database")
	}
	defer database.Close()
	server.deploymentStore = boltdb.NewDeploymentStore(database)

	err = server.initSecretStore()
	if err != nil {
		return errors.Wrap(err, "failed to initialize secret store")
	}

	err = server.initDeploymentFromDB()
	if err != nil {
		return errors.Wrap(err, "failed to initialize deployment")
	}

	maybeResetHabPath(server)

	certs, _, err := server.readOrGenDeploymentServiceCerts()
	if err != nil {
		return errors.Wrap(err, "failed to generate TLS certificate")
	}

	server.connFactory = secureconn.NewFactory(*certs, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))
	listener, err := net.Listen("tcp", address)
	if err != nil {
		return errors.Wrapf(err, "could not listen on address: %s", address)
	}

	logrusEntry := logrus.NewEntry(logrus.StandardLogger())
	logrusOpts := []grpc_logrus.Option{
		// Don't spam the log with health-check logs
		grpc_logrus.WithDecider(func(m string, _ error) bool {
			return !stringutils.SliceContains([]string{
				// Called by our health-check script
				"/chef.automate.domain.deployment.Deployment/Ping",
				// Called on every chef-automate command for auto-updating
				"/chef.automate.domain.deployment.Deployment/ManifestVersion",
			}, m)
		}),
	}

	logrus.Infof("Starting GRPC automate-deploy service on %s", address)
	grpcServer := server.connFactory.NewServer(
		grpc.StreamInterceptor(grpc_logrus.StreamServerInterceptor(logrusEntry, logrusOpts...)),
		grpc.UnaryInterceptor(grpc_middleware.ChainUnaryServer(
			grpc_logrus.UnaryServerInterceptor(logrusEntry, logrusOpts...),
			tracing.ServerInterceptor(tracing.GlobalTracer()))))

	server.converger = converge.StartConverger()
	err = server.reloadBackupRunner()
	if err != nil {
		return errors.Wrap(err, "failed to load the backup runner")
	}

	// register grpc services
	api.RegisterDeploymentServer(grpcServer, server)
	api.RegisterCertificateAuthorityServiceServer(grpcServer, server)

	reflection.Register(grpcServer)

	convergeIntervalDuration := durationFromSecs(config.ConvergeIntervalSecs, defaultConvergeInterval)

	server.convergeLoop = NewLooper(convergeIntervalDuration, periodicConverger(server, grpcServer))

	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP)
	go func() {
		sig := <-ch
		grpcServer.GracefulStop()
		logrus.WithField("signal", sig).Info("Exiting")
		os.Exit(0)
	}()

	usrChan := make(chan os.Signal, 1)
	signal.Notify(usrChan, syscall.SIGUSR1)
	go func() {
		sig := <-usrChan
		logrus.WithField("signal", sig).Info("Stopping all Chef Automate services")
		go func() {
			err := server.shutItAllDown()
			if err != nil {
				logrus.WithError(err).Error("Failed to shut down Automate from signal handler")
			}
		}()
	}()

	server.convergeLoop.Start()
	return grpcServer.Serve(listener)
}

func periodicConverger(s *server, grpcServer *grpc.Server) func() {
	return func() {
		if s.convergeDisabled() {
			logrus.WithField(
				"file", s.serverConfig.ConvergeDisableFile,
			).Warn("Skipping periodic converge because disable file is present")
			return
		}

		logrus.Debug("Starting periodic converge")

		restartRequired, err := s.periodicCertCheck()
		if err != nil {
			logrus.WithError(err).Warn("Periodic certificate authority validity check failed")
			return
		}

		if restartRequired {
			logrus.Info("Certificate changes require deployment-service restart. Restarting now.")
			grpcServer.GracefulStop()
			os.Exit(0)
		}

		err = s.convergeDeployment()
		if err != nil {
			logrus.WithError(err).Error("Periodic converge failed")
		}

		logrus.Debug("Periodic converge complete")
	}
}

func (s *server) periodicCertCheck() (bool, error) {
	s.deployment.Lock()
	defer s.deployment.Unlock()

	// Check that we have a valid certificate authority.
	// If we don't we need to regenerate it and then
	// restart the deployment service.
	err := s.deployment.CA().ValidateCA()
	if err != nil {
		logrus.WithError(err).Info("Certificate authority regeneration required.")
		err := s.deployment.CA().RegenerateRoot()
		if err != nil {
			return false, err
		}

		return true, nil
	}

	// Check the deployment-service certificates. These
	// are not rendered via configuration currently so we
	// need to check them outside of the main converge
	// loop.
	_, regen, err := s.readOrGenDeploymentServiceCerts()
	if regen {
		logrus.WithError(err).Info("deployment-service certificate's regenerated")
	}
	return regen, err
}

// shouldFetchManifests returns true if the server is supposed to
// fetch the manifest from the currently configured channel.
func (s *server) shouldFetchManifest() bool {
	return s.deployment.CurrentReleaseManifest == nil || s.deployment.Config.Deployment.GetV1().GetSvc().GetUpgradeStrategy().GetValue() != "none"
}

//isCompatibleForConverge checks the converge from v1 to v2 is possible or not
func (s *server) isCompatibleForConverge(v1, v2 string) bool {
	if v1 == v2 {
		return true
	}
	v1Major, isV1SemFmt := manifest.IsSemVersionFmt(v1)
	v2Major, isV2SemFmt := manifest.IsSemVersionFmt(v2)

	if isV1SemFmt != isV2SemFmt || v1Major != v2Major {
		return false
	}
	if (!isV1SemFmt && !isV2SemFmt && v1 < v2) || (isV1SemFmt && isV2SemFmt && manifest.IsCompareSemVersions(v1, v2)) {
		return true
	}

	return false
}

// nextManifest returns the next A2 manifest that we should use.  This
// is either the manifest currently in use or the latest manifest on
// the configured channel. If neither are available, an error is
// returned.
func (s *server) nextManifest() (*manifest.A2, error) {
	if !s.shouldFetchManifest() {
		return s.deployment.CurrentReleaseManifest, nil
	}

	ctx := context.Background()
	var nextManifestVersion string
	var err error

	if s.deployment.CurrentReleaseManifest == nil {
		nextManifest, err := s.releaseManifestProvider.GetCurrentManifest(context.Background(), s.deployment.Channel())
		if err != nil {
			logrus.WithError(err).Error("Could not fetch manifest")
			if s.deployment.CurrentReleaseManifest == nil {
				return nil, errors.New("Release manifest not available")
			}
			// Continue with last manifest even in the case of an error because we still want to converge
			logrus.Info("Continuing converge with last known manifest")
			return s.deployment.CurrentReleaseManifest, nil
		}
		return nextManifest, nil
	}

	currentVersion := s.deployment.CurrentReleaseManifest.Version()
	_, _, nextManifestVersion, err = s.releaseManifestProvider.GetCompatibleVersion(ctx, s.deployment.Channel(), currentVersion, "")

	if err != nil {
		logrus.WithError(err).Error("Could not fetch next manifest version")

		// Continue with last manifest even in the case of an error because we still want to converge
		logrus.Info("Continuing converge with last known manifest")
		return s.deployment.CurrentReleaseManifest, nil
	}
	// Never use an older manifest. If the user has explicitly upgraded to a manifest that is newer
	// than what is the channel, we keep using our current manifest until the channel catches up.
	if s.isCompatibleForConverge(s.deployment.CurrentReleaseManifest.Version(), nextManifestVersion) {
		m, err := s.releaseManifestProvider.GetManifest(ctx, nextManifestVersion)
		if err != nil {
			logrus.WithError(err).Error("Could not fetch manifest")
			if s.deployment.CurrentReleaseManifest == nil {
				return nil, errors.New("Release manifest not available")
			}
			// Continue with last manifest even in the case of an error because we still want to converge
			logrus.Info("Continuing converge with last known manifest")
			return s.deployment.CurrentReleaseManifest, nil
		}
		return m, nil
	}
	logrus.Infof("The next available version %s in channel %q requires a manual upgrade with --major flag from the current version %s. Thus, ignoring auto-upgrade",
		s.deployment.Channel(), nextManifestVersion, s.deployment.CurrentReleaseManifest.Version())

	return s.deployment.CurrentReleaseManifest, nil
}

func (s *server) convergeDeployment() error {
	if s.deployment == nil || s.releaseManifestProvider == nil {
		logrus.Debug("Cannot converge nil or un-deployed deployment")
		return errors.New("Uninitialized deployment")
	}

	task, err := converge.NewTask()
	if err != nil {
		return errors.Wrap(err, "could not generate converge task")
	}

	nextManifest, err := s.nextManifest()
	if err != nil {
		return err
	}

	// TODO(ssd) 2018-02-09: Right now this is locking the
	// deployment through the entire converge, even though it
	// doesn't need to.
	s.deployment.Lock()
	defer s.deployment.Unlock()

	if s.deployment.CurrentReleaseManifest != nil {
		currentVersion := s.deployment.CurrentReleaseManifest.Version()
		nextVersion := nextManifest.Version()

		logctx := logrus.WithFields(logrus.Fields{
			"current_manifest": currentVersion,
			"next_manifest":    nextVersion,
		})
		if nextVersion > currentVersion {
			//Todo(milestone) modify the comparison logic, should not use simple less than or greater than
			logctx.Info("Starting converge with UPDATED manifest")
		} else if nextVersion < currentVersion {
			// We should not see this because of the current implementation of
			// nextManifest()
			logctx.Warn("Starting converge with DOWNGRADED manifest")
		} else {
			logctx.Debug("Starting converge")
		}
	} else {
		logrus.WithFields(logrus.Fields{
			"next_manifest": nextManifest.Version(),
		}).Info("Starting converge with NEW manifest")
	}

	s.deployment.CurrentReleaseManifest = nextManifest
	err = s.updateExpectedServices()
	if err != nil {
		logrus.WithError(err).Error("failed to update expected services")
		return err
	}

	eDeploy := &errDeployer{server: s}
	eDeploy.convergeServices(task, newConvergeLoopLogger())
	if eDeploy.err != nil {
		if !api.IsDeploymentServicePendingError(eDeploy.err) {
			logrus.WithError(err).Error("Failed to converge")
		}
	}
	return eDeploy.err
}

// doConverge is a utility function to perform an operation on the server,
// start a converge, and stream the converge events to a given event sender,
// and handle any errors that occur in the converge loop. It returns a task
// that can be used to subscribe to converge events.
//
//  operation: Is a function that takes a pointer to the server and
//   performs any pre-converge tasks that manipulate state. If this
//   operation fails it should return a gRPC status.Status
//   with the proper fields configured.
//  sender: Is the event sender that is used to publish events.
//  sink: converge event "sink" which acts as a filter and publisher
//   to the sender.
//  errHandler: Is a callback function used to handle any errors that
//    may occur in the converge routine.
//
func (s *server) doConverge(
	operation convergeOperation,
	sender events.EventSender,
	sink converge.EventSink,
	errHandler convergeErrorHandler) (*converge.Task, error) {

	// Lock the deployment while we mutate deployment state with the user
	// operation, updating the sender store, and building the desired
	// state when we start the converge.
	s.deployment.Lock()
	defer s.deployment.Unlock()

	task, err := converge.NewTask()
	if err != nil {
		return nil, errors.Wrap(err, "could not generate converge task")
	}
	s.senderStore.Store(task.ID.String(), sender)
	eDeploy := &errDeployer{server: s, err: nil, sender: sender}

	if err := operation(s); err != nil {
		return nil, err
	}

	sender.Deploy(api.Running)
	eDeploy.startConverge(task, sink)
	go func() {
		defer sender.TaskComplete()
		defer os.Setenv(isUpgradeMajorEnv, "false")

		eDeploy.waitForConverge(task)
		// TODO: We need some way to know that the hab supervisor has called
		// the right hooks on each service before we check the status. Otherwise,
		// we can run into cases where we check the status of the services
		// before hab has applied all changes.
		eDeploy.ensureStatus(context.Background(), s.deployment.NotSkippedServiceNames(), s.ensureStatusTimeout)

		errHandler(eDeploy.err)
		// json file
		if os.Getenv(isUpgradeMajorEnv) == "true" {
			var err error
			ci, err := majorupgradechecklist.NewPostChecklistManager(s.deployment.CurrentReleaseManifest.Version())
			if err != nil {
				errHandler(err)
			}
			err = ci.CreatePostChecklistFile(majorupgradechecklist.UPGRADE_METADATA, majorupgradechecklist.IsExternalPG())
			if err != nil {
				errHandler(err)
			}
		}
	}()

	return task, nil
}

func (s *server) updateExpectedServices() error {
	err := s.deployment.UpdateExpectedServicesFromManifest()
	if err != nil {
		return errors.Wrap(err, "failed to update expected services from internal service list")
	}

	err = s.deployment.EnsureCerts()
	if err != nil {
		return errors.Wrap(err, "failed to ensure all services have TLS certificates")
	}

	return s.persistDeployment()
}

func (s *server) initDeploymentFromDB() error {
	err := s.deploymentStore.Initialize()
	if err != nil {
		logrus.WithError(err).Error("could not initialize database")
		return err
	}

	existingDeployment, err := s.deploymentStore.GetDeployment()
	switch err {
	case nil:
		logrus.WithFields(
			logrus.Fields{
				"id":         existingDeployment.ID,
				"created_at": existingDeployment.CreatedAt,
				"deployed":   existingDeployment.Deployed,
			}).Info("Found existing deployment in the database")
	case persistence.ErrDoesNotExist:
		logrus.Info("Creating a deployment as no existing deployment was found in the database")
		existingDeployment, err = deployment.CreateDeployment()
		if err != nil {
			logrus.WithError(err).Error("could not create new deployment")
			return err
		}
		logrus.WithFields(logrus.Fields{
			"id":         existingDeployment.ID,
			"created_at": existingDeployment.CreatedAt,
			"deployed":   existingDeployment.Deployed,
		}).Info("New deployment created")
	default:
		logrus.WithError(err).Error("could not restore deployment from database")
		return err
	}

	err = existingDeployment.InitCA(DataDir)
	if err != nil {
		logrus.WithError(err).Error("Failed to initialize CA for existing deployment")
		return err
	}

	// TODO: set this elsewhere
	existingDeployment.SetTarget(target.NewLocalTarget(airgap.AirgapInUse()))

	// TODO(jaym): This should be on the deployment
	if existingDeployment.Config != nil {
		s.releaseManifestProvider = s.initializeManifestProvider(existingDeployment.Config)
	}

	s.deployment = existingDeployment

	return s.persistDeployment()
}

// runConfigMigrations does any necessary config migrations in the current config version
func (s *server) runConfigMigrations(d *deployment.Deployment) {
	c := d.GetUserOverrideConfigForPersistence()

	if c == nil {
		return
	}
	if c.GetGlobal().GetV1().GetFrontendTls() == nil &&
		c.GetLoadBalancer().GetV1().GetSys().GetFrontendTls() != nil {
		err := d.MergeIntoUserOverrideConfig(&dc.AutomateConfig{
			Global: &config.GlobalConfig{
				V1: &config.V1{
					FrontendTls: c.GetLoadBalancer().GetV1().GetSys().GetFrontendTls(),
				},
			},
		}, s.secretStore)
		if err != nil {
			logrus.WithError(err).Warn("Failed to migrate load balancer config into global config")
		}
	}

	if err := d.MoveSecretsToSecretStore(s.secretStore); err != nil {
		logrus.WithError(err).Warn("Failed to move secrets into the secret store")
	}
}

func (s *server) initSecretStore() error {
	var err error

	s.secretStore, err = secrets.NewDefaultSecretStore()
	if err != nil {
		return err
	}

	// Populate the secret store with secrets we need to ensure exist during
	// backup restoration.
	backupSecrets := []secrets.SecretName{
		{Group: "backup-gateway", Name: "access_key"},
		{Group: "backup-gateway", Name: "secret_key"},
	}

	for _, secret := range backupSecrets {
		exists, err := s.secretStore.Exists(secret)
		if err != nil {
			return err
		}

		if !exists {
			randomBytes, err := secrets.GenerateRandomBytes(64)
			if err != nil {
				return err
			}

			if err = s.secretStore.SetSecret(secret, randomBytes); err != nil {
				return err
			}
		}
	}

	return nil
}

func (s *server) initializeManifestProvider(config *dc.AutomateConfig) manifest.CachingReleaseManifestProvider {
	manifestPath := ""
	offlineMode := airgap.AirgapInUse()
	if offlineMode {
		manifestPath = api.AirgapManifestPath
	} else {
		manifestPath = config.GetDeployment().GetV1().GetSvc().GetManifestDirectory().GetValue()
	}

	logrus.WithFields(logrus.Fields{
		"manifestPath":   manifestPath,
		"hartifactsPath": config.GetDeployment().GetV1().GetSvc().GetHartifactsPath().GetValue(),
		"overrideOrigin": config.GetDeployment().GetV1().GetSvc().GetOverrideOrigin().GetValue(),
		"pollDuration":   config.GetDeployment().GetV1().GetSvc().GetManifestCacheExpiry().GetValue(),
		"offlineMode":    offlineMode,
	}).Info("Initializing manifest provider")

	baseManifest := manifest.NewLocalHartManifestProvider(
		client.NewDefaultClient(manifestPath),
		config.GetDeployment().GetV1().GetSvc().GetHartifactsPath().GetValue(),
		config.GetDeployment().GetV1().GetSvc().GetOverrideOrigin().GetValue())

	cacheExpiry, err := time.ParseDuration(config.GetDeployment().GetV1().GetSvc().GetManifestCacheExpiry().GetValue())
	if err != nil {
		// we've validated this at least twice by now (on the client and on the
		// server) so we shouldn't hit this, but in case we do we should just
		// fall back to the old behavior of not caching
		logrus.WithError(err).Warn("Failed to parse manifest cache expiry - not caching manifest")
		cacheExpiry = 0 * time.Second
	}
	return manifest.NewCachingReleaseManifestProvider(baseManifest, cacheExpiry)
}

func (s *server) shutItAllDown() error {
	if s.deployment == nil {
		return ErrorNoDeployment
	}

	taskStop, err := converge.NewTask()
	if err != nil {
		return err
	}

	s.deployment.Lock()
	defer s.deployment.Unlock()

	logger := newConvergeLoopLogger()
	err = s.converger.PrepareForShutdown(taskStop, s.target(), logger)
	if err != nil && err != api.ErrRestartPending {
		return err
	}

	// Do not use the incoming context. We want to go through with the entire process
	// regardless of if the user disconnects
	ctx, cancel := context.WithTimeout(context.Background(), defaultWaitForDownTimeout)
	defer cancel()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case err := <-taskStop.C:
		if err != nil && err != api.ErrRestartPending {
			logrus.WithError(err).Error("Failed to stop services")
			return err
		}
	}

	// NOTE(ssd) 2019-07-12: We use context.Background here
	// because we want the hab-sup term command to continue even
	// after this commands exits.
	if err := s.target().Stop(context.Background()); err != nil {
		return err
	}

	return nil
}

func (s *server) Stop(context.Context, *api.StopRequest) (*api.StopResponse, error) {
	err := s.shutItAllDown()
	if err != nil {
		logrus.WithError(err).Error("Failed to shut down Automate")
	}
	return &api.StopResponse{}, err
}

func (s *server) target() target.Target {
	return s.deployment.Target()
}

// convergeDisabled will return true if the server configured disable
// sentinel file exists.
func (s *server) convergeDisabled() bool {
	_, err := os.Stat(s.serverConfig.ConvergeDisableFile)

	return err == nil
}

func initDatabase() (*bolt.DB, error) {
	dbFile := path.Join(DataDir, DBName)
	database, err := bolt.Open(dbFile, 0600, nil)
	if err != nil {
		return nil, err
	}

	return database, nil
}

// applyLicense attempts to apply a license from the config file if provided
func (s *errDeployer) applyLicense() {
	if s.err != nil {
		return
	}

	lcConfig := s.deployment.Config.LicenseControl
	// return if no license content

	if lcConfig.GetV1().GetSvc().GetLicense().GetValue() == "" {
		return
	}

	e := s.sender
	e.Phase(api.Running, events.ApplyLicense)

	// For now we are throwing away the response.
	// When we have human friendly error status for applying a license we can add the information here.
	// Force Apply license when applying from an A2 Configuration
	_, err := s.UpdateLicense(context.Background(), lcConfig.GetV1().GetSvc().GetLicense().GetValue(), true)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"mod": "server.applyLicense",
		}).Debug(err)

		s.err = err
		return
	}
}

// maybeApplyLicense applies the configured license if the
// license-control-service is included in the list of deployed
// services.
func (s *errDeployer) maybeApplyLicense(deployedServices []string) {
	if s.err != nil {
		return
	}
	if stringutils.SliceContains(deployedServices, "license-control-service") {
		s.applyLicense()
	}
}

// maybeCreateInitialUser creates the initial admin user if the
// required services to create a local admin user have been deployed.
func (s *errDeployer) maybeCreateInitialUser(deployedServices []string) {
	if s.err != nil {
		return
	}

	hasLUS := false
	hasTS := false
	for _, svc := range deployedServices {
		switch svc {
		case "local-user-service":
			hasLUS = true
		case "teams-service":
			hasTS = true
		default:
			if hasLUS && hasTS {
				break
			}
		}
	}

	if hasLUS && hasTS {
		s.createInitialUser()
	} else {
		logrus.Info("Not creating initial because teams-service and local-user-service were not deployed")
	}
}

func (s *errDeployer) createInitialUser() {
	if s.err != nil {
		return
	}

	admin := s.deployment.Config.GetDeployment().GetV1().GetSvc().GetAdminUser()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	s.sender.Phase(api.Running, events.CreateAdminUser)
	userMgmtClient, err := s.userMgmtClient(ctx)
	if err != nil {
		s.sender.PhaseStep(api.CompleteFail, events.CreateAdminUser, "", err.Error())
		s.sender.Phase(api.CompleteFail, events.CreateAdminUser)
		s.err = errors.Wrap(err, "could not create user mgmt client")
		return
	}

	err = attemptToCreateInitialUser(ctx, userMgmtClient, s.sender, admin)
	if err != nil {
		s.err = errors.Wrap(err, "could not create initial admin user")
	}
}

func (s *server) userMgmtClient(ctx context.Context) (usermgmt.UserMgmt, error) {
	if s.umClient != nil {
		return s.umClient, nil
	}
	userMgmtClient, err := usermgmt_client.NewUserMgmtClient(ctx, s.connFactory,
		s.AddressForService("local-user-service"),
		s.AddressForService("teams-service"))
	if err != nil {
		return nil, err
	}

	s.umClient = userMgmtClient
	return s.umClient, nil
}

// attemptToCreateInitialUser is a helper that creates a user and adds it to the admins team
// or returns an error. It will handle all sender events necessary.
func attemptToCreateInitialUser(ctx context.Context,
	userMgmtClient usermgmt.UserMgmt,
	sender events.EventSender,
	adminUser *dc.ConfigRequest_V1_AdminUser) error {

	var name string
	var username string
	password := adminUser.GetPassword().GetValue()

	// In previous iteration of configuration a user could supply a username
	// and an email when configuring the admin user, e.g.:
	//
	// username => "Alice McAllister"
	// email => "alice@mcallister.com"
	//
	// This structure for admin users was chosen to conform to Dex's internal
	// abstraction. It was later determined that Dex does not enforce email
	// addresses for that field so we've since modified our representation for
	// the admin user to be name and username, e.g.:
	//
	// name => "Alice McAllister"
	// username => "alice"
	//
	// To handle deployments that have been configured with the legacy fields
	// we'll translate them to conform to the new field names.
	//
	// name => "Alice McAllister" (from username)
	// username => "alice@mcallister.com" (from email)

	if email := adminUser.GetEmail().GetValue(); email != "" {
		username = email
		name = adminUser.GetUsername().GetValue()
	} else {
		username = adminUser.GetUsername().GetValue()
		name = adminUser.GetName().GetValue()
	}

	userID, wasCreated, err := userMgmtClient.CreateUser(ctx, name, username, password)
	logger := logrus.WithFields(logrus.Fields{
		"name":     name,
		"user_id":  userID,
		"username": username,
	})
	if err != nil {
		sender.PhaseStep(api.CompleteFail, events.CreateAdminUser, "", err.Error())
		sender.Phase(api.CompleteFail, events.CreateAdminUser)
		logger.WithError(err).Error("Failed to create user")
		return err
	}

	if err = userMgmtClient.AddUserToAdminTeam(ctx, userID); err != nil {
		sender.Phase(api.CompleteFail, events.CreateAdminUser)
		logger.WithError(err).Error("Failed to add user to 'admins' team", username)
		return err
	}

	if wasCreated {
		sender.Phase(api.CompleteOk, events.CreateAdminUser)
		logger.Debug("User created")
	} else {
		msg := "Skipping user creation because username exists"
		sender.PhaseStep(api.CompleteOk, events.CreateAdminUser, msg, "")
		sender.Phase(api.CompleteOk, events.CreateAdminUser)
		logger.Debug(msg)
	}
	return nil
}

func (s *server) DumpDB(req *api.DumpDBRequest, stream api.Deployment_DumpDBServer) error {
	var dbBuf bytes.Buffer

	err := s.deploymentStore.WriteTo(&dbBuf)
	if err != nil {
		return err
	}

	readBuf := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.DumpDBResponse{Data: p})
	})
	_, err = io.CopyBuffer(writer, &dbBuf, readBuf)
	return err
}

// CurrentReleaseManifest returns the server's current release manifest
func (s *server) CurrentReleaseManifest(ctx context.Context, req *api.CurrentReleaseManifestRequest) (*api.ReleaseManifest, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	if s.deployment.CurrentReleaseManifest == nil {
		return nil, ErrorNoReleaseManifest
	}

	js, err := json.Marshal(s.deployment.CurrentReleaseManifest)
	if err != nil {
		return nil, status.Error(codes.Internal, "Failed to marshal package manifest to JSON")
	}

	return &api.ReleaseManifest{Json: js}, nil
}

func (s *server) ManifestVersion(ctx context.Context, d *api.ManifestVersionRequest) (*api.ManifestVersionResponse, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	if s.deployment.CurrentReleaseManifest == nil {
		return nil, ErrorNoReleaseManifest
	}

	m := s.deployment.CurrentReleaseManifest
	var cliRelease string
	pkg := manifest.InstallableFromManifest(m, "automate-cli")
	if pkg != nil {
		cliRelease = pkg.Release()
	}

	return &api.ManifestVersionResponse{
		BuildTimestamp: m.Version(),
		BuildSha:       m.BuildSHA,
		CliRelease:     cliRelease,
	}, nil
}

func (s *server) IsValidUpgrade(ctx context.Context, req *api.UpgradeRequest) (*api.ValidatedUpgradeResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	var currentRelease = ""
	s.deployment.Lock()
	if s.deployment.CurrentReleaseManifest != nil {
		currentRelease = s.deployment.CurrentReleaseManifest.Version()
	}
	channel := s.deployment.Channel()
	s.deployment.Unlock()

	nextManifestVersion := currentRelease
	if !airgap.AirgapInUse() { //internet connected machine
		isMinorAvailable, isMajorAvailable, compVersion, err := s.releaseManifestProvider.GetCompatibleVersion(ctx, channel, currentRelease, req.VersionsPath)
		if err != nil {
			return nil, err
		}

		if !req.IsMajorUpgrade { //normal upgrade
			if isMinorAvailable {
				nextManifestVersion = compVersion
			} else if isMajorAvailable {
				return nil, status.Errorf(codes.FailedPrecondition, "This is a Major upgrade. Please use `--major` flag in the above command")
			}
		} else { //major upgrade
			if isMinorAvailable {
				return nil, status.Errorf(codes.FailedPrecondition, "The next upgradable version is not a major version upgrade, please run the upgrade command without `--major` flag")
			} else if isMajorAvailable {
				nextManifestVersion = compVersion
			}
		}
	} else {
		m, err := s.releaseManifestProvider.RefreshManifest(ctx, channel)
		if err != nil {
			return nil, err
		}
		nextManifestVersion = m.Version()

		//compare minimum compatible version with current version, i.e current version should be greater than or equal than min compatible version
		minCompVersion := m.MinCompatibleVer
		if minCompVersion == "" {
			return nil, status.Error(codes.FailedPrecondition, "The minimum compatible version field is missing in the manifest, please create a bundle with the latest automate-cli")
		} else if !isCompatibleForAirgap(currentRelease, minCompVersion) {
			return nil, status.Errorf(codes.FailedPrecondition, "The version specified %q is not compatible with the current version %q. Please first upgrade to the minimum compatible version %q", nextManifestVersion, currentRelease, minCompVersion)
		}

		//check for upgrade or degrade, we should not allow degrading
		if isDegrade(currentRelease, nextManifestVersion) {
			return nil, status.Errorf(codes.InvalidArgument, "The version specified %q is older than the current version %q", nextManifestVersion, currentRelease)
		}

		isActualMajorUpgrade := isMajorUpgrade(currentRelease, nextManifestVersion)

		//check the upgrade is major or not, and if the upgrade is minor/patch, user should not provide --major flag
		if !isActualMajorUpgrade && req.IsMajorUpgrade {
			return nil, status.Errorf(codes.FailedPrecondition, "The next upgradable version is not a major version upgrade, please run the upgrade command without `--major` flag")
		}

		//check the upgrade is major or not, and if the upgrade is major user should provide --major flag.
		if isActualMajorUpgrade && !req.IsMajorUpgrade {
			return nil, status.Errorf(codes.FailedPrecondition, "This is a Major upgrade. Please use `--major` flag in the above command")
		}

	}

	if req.Version != "" {
		if airgap.AirgapInUse() {
			return nil, status.Errorf(codes.InvalidArgument, "specifying a version is not allowed in airgap mode, please use `chef-automate upgrade run --airgap-bundle`")
		}

		if nextManifestVersion != req.Version && !isCompatible(currentRelease, req.Version, nextManifestVersion) {
			return nil, status.Errorf(codes.InvalidArgument, "The specified version %q is not compatible with the current version %q. The current version can be upgraded to %q", req.Version, currentRelease, nextManifestVersion)
		}

		nextManifestVersion = req.Version
	}

	var ReadPendingPostChecklist = []string{}
	_, is_major_version := manifest.IsSemVersionFmt(currentRelease)
	if is_major_version {
		var err error

		pcm, err := majorupgradechecklist.NewPostChecklistManager(currentRelease)
		if err != nil {
			return nil, status.Errorf(codes.InvalidArgument, "Failed to get post checklist manager: %s", err)
		}

		ReadPendingPostChecklist, _ = pcm.ReadPendingPostChecklistFile(majorupgradechecklist.UPGRADE_METADATA, majorupgradechecklist.IsExternalPG())
	}

	if len(ReadPendingPostChecklist) == 0 {
		major, isSemFormat := manifest.IsSemVersionFmt(nextManifestVersion)
		resp := &api.ValidatedUpgradeResponse{
			CurrentVersion: currentRelease,
			TargetVersion:  nextManifestVersion,
		}
		if isSemFormat {
			resp.TargetMajor = major
		}
		return resp, nil
	} else {
		return nil, status.Errorf(codes.FailedPrecondition, "Please complete pending post checklist from version %s", currentRelease)
	}
}

// Upgrade requests the deployment-service pulls down the latest manifest and applies it
func (s *server) Upgrade(ctx context.Context, req *api.UpgradeRequest) (*api.UpgradeResponse, error) {

	validatedResp, err := s.IsValidUpgrade(ctx, req)
	if err != nil {
		return nil, err
	}

	m, err := s.releaseManifestProvider.GetManifest(ctx, validatedResp.TargetVersion)

	_, ok := err.(*manifest.NoSuchManifestError)
	if ok {
		return nil, status.Errorf(codes.NotFound, errors.Wrap(err, "No manifest for the specified version or channel could be found").Error())
	}
	if err != nil {
		return nil, err
	}

	if req.IsMajorUpgrade {
		os.Setenv(isUpgradeMajorEnv, "true")
	}

	s.deployment.Lock()
	// We don't compare the releases to make sure this command works with the hart overrides
	s.deployment.CurrentReleaseManifest = m
	s.deployment.Unlock()

	operation := func(s *server) error {
		return s.persistDeployment()
	}

	// TODO (jaym): the task id is going to get reset when deployment-service
	//              restarts. It cannot be used to stream events
	sender := s.newEventSender()
	errHandler := deployErrorHandler(sender)
	sink := newEventAdapter(sender)
	task, err := s.doConverge(operation, sender, sink, errHandler)
	if err != nil {
		return nil, err
	}

	return &api.UpgradeResponse{
		PreviousVersion: validatedResp.CurrentVersion,
		NextVersion:     m.Version(),
		TaskId:          task.ID.String(),
	}, nil
}

//isDegrade return true, if the v2 is previous version of v1
func isDegrade(v1, v2 string) bool {
	_, isV1Sem := manifest.IsSemVersionFmt(v1)
	_, isV2Sem := manifest.IsSemVersionFmt(v2)

	if !isV1Sem && !isV2Sem && v1 > v2 {
		return true
	}
	if isV1Sem && isV2Sem && !manifest.IsCompareSemVersions(v1, v2) {
		return true
	}

	if isV1Sem && !isV2Sem {
		return true
	}
	return false
}

// IsMajorUpgrade returns true if the v2 is next major upgrade of v1 else return false
func isMajorUpgrade(v1, v2 string) bool {
	v1Major, isV1Sem := manifest.IsSemVersionFmt(v1)
	v2Major, isV2Sem := manifest.IsSemVersionFmt(v2)

	if !isV1Sem && isV2Sem {
		return true
	}

	if isV1Sem && isV2Sem && v1Major != v2Major {
		return true
	}

	return false
}

//isCompatibleForAirgap will return true if the current version is greater than or equal to min compatible version
func isCompatibleForAirgap(current, minCompatibleVer string) bool {
	_, isCurrentSem := manifest.IsSemVersionFmt(current)
	_, isMinCompSem := manifest.IsSemVersionFmt(minCompatibleVer)

	if !isCurrentSem && !isMinCompSem {
		return current >= minCompatibleVer
	}

	if isCurrentSem && isMinCompSem {
		return manifest.IsCompareSemVersions(minCompatibleVer, current)
	}

	//if minCompatibleVer is in timestampversion and current version is semantic version - we need to allow the upgrade
	if !isMinCompSem && isCurrentSem {
		return true
	}
	return false
}

func isCompatible(current, givenVersion, maxPossibleVersion string) bool {
	_, isCurrentSem := manifest.IsSemVersionFmt(current)
	givenMajor, isGivenVSem := manifest.IsSemVersionFmt(givenVersion)
	maxPossibleMajor, isMaxPossibleSem := manifest.IsSemVersionFmt(maxPossibleVersion)

	if !isCurrentSem { //current version is in timestamp version
		if !isGivenVSem { //given version is in timestamp version
			if !isMaxPossibleSem { //max possible version is in timestamp version
				if current < givenVersion && givenVersion <= maxPossibleVersion {
					return true
				}
				return false
			} else { //max possible version is in semantic version
				return current < givenVersion
			}
		} else { //given version is in semantic version
			if !isMaxPossibleSem { //max possible version is in timestamp version
				return false
			} else { //max possible version is in semantic version
				if givenMajor == maxPossibleMajor {
					return manifest.IsCompareSemVersions(givenVersion, maxPossibleVersion)
				}
			}
		}
	} else { //current version is in semantic version
		if !isGivenVSem || !isMaxPossibleSem { // given version is in time stamp version or max possible version is in timestamp version
			return false
		}
		if manifest.IsCompareSemVersions(current, givenVersion) && givenMajor == maxPossibleMajor {
			return manifest.IsCompareSemVersions(givenVersion, maxPossibleVersion)
		}
	}
	return false
}

func (s *server) getPackageCleanupMode() string {
	return s.deployment.Config.GetDeployment().GetV1().GetSvc().GetPackageCleanupMode().GetValue()
}

func deployErrorHandler(sender events.EventSender) func(err error) {
	return func(err error) {
		if err == api.ErrSelfUpgradePending {
			sender.Deploy(api.SelfUpgradePending)
			return
		}

		if err == api.ErrSelfReconfigurePending {
			sender.Deploy(api.SelfReconfigurePending)
			return
		}

		if err != nil {
			sender.Deploy(api.CompleteFail)
			return
		}

		sender.Deploy(api.CompleteOk)
	}
}

func (s *server) genDeploymentServiceCerts(req certauthority.CertRequest) (*certs.ServiceCerts, error) {
	tlsCertData, err := s.deployment.CA().CertDataForService(req)
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate deployment-service TLS certificate")
	}

	tlsCerts, err := certs.ServiceCertsFromBytes(
		[]byte(tlsCertData.Cert),
		[]byte(tlsCertData.Key),
		[]byte(*tlsCertData.RootCert))
	if err != nil {
		return nil, errors.Wrap(err, "Could not parse certificate data")
	}

	err = ioutil.WriteFile(constants.CertPath, []byte(tlsCertData.Cert), 0600)
	if err != nil {
		logrus.WithError(err).Error("failed to write certificate data to disk")
	}

	err = ioutil.WriteFile(constants.KeyPath, []byte(tlsCertData.Key), 0600)
	if err != nil {
		logrus.WithError(err).Error("failed to write certificate data to disk")
	}

	err = ioutil.WriteFile(constants.RootCertPath, []byte(*tlsCertData.RootCert), 0600)
	if err != nil {
		logrus.WithError(err).Error("failed to write certificate data to disk")
	}

	return tlsCerts, nil
}

func (s *server) readDeploymentServiceCerts() (*certs.ServiceCerts, error) {
	c := certs.TLSConfig{
		CertPath:       constants.CertPath,
		KeyPath:        constants.KeyPath,
		RootCACertPath: constants.RootCertPath,
	}
	return c.ReadCerts()
}

// validateCerts checks the deployment-service certificates and makes
// sure they are consistent with the CA. This check goes beyond our
// standard ValidateCertificateRequest checks in an attempt to
// investigate issues we've seen in the development studio in the
// past.
func (s *server) validateCerts(certs *certs.ServiceCerts, req certauthority.CertRequest) error {
	ca := s.deployment.CA()
	// NOTE(ssd) 2018-05-04: We don't check if the key matches the
	// cert because this check already happens when we construct
	// an x509KeyPair.
	localCert, err := certauthority.BytesToCert(certs.ServiceKeyPair.Certificate[0])
	if err != nil {
		return errors.Wrap(err, "invalid certificate data")
	}

	err = ca.ValidateCertificateForRequest(localCert, req)
	if err != nil {
		return err
	}

	// Check if the on-disk root_ca.crt is the same as the root cert in our current CA
	caRoot, err := certauthority.PEMToCert(ca.RootCert())
	if err != nil {
		return errors.Wrap(err, "certificate authority's root cert could not be converted to x509.Certificate")
	}

	localRoot := certs.RootCACert
	if caRoot.Subject.CommonName != localRoot.Subject.CommonName {
		return errors.Wrap(err, "root CA cert found on disk and root CA cert from certificate authority mismatch")
	}

	return nil
}

func (s *server) readAndValidate(req certauthority.CertRequest) (*certs.ServiceCerts, error) {
	tlsCerts, err := s.readDeploymentServiceCerts()
	if err != nil {
		return nil, err
	}

	err = s.validateCerts(tlsCerts, req)
	if err != nil {
		return nil, err
	}

	return tlsCerts, nil
}

func (s *server) readOrGenDeploymentServiceCerts() (*certs.ServiceCerts, bool, error) {
	req := certauthority.NewCertRequest(deploymentServiceName, s.target().IPs(), []string{})
	if fileExist(constants.CertPath) && fileExist(constants.KeyPath) && fileExist(constants.RootCertPath) {
		tlsCerts, err := s.readAndValidate(req)
		if err != nil {
			logrus.WithError(err).Info("deployment-service certificate regeneration required")
			cleanupOldCerts()
		} else {
			return tlsCerts, false, nil
		}
	}

	tlsCerts, err := s.genDeploymentServiceCerts(req)
	return tlsCerts, true, err
}

func cleanupOldCerts() {
	logrus.Info("cleaning up old deployment-service certificate")
	tryRemoveFile(constants.CertPath)
	logrus.Info("cleaning up old deployment-service key")
	tryRemoveFile(constants.KeyPath)
	logrus.Info("cleaning up old deployment-service root ca cert")
	tryRemoveFile(constants.RootCertPath)
}

// tryRemoveFile attempts to remove a file and logs any failure
func tryRemoveFile(path string) {
	err := os.Remove(path)
	if err != nil && !os.IsNotExist(err) {
		logrus.WithFields(logrus.Fields{"file": path}).WithError(err).Warn("failed to remove file")
	}
}

func fileExist(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// DeployID retrieves the deployment ID
func (s *server) DeployID(ctx context.Context, d *api.DeployIDRequest) (*api.DeployIDResponse, error) {
	if s.deployment == nil {
		return nil, ErrorNoDeployment
	}

	return &api.DeployIDResponse{
		DeploymentId: s.deployment.ID,
	}, nil
}

// This function isn't perfect. It's intent is that you can wait for the lock with
// a timeout. In reality, this is not possible using mutexes. So instead, it errors
// out if the context is expired, and then waits for the lock. This is fine because
// the next use of the context will error out and then the mutex will be unlocked
// fairly quickly because of that. We just end up with an extra thread waiting
// for the lock on the server.
func (s *server) acquireLock(ctx context.Context) error {
	select {
	case <-ctx.Done():
		if ctx.Err() != nil {
			return status.Errorf(codes.DeadlineExceeded,
				"deadline exceeded waiting for deployment lock: %s", ctx.Err())
		}
		return nil
	default:
		s.deployment.Lock()
		return nil
	}
}

func (s *server) reloadBackupRunner() error {
	// platformConfig knows how to deal with superuser, and external vs internal PG
	platformConfig := platform_config.Config{
		Config: &papi.Config{
			Service: &papi.Config_Service{
				// NOTE (jaym): The below hack is to allow deployment-service to find the
				// root cert for external postgres.
				// deployment-service doesn't fully participate in the platform config, and
				// so it does not get the external postgres root cert automatically
				Path: "/hab/svc/automate-pg-gateway",
			},
			Postgresql: &papi.Config_Postgresql{
				// FIXME (jaym): GetHost is deprecated. This is always localhost
				Ip: s.deployment.Config.GetPgGateway().GetV1().GetSys().GetService().GetHost().GetValue(),
				Cfg: &papi.Config_Postgresql_Cfg{
					Port: int64(s.deployment.Config.GetPgGateway().GetV1().GetSys().GetService().GetPort().GetValue()),
				},
			},
			Platform: &papi.Config_Platform{
				ExternalPostgresql: s.deployment.Config.GetGlobal().GetV1().GetExternal().GetPostgresql(),
			},
		},
	}

	pgConnInfo, err := pg.SuperuserConnInfoFromPlatformConfig(&platformConfig)
	if err != nil {
		return err
	}

	configRenderer, err := s.configRenderer()
	if err != nil {
		return err
	}

	locationSpec, err := s.backupGatewayLocationSpec()
	if err != nil {
		return err
	}

	var builderMinioLocationSpec backup.LocationSpecification
	if deployment.ContainsCollection(s.deployment.Config.GetDeployment(), services.BuilderCollectionName) {
		var err error
		builderMinioLocationSpec, err = s.builderMinioLocationSpec()
		if err != nil {
			return err
		}
	}

	esSidecarInfo := backup.ESSidecarConnInfo{
		Host: "127.0.0.1",
		Port: s.deployment.Config.GetEsSidecar().GetV1().GetSys().GetService().GetPort().GetValue(),
	}

	target := target.NewLocalTarget(airgap.AirgapInUse())

	if s.backupRunner == nil {
		s.backupRunner = backup.NewRunner()
	}

	s.backupRunner.Configure(
		backup.WithConfigRenderer(configRenderer),
		backup.WithConnFactory(s.connFactory),
		backup.WithSpecs(backup.DefaultSpecs(s.deployment.ServiceNames())),
		backup.WithBackupLocationSpecification(locationSpec),
		backup.WithPGConnInfo(pgConnInfo),
		backup.WithEsSidecarInfo(esSidecarInfo),
		backup.WithConnFactory(s.connFactory),
		backup.WithTarget(target),
		backup.WithReleaseManifest(s.deployment.CurrentReleaseManifest),
		backup.WithDeploymentStore(s.deploymentStore),
		backup.WithBuilderMinioLocationSpec(builderMinioLocationSpec),
	)

	return nil
}
