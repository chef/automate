// Copyright © 2017 Chef Software

// Package target encapsulates all the ways we will interact with deployment
// targets, starting with a client/target to control the hab supervisor locally
package target

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/sys"

	"github.com/chef/automate/lib/io/fileutils"
)

// LocalTarget struct
type LocalTarget struct {
	HabCmd
	Executor   command.Executor
	HabClient  *habapi.Client
	habSup     HabSup
	HabBaseDir string
	HabBackoff time.Duration
}

const HabitatInstallScriptURL = "https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh"

var defaultHabBackoff = 500 * time.Millisecond
var defaultHabDir = "/hab"
var habUser = "hab"
var habGroup = "hab"

var systemdUnitPath = "/usr/lib/systemd/system/"
var habSupUnitName = "chef-automate.service"

// We must clean up the PID files in ExecStartPre as hab sup term currently does not
// See https://github.com/habitat-sh/habitat/issues/5045
var systemdUnitTemplate = `
[Unit]
Description = The Habitat Supervisor for Chef Automate

[Service]
Type = simple
PIDFile =/hab/sup/default/LOCK
ExecStartPre=/bin/sh -c 'rm -f /hab/svc/*/PID'
ExecStart = %s sup run --no-color --listen-http 127.0.0.1:9631 --listen-gossip 127.0.0.1:9638
ExecStop =/bin/sh -c 'kill -s USR1 $(cat /hab/svc/deployment-service/PID) || true; tail -f --pid=$MAINPID /dev/null || true'
TimeoutStopSec=200s
Restart = on-failure
LimitNOFILE = 65536
LimitMEMLOCK=infinity
UMask = 0022
Environment = "HAB_SUP_BINARY=%s"
Environment = "HAB_LAUNCH_BINARY=%s"
%s

[Install]
WantedBy = default.target
`

var habStatusRetryWait = 1 * time.Second

// The following interfaces are used to allow us to inject mock versions of these
// in the unit tests.
var defaultTempFileProvider tempFileProvider = &ioutilTempFileProvider{}
var defaultUserLookupProvider userLookupProvider = &osUserLookupProvider{}

type tempFileProvider interface {
	TempFile(string, string) (*os.File, error)
}

type ioutilTempFileProvider struct{}

func (p *ioutilTempFileProvider) TempFile(dir string, prefix string) (*os.File, error) {
	return ioutil.TempFile(dir, prefix)
}

type userLookupProvider interface {
	Lookup(username string) (*user.User, error)
	LookupGroup(groupname string) (*user.Group, error)
	LookupGroupId(gid string) (*user.Group, error)
}

type osUserLookupProvider struct{}

func (o *osUserLookupProvider) Lookup(username string) (*user.User, error) {
	return user.Lookup(username)
}

func (o *osUserLookupProvider) LookupGroup(groupname string) (*user.Group, error) {
	return user.LookupGroup(groupname)
}

func (o *osUserLookupProvider) LookupGroupId(gid string) (*user.Group, error) {
	return user.LookupGroupId(gid)
}

// NewLocalTarget creates a new local target
func NewLocalTarget(offlineMode bool) *LocalTarget {
	c := command.NewExecExecutor()
	// TODO(ssd) 2018-01-25: Make at least the port configurable
	client := habapi.New("http://127.0.0.1:9631")
	habSup := LocalHabSup(client)
	return &LocalTarget{
		HabCmd:     NewHabCmd(c, offlineMode),
		HabBaseDir: defaultHabDir,
		HabClient:  client,
		habSup:     habSup,
		HabBackoff: defaultHabBackoff,
		Executor:   c,
	}
}

func (t *LocalTarget) habUserDir() string {
	return filepath.Join(t.HabBaseDir, "user")
}

func (t *LocalTarget) habPkgDir() string {
	return filepath.Join(t.HabBaseDir, "pkgs")
}

func (t *LocalTarget) deploymentServiceDataDir() string {
	return filepath.Join(t.HabBaseDir, "svc", "deployment-service", "data")
}

func (t *LocalTarget) habTmpDir() string {
	return filepath.Join(t.HabBaseDir, "tmp")
}

func (t *LocalTarget) CommandExecutor() command.Executor {
	return t.Executor
}

func (t *LocalTarget) HabAPIClient() *habapi.Client {
	return t.HabClient
}

func (t *LocalTarget) HabSup() HabSup {
	return t.habSup
}

func (t *LocalTarget) InstallHabitat(m manifest.ReleaseManifest, writer cli.BodyWriter) error {
	found, requiredVersion := m.PackageForServiceName("hab")
	if !found {
		return errors.New("could not find hab in release manifest")
	}

	writer.Bodyf("Installing Habitat %s", habpkg.VersionString(&requiredVersion))
	if t.habBinaryInstalled() {
		err := t.installHabViaHab(requiredVersion)
		if err != nil {
			return err
		}
	} else {
		err := t.installHabViaInstallScript(&requiredVersion)
		if err != nil {
			return err
		}
	}

	output, err := t.HabCmd.BinlinkPackage(&requiredVersion, "hab")
	if err != nil {
		ident := habpkg.Ident(&requiredVersion)
		logrus.Debugf("Binlink of %s failed with output: %s", ident, output)
		logrus.Warnf("Could not binlink %q, some hab commands may not work", ident)
	}

	return nil
}

func (t *LocalTarget) InstallDeploymentService(c *dc.ConfigRequest, m manifest.ReleaseManifest) error {
	pkg := manifest.InstallableFromManifest(m, "deployment-service")
	if pkg == nil {
		return errors.New("deployment-service was not found in the manifest")
	}

	return t.InstallService(pkg, c.GetV1().GetSvc().GetChannel().GetValue())
}

func (t *LocalTarget) SetupSupervisor(config *dc.ConfigRequest, m manifest.ReleaseManifest, writer cli.FormatWriter) error {
	if err := t.InstallSupPackages(m, writer); err != nil {
		return err
	}

	if err := t.installHabComponents(m, writer); err != nil {
		return err
	}

	if os.Getenv("CHEF_AUTOMATE_SKIP_SYSTEMD") != "true" {
		if err := t.addHabSupToSystemd(m, config, writer); err != nil {
			return err
		}
	}

	if err := t.EnsureHabUser(writer); err != nil {
		return err
	}

	if os.Getenv("CHEF_AUTOMATE_SKIP_SYSTEMD") != "true" {
		if err := t.startHabSupFromSystemd(writer); err != nil {
			return err
		}
	} else {
		if err := t.startHabSupFromLauncher(m, writer); err != nil {
			return err
		}
	}

	return t.waitForHabSupToStart(m)
}

func (t *LocalTarget) LoadDeploymentService(svc habpkg.VersionedPackage) error {
	if svc.Name() != "deployment-service" {
		logrus.Fatal("Invalid package name")
	}
	if !habpkg.IsFullyQualified(svc) {
		return errors.New("Expected fully qualified deployment service package")
	}
	return t.LoadService(svc)
}

func (t *LocalTarget) DeployDeploymentService(config *dc.ConfigRequest, m manifest.ReleaseManifest, writer cli.BodyWriter) error {
	pkg := manifest.InstallableFromManifest(m, "deployment-service")
	if pkg == nil {
		return errors.New("deployment-service was not found in the manifest")
	}

	deploymentSysConfig, err := toml.Marshal(config.V1.Sys)
	if err != nil {
		return errors.Wrap(err, "could not render deployment service config")
	}

	writer.Body("Configuring deployment-service")
	err = t.SetUserToml("deployment-service", string(deploymentSysConfig))
	if err != nil {
		return err
	}

	writer.Body("Starting deployment-service")
	err = t.UnloadService(pkg)
	if err != nil {
		return err
	}

	err = t.LoadService(pkg)
	return err
}

// Modify environment to ensure we are using a version of hab
// and hab-sup from the given manifest.
//
// We do this for the backwards compatibility path.
func (t *LocalTarget) SetHabitatEnvironment(m manifest.ReleaseManifest) error {
	habSupPath, err := t.getHabSupBin(m)
	if err != nil {
		return err
	}

	logrus.Debugf("Setting HAB_SUP_BINARY=%s", habSupPath)
	os.Setenv("HAB_SUP_BINARY", habSupPath)

	habPath, err := t.getHabBin(m)
	if err != nil {
		return err
	}

	habPath = filepath.Dir(habPath)
	existingPath := os.Getenv("PATH")

	var newPath string
	if existingPath != "" {
		newPath = strings.Join([]string{habPath, existingPath}, ":")
	} else {
		newPath = habPath
	}

	logrus.Debugf("Setting PATH=%s", newPath)
	return os.Setenv("PATH", newPath)
}

// InitServiceConfig lays down config for an automate service
func (t *LocalTarget) SetUserToml(name, config string) error {
	logrus.Debugf("Configuring Chef Automate package: %s", name)
	dirName := filepath.Join(t.habUserDir(), name, "config")
	if err := os.MkdirAll(dirName, sys.DefaultDirPerms); err != nil {
		return errors.Wrap(err, "Could not create config dir "+dirName)
	}
	filePath := filepath.Join(dirName, "user.toml")
	file, err := os.OpenFile(filePath, os.O_RDWR|os.O_CREATE|os.O_TRUNC, sys.DefaultFilePerms)
	if err != nil {
		return errors.Wrapf(err, "Error creating config file %s", filePath)
	}
	defer func() {
		closeErr := file.Close()
		if closeErr != nil {
			logrus.WithError(closeErr).WithFields(logrus.Fields{
				"path": filePath,
			}).Error("failed to close user.toml")
		}
	}()

	_, err = file.Write([]byte(config))
	if err != nil {
		return errors.Wrapf(err, "Error writing config file for service %s", name)
	}

	return nil
}

// InstallService installs an automate service. Returns an error if
// the install failed for any reason.
func (t *LocalTarget) InstallService(svc habpkg.Installable, channel string) error {
	return t.installPackage(svc, channel)
}

// unloadServiceWithHabVersion unloads the given service with a
// particular version of hab and hab-sup. This is used during the
// Habitat upgrade process to unload services before upgrading hab.
func (t *LocalTarget) unloadServiceWithHabVersion(svc habpkg.VersionedPackage, binPkg habpkg.HabPkg, habSupPkg habpkg.HabPkg) error {
	svcIdent := habpkg.Ident(svc)

	habSupBin, err := t.getBinPath(habSupPkg, "hab-sup")
	if err != nil {
		return errors.Wrap(err, "could not retrieve path of require hab-sup binary")
	}

	output, err := t.Executor.CombinedOutput("hab",
		command.Args("pkg", "exec", habpkg.Ident(&binPkg),
			"hab", "svc", "unload", svcIdent),
		command.Timeout(HabTimeoutDefault),
		command.Envvar("HAB_SUP_BINARY", habSupBin))
	if err != nil {
		logrus.WithError(err).WithFields(logrus.Fields{
			"package": svcIdent,
			"output":  output,
		}).Error("unload failed")
		return errors.Wrapf(err, "failed to unload service %s", svcIdent)
	}

	return nil
}

// RemoveService removes the given service from this target. It
// returns an error if any portion of the removal fails.
func (t *LocalTarget) RemoveService(svc habpkg.VersionedPackage) error {
	ident := habpkg.Ident(svc)

	// A malformed ident would likely cause the sup unload to fail
	// and yield a dangerous situation in the package removal
	// below, so we check for it early.
	if strings.HasSuffix(ident, "/") {
		logrus.WithFields(logrus.Fields{
			"package": ident,
		}).Error("invalid package identifier")
		return errors.Errorf("cannot remove service with invalid identifier %s", ident)
	}

	err := t.UnloadService(svc)
	if err != nil {
		return err
	}

	packageContentPath := fmt.Sprintf("%s/%s", t.habPkgDir(), ident)
	err = os.RemoveAll(packageContentPath)
	if err != nil {
		logrus.WithError(err).WithFields(logrus.Fields{
			"package": ident,
		}).Error("package removal failed")
		return errors.Wrapf(err, "failed to remove package contents for service %s", ident)
	}

	return nil
}

func supplementaryPackages() ([]habpkg.HabPkg, error) {
	return services.SupplementaryPackagesInCollection("automate-full")
}

// InstallSupPackages installs non-service Habitat packages included
// in automate-deployment's data/services.json
func (t *LocalTarget) InstallSupPackages(releaseManifest manifest.ReleaseManifest, writer cli.BodyWriter) error {
	writer.Body("Installing supplementary Habitat packages")
	packages, err := supplementaryPackages()
	if err != nil {
		return err
	}

	for _, id := range packages {
		writer.Bodyf("Installing Habitat package %s", id.Name())
		err = t.installHabPackageFromReleaseManifest(releaseManifest, id.Name())
		if err != nil {
			return err
		}
	}
	return nil
}

func (t *LocalTarget) installHabPackageFromReleaseManifest(releaseManifest manifest.ReleaseManifest, name string) error {
	var p habpkg.HabPkg
	var found bool
	var err error

	found, p = releaseManifest.PackageForServiceName(name)
	if !found {
		// TODO(jaym) 2018-05-03: SHIM so that this can pass unit tests
		// until the change for updating the release manifest lands
		if name == "rsync" {
			p, err = habpkg.FromString("core/rsync")
			if err != nil {
				return errors.Wrap(err, "could not construct habpkg for core/rsync. If you are seeing this error message we have done something very silly")
			}
		} else {
			return errors.Errorf("could not find %s in release manifest", name)
		}
	}
	return t.installPackage(&p, "")
}

func (t *LocalTarget) installPackage(pkg habpkg.Installable, channel string) error {
	logrus.WithFields(logrus.Fields{
		"package": pkg.InstallIdent(),
		"channel": channel,
		"action":  "install",
	}).Debug()

	output, err := t.HabCmd.InstallPackage(pkg, channel)
	if err != nil {
		logrus.WithError(err).WithFields(logrus.Fields{
			"package": pkg.InstallIdent(),
			"output":  output,
		}).Error("install failed")
		return errors.Wrapf(err, "msg=\"failed to install\" package=%s output=%s", pkg.InstallIdent(), output)
	}

	return nil
}

// StartService starts an already loaded service. Service startup is
// asynchronous.
func (t *LocalTarget) StartService(svc habpkg.VersionedPackage) error {
	ident := habpkg.Ident(svc)
	logrus.WithFields(logrus.Fields{
		"package": ident,
		"action":  "start",
	}).Debug()

	output, err := t.HabCmd.StartService(svc)
	if err != nil {
		return errors.Wrapf(err, "Failed to start service %s\nOutput:\n%s",
			ident, output)
	}

	return nil
}

func (t *LocalTarget) StopService(svc habpkg.VersionedPackage) error {
	ident := habpkg.Ident(svc)
	logrus.WithFields(logrus.Fields{
		"package": ident,
		"action":  "stop",
	}).Debug()

	output, err := t.HabCmd.StopService(svc)
	if err != nil {
		return errors.Wrapf(err, "Failed to stop service %s\nOutput:\n%s",
			ident, output)
	}

	return nil
}

// UnloadService unloads the given service from this target. It
// returns an error if any portion of the removal fails. Packages are
// not deleted for unload
func (t *LocalTarget) UnloadService(svc habpkg.VersionedPackage) error {
	ident := habpkg.ShortIdent(svc)
	logrus.WithFields(logrus.Fields{
		"package": ident,
		"action":  "unload",
	}).Debug()

	output, err := t.HabCmd.UnloadService(svc)
	if err != nil {
		return errors.Wrapf(err, "Failed to unload service %s\nOutput:\n%s",
			ident, output)
	}

	return nil
}

// LoadService starts the package in the supervisor, either via load or start
// as appropriate for the startStyle
func (t *LocalTarget) LoadService(svc habpkg.VersionedPackage, opts ...LoadOption) error {
	ident := habpkg.Ident(svc)

	logrus.WithFields(logrus.Fields{
		"pkg": ident,
	}).Info("Loading service")

	output, err := t.HabCmd.LoadService(svc, opts...)
	if err != nil {
		return errors.Wrapf(err, "Failed to load service %s\nOutput:\n%s",
			ident, output)
	}
	// give hab sup a brief pause between load requests
	time.Sleep(t.HabBackoff)
	return nil
}

// Status verifies status of a service by hitting its health check endpoint
func (t *LocalTarget) Status(ctx context.Context, serviceNames []string) *api.ServiceStatus {
	log := func(svc string, health string) {
		logrus.WithFields(logrus.Fields{
			"mod":     "target.LocalTarget.Status",
			"service": svc,
			"health":  health,
		}).Debug()
	}

	now := uint64(time.Now().UTC().Unix())

	svcInfos, err := t.HabClient.ListServices(ctx)
	if err != nil {
		logrus.WithError(err).WithFields(logrus.Fields{
			"mod": "target.LocalTarget.Status",
		}).Error("failed to get service info")
	}

	status := &api.ServiceStatus{}
	for _, svc := range serviceNames {
		state := &api.ServiceState{Name: svc}
		response, err := t.HabClient.ServiceHealth(ctx, svc, "default")
		if err != nil {
			state.State = api.ServiceState_CONNECTION_ERROR
			status.Add(state)
			log(svc, "connection_error")
			continue
		}
		switch response {
		case habapi.StatusOk:
			state.State = api.ServiceState_OK
		case habapi.StatusWarning:
			state.State = api.ServiceState_WARNING
		case habapi.StatusCritical:
			state.State = api.ServiceState_CRITICAL
		case habapi.StatusUnknown:
			state.State = api.ServiceState_UNKNOWN
		case habapi.StatusDown:
			state.State = api.ServiceState_DOWN
		default:
			state.State = api.ServiceState_UNKNOWN
		}

		svcInfo, found := habapi.ServiceInfoByName(svcInfos, svc)
		if found {
			state.Pid = svcInfo.Process.PID
			state.Uptime = now - svcInfo.Process.TimeStateEntered
		}

		if state.Pid == 0 {
			if state.State == api.ServiceState_OK {
				logrus.Warnf("service %s has PID 0 with state OK", svc)
				state.State = api.ServiceState_UNKNOWN
			}
		}

		status.Add(state)
	}
	return status
}

// DeployedServices returns a list of all the services the supervisor is supervising
func (t *LocalTarget) DeployedServices(ctx context.Context) (map[string]DeployedService, error) {
	services, err := t.HabClient.ListServices(ctx)
	if err != nil {
		return nil, err
	}

	deployedServices := make(map[string]DeployedService, len(services))
	for _, svc := range services {
		deployedSvc := DeployedService{
			Pkg: habpkg.NewFQ(svc.Pkg.Origin, svc.Pkg.Name, svc.Pkg.Version, svc.Pkg.Release),
			//TODO(jaym) Binds needs to be a []Bind type. We're throwing away the
			//           service group part of the binding.
			Binds:               bindsLValues(svc.Binds),
			UpdateStrategy:      svc.UpdateStrategy,
			DesiredProcessState: parseDesiredState(svc.DesiredState),
		}

		existingSvc, alreadyAdded := deployedServices[svc.Pkg.Name]
		if alreadyAdded {
			logrus.WithFields(logrus.Fields{
				"service_name":   svc.Pkg.Name,
				"new_entry":      deployedSvc,
				"existing_entry": existingSvc,
			}).Warn("Found 2 services with same name (possibly the result of a recent reload)")
			deployedServices[svc.Pkg.Name] = preferredDeployedService(existingSvc, deployedSvc)
		} else {
			deployedServices[svc.Pkg.Name] = deployedSvc
		}

	}

	return deployedServices, nil
}

func parseDesiredState(s string) DesiredProcessState {
	switch strings.ToLower(s) {
	case "up":
		return ProcessStateUp
	case "down":
		return ProcessStateDown
	default:
		return ProcessStateUnknown
	}
}

// preferredDeployedService returns whichever of the two passed
// services are in the Up state, preferring the first service if both
// or neither of the two services is up. The goal of this is to
// account for the fact that just after a reload, Habitat can report
// both the old and new service:
//
// https://github.com/habitat-sh/habitat/issues/5317
//
func preferredDeployedService(svcA, svcB DeployedService) DeployedService {
	if svcA.DesiredProcessState == ProcessStateUp {
		return svcA
	}

	if svcB.DesiredProcessState == ProcessStateUp {
		return svcB
	}

	return svcA
}

func bindsLValues(binds []string) []string {
	if binds == nil {
		return []string{}
	}
	lvalues := make([]string, len(binds))

	for i, b := range binds {
		lvalues[i] = strings.Split(b, ":")[0]
	}
	return lvalues
}

func (t *LocalTarget) HabSupRestartRequired(desiredPkg habpkg.HabPkg) (bool, error) {
	runningHabVersion, err := t.habSup.SupPkg()
	if err != nil {
		return false, errors.Wrap(err, "determining running hab-sup version")
	}

	restartRequired := !habpkg.GreaterOrEqual(&runningHabVersion, &desiredPkg)
	logrus.WithFields(logrus.Fields{
		"restart_required": restartRequired,
		"desired_version":  habpkg.VersionString(&desiredPkg),
		"actual_version":   habpkg.VersionString(&runningHabVersion),
	}).Debug("RestartRequired check")

	return restartRequired, nil
}

// HabSupRestart restarts the Habitat Supervisor
//
// The list of services is used a mitigation which attempts to avoid
// Habitat sending a SIGKILL to data services. The features described
// at
//
//  https://github.com/habitat-sh/habitat/issues/5162
//  https://github.com/habitat-sh/habitat/issues/5135
//
// would help us avoid this.
//
// Note, when we move to multiple nodes, this mitigation will need to
// change a bit since right now this assumes all clients of postgresql
// & ES are on the same machine.
//
func (t *LocalTarget) HabSupRestart(sortedServiceList []string) (bool, error) {
	habSupP, err := t.habSup.SupPkg()
	if err != nil {
		return false, errors.Wrap(err, "determining running hab-sup version")
	}

	if SupportsSupHup(habSupP) {
		if err := t.habSup.Hup(context.Background()); err != nil {
			return false, errors.Wrap(err, "Failed to send sighup")
		}
		return false, nil
	}

	services, err := t.HabClient.ListServices(context.Background())
	if err != nil {
		return false, errors.Wrap(err, "querying running services")
	}

	supPkg, err := t.habSup.SupPkg()
	if err != nil {
		// TODO(ssd) 2018-06-13: Should we just try with
		// whatever hab we have, or maybe skip the mitigation
		// in this case?
		return false, errors.Wrap(err, "could not fetch hab-sup version to ensure compatible unload calls")
	}

	// Create a core/hab package with the same version of the
	// supervisor. This assumes we will generally be bumping hab
	// and hab-sup at the same time. Typically, we think this
	// version of hab should be installed, but just in case we
	// attempt an install.
	binPkg := habpkg.NewWithVersion("core", "hab", supPkg.Version())
	err = t.installHabViaHab(binPkg)
	if err != nil {
		logrus.WithError(err).Warn("failed to install hab version required for unload")
	}

	// Assume that the sortedServiceList is in bind-dep-sorted order, so
	// we want to stop things in the opposite order.
	for i := len(sortedServiceList) - 1; i >= 0; i-- {
		svcName := sortedServiceList[i]
		if svcName == "deployment-service" {
			// Don't unload ourselves, yet
			continue
		}

		if svcName == "automate-cli" {
			// Skip non-service to avoid spurious warning
			continue
		}

		svcInfo, found := habapi.ServiceInfoByName(services, svcName)
		if !found {
			logrus.WithField("service", svcName).Warn("Could not find service in hab-sup service info, not unloading")
			continue
		}

		svc := habpkg.New(svcInfo.Pkg.Origin, svcInfo.Pkg.Name)
		logrus.WithField("service", svcName).Info("Unloading service before supervisor shutdown")
		err := t.unloadServiceWithHabVersion(&svc, binPkg, supPkg)
		if err != nil {
			logrus.WithFields(logrus.Fields{
				"svc":   habpkg.Ident(&svc),
				"error": err,
			}).Warn("Unable to unload service")
		}

		// UnloadService is currently async. Here we wait for 10
		// seconds for the service to unload. The habitat launcher
		// will kill services after 8 seconds, so by 10 seconds it is
		// likely the last service has been handled.
		//
		//  https://github.com/habitat-sh/habitat/blob/85fcde682ca2bab01e9174bb75cc5364319c7369/components/launcher/src/sys/unix/service.rs#L47-L86
		//
		err = t.waitForUnload(svcName, 10*time.Second)
		if err != nil {
			logrus.WithError(err).WithField("service", svcName).Warn("Failed waiting for service to unload")
		}
	}

	err = t.Executor.Run("systemctl", command.Args("restart", "chef-automate.service"))
	return true, errors.Wrap(err, "failed to restart habitat supervisor")
}

func (t *LocalTarget) waitForUnload(name string, timeout time.Duration) error {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	for {
		_, err := t.HabClient.ServiceInfo(ctx, name, "default")
		if err == habapi.ErrServiceNotFound {
			return nil
		}

		if err != nil {
			return err
		}
		time.Sleep(500 * time.Millisecond)
	}
}

// SystemdRunning returns true if PID 1 is systemd
func (t *LocalTarget) SystemdRunning() (bool, error) {
	content, err := ioutil.ReadFile("/proc/1/comm")
	if os.IsNotExist(err) {
		// If we don't have proc assume that we aren't on systemd
		return false, nil
	}

	if err != nil {
		return false, errors.Wrap(err, "reading procfs for PID 1")
	}

	return bytes.HasPrefix(content, []byte("systemd")), nil
}

// SystemdReloadRequired returns true if a reload is required.
//
// It looks like we should be able to call `systemctl show
// chef-chef-automate.service --property NeedDaemonReload` instead,
// but that seems to report 'no' even when status reports a reload is
// required.
func (t *LocalTarget) SystemdReloadRequired() (bool, error) {
	stderrBuff := new(strings.Builder)
	err := t.Executor.Run("systemctl", command.Args("status", "chef-automate.service"), command.Stderr(stderrBuff))
	if err != nil {
		return false, errors.Wrapf(err, "systemctl status chef-automate.service failed: %s", stderrBuff.String())
	}

	return strings.Contains(stderrBuff.String(), "changed on disk"), nil
}

// SystemdReload calls systemctl daemon-reload on the target
func (t *LocalTarget) SystemdReload() error {
	return t.Executor.Run("systemctl", command.Args("daemon-reload"))
}

// Stop stops the A2 services
func (t *LocalTarget) Stop() error {
	logrus.Info("Calling hab sup term")
	_, err := t.Executor.Start("hab", command.Args("sup", "term"))
	if err != nil {
		return errors.Wrap(err, "failed to stop Chef Automate via hab sup term")
	}

	return nil
}

func (t *LocalTarget) EnsureStopped() error {
	_, lastErr := t.Executor.CombinedOutput("systemctl", command.Args("status", habSupUnitName))
	if lastErr == nil {
		_, err := t.Executor.CombinedOutput("systemctl", command.Args("stop", habSupUnitName))
		return err
	}
	return nil
}

func (t *LocalTarget) Disable() error {
	out, err := t.Executor.CombinedOutput("systemctl", command.Args("disable", "chef-automate.service"))
	ctxLog := logrus.WithFields(logrus.Fields{
		"command": "systemctl disable chef-automate.service",
		"output":  out,
	})
	if err != nil {
		ctxLog.WithError(err).Error("failed to disable Chef Automate via systemd")
		return errors.Wrap(err, "failed to disable Chef Automate via systemd disable chef-automate.service")
	} else {
		ctxLog.Info("disabled Chef Automate via systemd")
	}

	habSupUnitPath := filepath.Join(systemdUnitPath, habSupUnitName)
	ctxLog = logrus.WithFields(logrus.Fields{
		"path": habSupUnitPath,
	})
	err = os.Remove(habSupUnitPath)
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove hab supervisor systemd unit file")
		return errors.Wrapf(err, "failed to delete systemd unit file at %s", habSupUnitPath)
	} else {
		ctxLog.Info("removed hab supervisor systemd unit file")
	}
	return nil
}

func (t *LocalTarget) EnsureDisabled() error {
	_, err := t.Executor.CombinedOutput("systemctl", command.Args("is-enabled", habSupUnitName))
	if err == nil {
		return t.Disable()
	}
	return nil
}

func (t *LocalTarget) DestroySupervisor() error {
	//  rm -rf /hab/sup
	habSupPath := "/hab/sup"
	ctxLog := logrus.WithFields(logrus.Fields{
		"path": habSupPath,
	})
	err := os.RemoveAll(habSupPath)
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove hab supervisor files")
		return errors.Wrapf(err, "failed to delete hab supervisor files at %s", habSupPath)
	} else {
		ctxLog.Info("removed hab supervisor files")
	}

	// rm -rf /hab/user
	habUserPath := "/hab/user"
	ctxLog = logrus.WithFields(logrus.Fields{
		"path": habUserPath,
	})
	err = os.RemoveAll(habUserPath)
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove hab user configuration")
		return errors.Wrapf(err, "failed to delete hab user files at %s", habUserPath)
	} else {
		ctxLog.Info("removed hab user configuration")
	}

	// userdel hab || true
	return t.ensureHabUserRemoved()
}

func (t *LocalTarget) DestroyData() error {
	// rm -rf /hab/svc
	habSvcPath := "/hab/svc"
	ctxLog := logrus.WithFields(logrus.Fields{
		"path": habSvcPath,
	})
	err := os.RemoveAll(habSvcPath)
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove hab service data")
		return errors.Wrapf(err, "failed to delete hab service data at %s", habSvcPath)
	} else {
		ctxLog.Info("removed hab service data")
	}
	return nil
}

func (t *LocalTarget) DestroyPkgCache() error {
	//  rm -rf /hab
	habPath := "/hab"
	ctxLog := logrus.WithFields(logrus.Fields{
		"path": habPath,
	})
	err := os.RemoveAll(habPath)
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove hab files")
		return errors.Wrapf(err, "failed to delete hab files at %s", habPath)
	} else {
		ctxLog.Info("removed hab files")
	}
	return nil
}

// GetUserToml reads the user toml for the given package. If it does not
// exist, an empty string is returned
func (t *LocalTarget) GetUserToml(pkg habpkg.VersionedPackage) (string, error) {
	filePath := filepath.Join(t.habUserDir(), pkg.Name(), "config", "user.toml")
	_, err := os.Stat(filePath)

	if err != nil {
		if os.IsNotExist(err) {
			return "", nil
		}
	}

	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return "", err
	}

	return string(data), nil
}

func (t *LocalTarget) IsBinlinked(pkg habpkg.VersionedPackage, cmd string) (bool, error) {
	binlinkLoc := path.Join("/bin", cmd)
	expectedPrefix := path.Join(t.habPkgDir(), pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	isLink, err := fileutils.IsSymlink(binlinkLoc)
	if err != nil {
		if os.IsNotExist(err) {
			return false, nil
		}
		return false, err
	}

	if !isLink {
		return false, nil
	}

	path, err := os.Readlink(binlinkLoc)
	if err != nil {
		return false, err
	}

	isBinlink := strings.HasPrefix(path, expectedPrefix)
	logrus.WithFields(logrus.Fields{
		"expected_prefix": expectedPrefix,
		"resolved_path":   path,
		"is_binlink":      isBinlink,
	}).Debug("checking binlink")

	return isBinlink, nil
}

func (t *LocalTarget) SymlinkHabSup(habSupP habpkg.HabPkg) error {
	habSupLink := t.habSupSymlinkPath()
	runtimeDir := path.Dir(habSupLink)
	if err := os.MkdirAll(runtimeDir, 0755); err != nil {
		return errors.Wrap(err, "failed to create deployment-service runtime directory")
	}

	habSupBinPath, err := t.getBinPath(habSupP, "hab-sup")
	if err != nil {
		return errors.Wrap(err, "hab-sup binary lookup")
	}

	if err := fileutils.AtomicSymlink(habSupBinPath, habSupLink); err != nil {
		return errors.Wrap(err, "could not create hab-sup symlink")
	}

	return nil
}

func (t *LocalTarget) GetSymlinkedHabSup() (habpkg.HabPkg, error) {
	habSupPath, err := os.Readlink(t.habSupSymlinkPath())
	if err != nil {
		// Do not wrap any PathErrors. They should be passed as is
		// because they need the be handled in a special way
		return habpkg.HabPkg{}, err
	}

	relPath, err := filepath.Rel("/hab/pkgs/", habSupPath)
	if err != nil {
		return habpkg.HabPkg{}, errors.Wrapf(err, "could not get relative path of hab pkg")
	}
	pkgIdent := strings.TrimSuffix(relPath, "/bin/hab-sup")
	return habpkg.FromString(pkgIdent)
}

func (t *LocalTarget) RenderAutomateUnitFile(proxyConfig string, habP habpkg.HabPkg, habLauncherP habpkg.HabPkg) (string, error) {
	habBin, err := t.getBinPath(habP, "hab")
	if err != nil {
		return "", errors.Wrap(err, "hab binary lookup")
	}

	habLauncherBin, err := t.getBinPath(habLauncherP, "hab-launch")
	if err != nil {
		return "", errors.Wrap(err, "hab-launch binary lookup")
	}

	return fmt.Sprintf(systemdUnitTemplate, habBin, t.habSupSymlinkPath(), habLauncherBin, proxyConfig), nil
}

func (t *LocalTarget) WriteAutomateUnitFile(content []byte) error {
	if err := os.MkdirAll(systemdUnitPath, os.ModePerm); err != nil {
		return err
	}

	return ioutil.WriteFile(filepath.Join(systemdUnitPath, habSupUnitName), content, 0644)
}

func (t *LocalTarget) GetAutomateUnitFile() ([]byte, error) {
	return ioutil.ReadFile(filepath.Join(systemdUnitPath, habSupUnitName))
}

func (t *LocalTarget) habSupSymlinkPath() string {
	return path.Join(t.deploymentServiceDataDir(), "runtime", "hab-sup")
}

func (t *LocalTarget) InstallAutomateUnitFile(config *dc.ConfigRequest, habP habpkg.HabPkg, habSupP habpkg.HabPkg, habLauncherP habpkg.HabPkg) error {
	if err := t.SymlinkHabSup(habSupP); err != nil {
		return err
	}

	content, err := t.RenderAutomateUnitFile(config.SystemdProxyConfig(), habP, habLauncherP)
	if err != nil {
		return err
	}
	return t.WriteAutomateUnitFile([]byte(content))
}

func (t *LocalTarget) addHabSupToSystemd(releaseManifest manifest.ReleaseManifest, config *dc.ConfigRequest, writer cli.BodyWriter) error {
	writer.Body("Installing Habitat systemd unit")
	found, habP := releaseManifest.PackageForServiceName("hab")
	if !found {
		return errors.New("could not find hab in release manifest")
	}

	found, habSupP := releaseManifest.PackageForServiceName("hab-sup")
	if !found {
		return errors.New("could not find hab-sup in release manifest")
	}

	found, habLauncherP := releaseManifest.PackageForServiceName("hab-launcher")
	if !found {
		return errors.New("could not find hab-sup in release manifest")
	}
	return t.InstallAutomateUnitFile(config, habP, habSupP, habLauncherP)
}

// getHabSupBin returns the full path to the habSup command for the
// given release manifest
func (t *LocalTarget) getHabSupBin(releaseManifest manifest.ReleaseManifest) (string, error) {
	found, habP := releaseManifest.PackageForServiceName("hab-sup")
	if !found {
		return "", errors.New("could not find hab-sup in release manifest")
	}
	return t.getBinPath(habP, "hab-sup")
}

func (t *LocalTarget) getHabBin(releaseManifest manifest.ReleaseManifest) (string, error) {
	found, p := releaseManifest.PackageForServiceName("hab")
	if !found {
		return "", errors.New("could not find hab in release manifest")
	}
	return t.getBinPath(p, "hab")
}

// getBinPath returns the full path to a command contained inside a
// Habitat package, based on the version of that package in the
// release manifest. Note, this currently assumes all binaries exist
// in the bin/ directory.
func (t *LocalTarget) getBinPath(pkg habpkg.HabPkg, bin string) (string, error) {
	pkgIdent := habpkg.Ident(&pkg)
	output, err := t.Executor.Output("hab", command.Args("pkg", "path", pkgIdent), command.Timeout(HabTimeoutIsInstalled))
	if err != nil {
		return "", errors.Wrapf(err, "failed looking up package path from %q", pkgIdent)
	}

	basePath := strings.TrimSpace(output)
	candidatePath := filepath.Join(basePath, "bin", bin)
	_, err = os.Stat(candidatePath)
	if err != nil {
		return "", errors.Wrapf(err, "failed looking for candidate binary %q", candidatePath)
	}

	return candidatePath, err
}

func (t *LocalTarget) startHabSupFromSystemd(writer cli.BodyWriter) error {
	writer.Body("Starting Habitat with systemd")
	_, err := t.Executor.CombinedOutput("systemctl", command.Args("daemon-reload"))
	if err != nil {
		return errors.Wrap(err, "failed to reload systemd daemon")
	}

	_, err = t.Executor.CombinedOutput("systemctl", command.Args("enable", habSupUnitName))
	if err != nil {
		return errors.Wrap(err, "failed to enable chef-automate systemd unit")
	}

	_, err = t.Executor.CombinedOutput("systemctl", command.Args("start", habSupUnitName))
	if err != nil {
		return errors.Wrap(err, "failed to start chef-automate systemd unit")
	}

	return nil
}

func (t *LocalTarget) startHabSupFromLauncher(m manifest.ReleaseManifest, writer cli.BodyWriter) error {
	var err error

	writer.Body("Starting Habitat with hab-launcher")

	habBin, err := t.getHabBin(m)
	if err != nil {
		return err
	}

	habSupBin, err := t.getHabSupBin(m)
	if err != nil {
		return err
	}

	_, err = t.Executor.Start(habBin,
		command.Args("run", "--listen-http", "127.0.0.1:9631", "--listen-gossip", "127.0.0.1:9638"),
		command.PipeTo("/hab/sup/default/sup.log"),
		command.Envvar("HAB_SUP_BINARY", habSupBin),
	)

	if err != nil {
		return errors.Wrap(err, "failed to start habitat supervisor")
	}

	return nil
}

// waitForHabSupToStart waits for the supervisor status command to
// complete successfully.  There are two cases we are trying to handle with this loop:
//
// - systemctl will return a 0 exit code in some
//   cases of `hab sup run` failures (for example if /bin/hab doesn't
//   exist).
//
// - the main hab-sup process starts, it may take a few moments before
//   the daemon is listening.
//
// TODO(ssd) 2018-06-19: We currently look up the exact path to our
// manifest version of hab and hab-sup in this function since it is
// called from places where we don't have a hab-controlled PATH and
// where we might not be able to set up the correct environment ahead
// of time because the correct versions of hab are not installed yet.
func (t *LocalTarget) waitForHabSupToStart(releaseManifest manifest.ReleaseManifest) error {
	habSupBin, err := t.getHabSupBin(releaseManifest)
	if err != nil {
		return errors.Wrap(err, "failed to find hab-sup binary")
	}

	habBin, err := t.getHabBin(releaseManifest)
	if err != nil {
		return errors.Wrap(err, "failed to find hab binary")
	}

	tries := 0
	var lastErr error
	for {
		if tries >= 5 {
			m := "Habitat supervisor failed to report healthy status; run `journalctl -u chef-automate` for logs"
			return errors.Wrap(lastErr, m)
		}
		tries++
		output, err := t.Executor.CombinedOutput(habBin,
			command.Args("svc", "status"),
			command.Timeout(HabTimeoutDefault),
			command.Envvar("HAB_SUP_BINARY", habSupBin))
		if err != nil {
			lastErr = errors.Wrapf(err, "hab svc status failed with output: %s", output)
			time.Sleep(habStatusRetryWait)
		} else {
			return nil
		}
	}
}

func downloadInstallScript() (*os.File, error) {
	file, err := defaultTempFileProvider.TempFile("", "")
	if err != nil {
		return nil, err
	}
	defer file.Close()

	resp, err := http.Get(HabitatInstallScriptURL)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	_, err = io.Copy(file, resp.Body)
	if err != nil {
		return nil, err
	}
	return file, nil
}

func (t *LocalTarget) habBinaryInstalled() bool {
	_, err := t.Executor.CombinedOutput("bash", command.Args("-c", "command -v hab"))
	return err == nil
}

func (t *LocalTarget) installHabViaHab(requiredVersion habpkg.HabPkg) error {
	return t.installPackage(&requiredVersion, "")
}

func (t *LocalTarget) installHabViaInstallScript(requiredVersion habpkg.VersionedArtifact) error {
	script, downloadErr := downloadInstallScript()
	if downloadErr != nil {
		return downloadErr
	}
	defer os.Remove(script.Name())

	if err := os.MkdirAll(t.habTmpDir(), os.ModePerm); err != nil {
		return err
	}

	output, execErr := t.Executor.CombinedOutput(
		"bash",
		command.Args(script.Name(), "-v", habpkg.VersionString(requiredVersion)),
		command.Envvar("TMPDIR", t.habTmpDir()))
	if execErr != nil {
		return errors.Wrapf(execErr, "Habitat install failed\nOUTPUT:\n%s", output)
	}

	return nil
}

func (t *LocalTarget) installHabComponents(releaseManifest manifest.ReleaseManifest, writer cli.BodyWriter) error {
	pkgs := []string{"hab-sup", "hab-launcher"}
	for _, name := range pkgs {
		writer.Bodyf("Installing Habitat package %s", name)
		err := t.installHabPackageFromReleaseManifest(releaseManifest, name)
		if err != nil {
			return err
		}
	}

	return nil
}

func (t *LocalTarget) reconfigurePendingSentinel() string {
	return filepath.Join(t.deploymentServiceDataDir(), "reconfigure-pending")
}

// SetDeploymentServiceReconfigurePending marks the deployment service
// as needing a reconfiguration
func (t *LocalTarget) SetDeploymentServiceReconfigurePending() error {
	file, err := os.OpenFile(t.reconfigurePendingSentinel(), os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return err
	}
	return file.Close()
}

// GetDeploymentServiceReconfigurePending pending returns true if the
// deployment service needs to be reconfigured. If the state couldn't
// be determined an error is returned.
func (t *LocalTarget) GetDeploymentServiceReconfigurePending() (bool, error) {
	_, err := os.Stat(t.reconfigurePendingSentinel())
	if os.IsNotExist(err) {
		return false, nil
	}

	if err != nil {
		return true, err
	}

	return true, nil
}

// UnsetDeploymentServiceReconfigurePending marks the deployment
// service as no longer needing reconfiguration.
func (t *LocalTarget) UnsetDeploymentServiceReconfigurePending() error {
	err := os.Remove(t.reconfigurePendingSentinel())
	if os.IsNotExist(err) {
		return nil
	}
	return err
}

// EnsureHabUser ensures that the hab user and group exists.
//
// If the hab user and hab group both exist, we return without
// modification (even if the hab group isn't the primary group of
// the hab user.
//
// If the hab group exists but the hab user doesn't, we create the hab
// user and set the hab group as its primary group.
//
// If neither the hab user or hab group exist, we create both the user
// and group.
//
// An error is returned if user or group lookup fails, if a hab user
// exists without a corresponding hab group, or if the useradd command
// fails.
func (t *LocalTarget) EnsureHabUser(writer cli.FormatWriter) error {
	var userExists, groupExists bool

	userInfo, err := defaultUserLookupProvider.Lookup(habUser)
	switch err.(type) {
	case nil:
		userExists = true
	case user.UnknownUserError:
		userExists = false
	default:
		return errors.Wrap(err, "user lookup")
	}

	_, err = defaultUserLookupProvider.LookupGroup(habGroup)
	switch err.(type) {
	case nil:
		groupExists = true
	case user.UnknownGroupError:
		groupExists = false
	default:
		return errors.Wrap(err, "group lookup")
	}

	if userExists && groupExists {
		writer.Body("Skipping user and group creation (both already exist)")
		return nil
	}

	// If the user already exists, but the group doesn't, fail and
	// let the user sort it out rather than modifying an existing
	// user.
	//
	// TODO(ssd) 2018-07-19: Should we handle this case via
	// groupadd + usermod?
	if userExists && !groupExists {
		userGroupDetails := ""
		groupInfo, err := defaultUserLookupProvider.LookupGroupId(userInfo.Gid)
		if err != nil {
			writer.Warnf("Error looking up group information for user %q: %s", habUser, err.Error())
		} else {
			userGroupDetails = fmt.Sprintf(" The user's primary group is %s(%s).", groupInfo.Gid, groupInfo.Name)
		}

		return errors.Errorf("The user %q already exists but no group %q exists.%s Please remove the existing user %q or create a group %q.",
			habUser, habGroup,
			userGroupDetails,
			habUser, habGroup)
	}

	// Useradd options
	//
	// -g GROUP: Use existing group GROUP as the login group
	// -U: Create a group with the same name as the user (even if
	//     USERGROUPS_ENAB is no in /etc/login.defs)
	//
	useraddArgs := []string{habUser}
	andGroup := ""
	if groupExists {
		// TODO(ssd) 2018-07-19: Should we fail here like above instead?
		writer.Warnf("Group %q already exists, new user %q will be added to existing group", habGroup, habUser)
		useraddArgs = append(useraddArgs, "-g", habGroup)
	} else {
		andGroup = " and group"
		useraddArgs = append(useraddArgs, "-U")
	}

	writer.Bodyf("Creating Habitat user%s", andGroup)
	output, err := t.Executor.CombinedOutput("useradd", command.Args(useraddArgs...))
	if err != nil {
		return errors.Wrapf(err, "creating %s user failed: %q", habUser, output)
	}
	return nil
}

func (t *LocalTarget) ensureHabUserRemoved() error {
	if _, err := user.Lookup(habUser); err != nil {
		return nil
	}

	out, err := t.Executor.CombinedOutput("userdel", command.Args(habUser))
	ctxLog := logrus.WithFields(logrus.Fields{
		"command": fmt.Sprintf("userdel %s", habUser),
		"output":  out,
	})
	if err != nil {
		ctxLog.WithError(err).Error("failed to remove habitat user")
		return errors.Wrapf(err, "failed to delete habitat user via userdel %s", habUser)
	} else {
		ctxLog.Info("deleted hab user")
	}
	return nil
}

func (t *LocalTarget) IPs() []net.IP {
	// TODO(ssd) 2018-10-16: A bit of a hack to get us going.
	//
	// AUTOMATE_SYS_IP is set in our service run script and is set
	// to Habitat's sys.ip setting. sys.ip is what is used in most
	// of our configuration templates when looking for a service's
	// IP address. While we could write our own IP lookup
	// routine, until we feed this through everywhere, we would
	// have to be sure it returned the same value as Habitat did.
	//
	// Note, Habitat can return 127.0.0.1 in some cases.
	ipFromEnv := os.Getenv("AUTOMATE_SYS_IP")
	ip := net.ParseIP(ipFromEnv)
	if ip != nil {
		return []net.IP{ip}
	}
	return []net.IP{}
}

func (t *LocalTarget) HabCache() depot.HabCache {
	return depot.FromLocalCache()
}
