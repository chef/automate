package converge

import (
	"context"
	"os"
	"strconv"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/version"
)

// unloadWaitTime is the amount of time we will wait for a service to
// Unload. Habitat kills services after 8 seconds. This is over 3
// times that interval.
const unloadWaitTime = 30 * time.Second

// Compiler is an interface that gives back a plan that can be used to converge
// to a desired state
type Compiler interface {
	// Compile creates a plan to get to the desired state
	Compile(desiredState DesiredState) (Plan, error)
}

// Plan is a plan to execute that will bring us to a desired state
type Plan interface {
	// Execute executes the plan to get to the desired state
	Execute(EventSink) error
}

// PhaseOrderedCompiler is a compiler that will take a desired state and
// return a plan that achieves that desired state by working in phases.
type PhaseOrderedCompiler struct {
}

// PhaseOrderedProgram is a plan returned by the PhaseOrderedCompiler
// It will install the desired packages, configure the services, run the services,
// and remove any services that are no longer needed
// TODO(jaym) this type should not be exported
type PhaseOrderedProgram struct {
	phases []Phase
}

// Execute executes the plan to get to the desired state
func (p *PhaseOrderedProgram) Execute(eventSink EventSink) error {
	writer := newEventWriter(eventSink)
	for _, phase := range p.phases {
		err := phase.Run(writer)
		if err != nil {
			if err == api.ErrSelfUpgradePending {
				logrus.Info("Deployment-service upgrade pending")
				return err
			}
			if err == api.ErrSelfReconfigurePending {
				logrus.Info("Deployment-service reconfiguration pending")
				return err
			}
			logrus.WithError(err).WithFields(logrus.Fields{
				"phase": phase.Name(),
			}).Error("Phase failed")
			return err
		}
	}
	return nil
}

// Phase represents an execution phase for the PhaseOrderedProgram
// TODO(jaym) this type should not be exported
type Phase interface {
	Run(*eventWriter) error
	Name() string
}

// NewPhaseOrderedCompiler returns a PhaseOrderedCompiler
func NewPhaseOrderedCompiler() Compiler {
	return &PhaseOrderedCompiler{}
}

// Compile creates a plan to get to the desired state
func (c *PhaseOrderedCompiler) Compile(desiredState DesiredState) (Plan, error) {
	phases, err := buildPhases(desiredState)
	if err != nil {
		return nil, err
	}
	return &PhaseOrderedProgram{
		phases: phases,
	}, nil
}

// InstallPhase represents the steps to execute during the install phase
type InstallPhase struct {
	steps []installPhaseStep
}

type installPhaseStep struct {
	target target.Target
	pkg    habpkg.Installable
}

// SelfUpgradePhase describes a phase to upgrade the deployment service
type SelfUpgradePhase struct {
	target                   target.Target
	deploymentServicePackage habpkg.Installable
}

// SelfConfigurePhase describes a phase to configure the deployment service
type SelfConfigurePhase struct {
	target                   target.Target
	deploymentServicePackage habpkg.Installable
	userToml                 string
}

// SupervisorUpgradePhase describes a phase to configure (and
// potentially restart) the Habitat Supervisor
type SupervisorUpgradePhase struct {
	desiredSupState SupervisorState
	topology        Topology
}

// RunningPhase represents the phase that gets services running
type RunningPhase struct {
	steps []runningPhaseStep
}

type runningPhaseStep struct {
	target   target.Target
	pkg      habpkg.Installable
	bindMode string
	binds    []string
	userToml string
}

func buildPhases(desiredState DesiredState) ([]Phase, error) {
	var selfUpgradePhase *SelfUpgradePhase
	var selfConfigurePhase *SelfConfigurePhase

	installPhase := &InstallPhase{steps: []installPhaseStep{}}
	runningPhase := &RunningPhase{steps: []runningPhaseStep{}}
	supUpgradePhase := &SupervisorUpgradePhase{
		desiredSupState: desiredState.supervisor,
		topology:        desiredState.topology,
	}

	for target, services := range desiredState.topology {
		for _, service := range services {
			stepInstall := installPhaseStepForService(target, service)
			if stepInstall == nil {
				continue
			}
			installPhase.steps = append(installPhase.steps, *stepInstall)

			var err error
			if service.Name == deploymentServiceName {
				selfUpgradePhase, err = selfUpdatePhaseForDeploymentService(target, service, *stepInstall)
				if err != nil {
					return nil, err
				}

				selfConfigurePhase, err = selfConfigurePhaseForDeploymentService(target, service, *stepInstall)
				if err != nil {
					return nil, err
				}
			}

			stepRunning := runningPhaseStepForService(target, service, *stepInstall)
			if stepRunning == nil {
				continue
			}
			runningPhase.steps = append(runningPhase.steps, *stepRunning)
		}
	}

	return []Phase{
		installPhase,
		selfUpgradePhase,
		selfConfigurePhase,
		supUpgradePhase,
		runningPhase,
		buildCleanupPhase(desiredState),
	}, nil
}

// buildInstallPhase builds the phase that does the installing. If it finds
// any services with a desired state of installed, configured, or running,
// a step will be added to the install phase
func installPhaseStepForService(target target.Target, service Service) *installPhaseStep {
	var installedDesiredState *installed
	switch convergeState := service.ConvergeState.(type) {
	case *installed:
		installedDesiredState = convergeState
	case *running:
		installedDesiredState = convergeState.installed
	default:
		return nil
	}

	step := installPhaseStep{
		pkg:    installedDesiredState.pkg,
		target: target,
	}
	return &step
}

func selfUpdatePhaseForDeploymentService(target target.Target, service Service, stepInstall installPhaseStep) (*SelfUpgradePhase, error) {
	switch service.ConvergeState.(type) {
	case *running:
		return &SelfUpgradePhase{
			target:                   target,
			deploymentServicePackage: stepInstall.pkg,
		}, nil
	case *skip:
		return nil, nil
	}

	return nil, errors.New("deployment-service desired state must be running")
}

func selfConfigurePhaseForDeploymentService(target target.Target, service Service, stepInstall installPhaseStep) (*SelfConfigurePhase, error) {
	switch convergeState := service.ConvergeState.(type) {
	case *running:
		return &SelfConfigurePhase{
			target:                   target,
			deploymentServicePackage: stepInstall.pkg,
			userToml:                 convergeState.userToml,
		}, nil
	case *skip:
		return nil, nil
	}

	return nil, errors.New("deployment-service desired state must be running")
}

func runningPhaseStepForService(target target.Target, service Service, installStep installPhaseStep) *runningPhaseStep {
	switch convergeState := service.ConvergeState.(type) {
	case *running:
		return &runningPhaseStep{
			target:   target,
			pkg:      installStep.pkg,
			binds:    convergeState.binds,
			bindMode: convergeState.bindMode,
			userToml: convergeState.userToml,
		}
	default:
		return nil
	}
}

// Run runs the phase
// For the install phase, this means installing hartifacts and packages that
// are not installed.
func (phase *InstallPhase) Run(writer *eventWriter) error {
	for _, step := range phase.steps {
		modified := false
		writer.InstallingService(step.pkg)

		logCtx := logrus.WithFields(logrus.Fields{
			"phase":       phase.Name(),
			"pkg":         habpkg.Ident(step.pkg),
			"install-arg": step.pkg.InstallIdent(),
		})

		isInstalled, err := step.target.IsInstalled(context.TODO(), step.pkg)
		if err != nil {
			err = errors.Wrapf(err, "failure occurred checking if package %q is installed", step.pkg)
			logCtx.WithError(err).Error("Failed checking if package was installed")
			writer.InstallFailed(step.pkg, err)
			return err
		}
		if !isInstalled {
			err = step.target.InstallService(context.TODO(), step.pkg, "")
			if err != nil {
				err = errors.Wrapf(err, "failed to install package %q", step.pkg)
				logCtx.WithError(err).Error("Unable to install package")
				writer.InstallFailed(step.pkg, err)
				return err
			}
			logCtx.Info("installed package")
			modified = true
		}

		binlinks := services.BinlinksForPackage(step.pkg.Name())
		for _, cmd := range binlinks {
			isBinlinked, err := step.target.IsBinlinked(step.pkg, cmd)
			if err != nil {
				err = errors.Wrapf(err, "failed to check existing binlink %q for %q", cmd, step.pkg.Name())
				logCtx.WithError(err).Error("Failed checking if package was installed")
				writer.InstallFailed(step.pkg, err)
				return err
			}

			if !isBinlinked {
				cmdOutput, err := step.target.BinlinkPackage(context.TODO(), step.pkg, cmd)
				if err != nil {
					logCtx.WithError(err).Warnf("failed to binlink command %q in pkg %q - hab output: %s",
						cmd, habpkg.Ident(step.pkg), cmdOutput)
				} else {
					logCtx.WithFields(logrus.Fields{"binlink": cmd}).Info("created binlink")
				}
				modified = true
			}
		}

		writer.InstallSuccess(step.pkg, modified)
	}
	//TODO(jaym) Uninstall things? Probably not needed
	return nil
}

// Name returns "install"
func (phase *InstallPhase) Name() string {
	return "Install"
}

// Run the self upgrade phase.
func (phase *SelfUpgradePhase) Run(writer *eventWriter) error {
	if phase == nil {
		// Allow skipping
		return nil
	}

	currentReleaseStr := version.BuildTime
	desiredPackage := phase.deploymentServicePackage
	desiredReleaseStr := desiredPackage.Release()

	logCtx := logrus.WithFields(logrus.Fields{
		"current_release": currentReleaseStr,
		"desired_release": desiredReleaseStr,
	})

	curRel, err := strconv.Atoi(currentReleaseStr)
	if err != nil {
		return errors.Wrapf(err, "Could not parse deployment-service release %s", currentReleaseStr)
	}
	desRel, err := strconv.Atoi(desiredReleaseStr)
	if err != nil {
		return errors.Wrapf(err, "Could not parse deployment-service release %s", desiredReleaseStr)
	}

	if curRel < desRel {
		// only upgrade if the release is newer. this is a safety net for
		// the following situation: customers upgrading from a version of a2
		// with deployments managed by `hab-sup` when the service_updater
		// gets a new version of the package that watches the manifest, and
		// the new manifest hasn't been published (this is not atomic) then
		// we would immediately downgrade to a version of
		// `deployment-service` that didn't support manifest-based upgrades
		// and would be stuck at that old version until another promotion,
		// or possibly forever.
		logCtx.Info("Requesting self upgrade")
		err = phase.target.LoadDeploymentService(context.TODO(), desiredPackage)
		if err != nil {
			return errors.Wrap(err, "Failed to start service")
		}
		return api.ErrSelfUpgradePending
	} else if curRel > desRel {
		// if the current release is newer than the desired release, stop
		// the converge loop so that we don't continue installing and
		// configuring services when we know that the version of the
		// deployment-service doesn't match what's in the manifest
		logCtx.Info("Refusing to self downgrade")
		return errors.New("Refusing to self downgrade")
	}
	// else (the previous block returns)
	logCtx.Debug("Deployment service up-to-date")
	return nil
}

// Name returns "install"
func (phase *SelfUpgradePhase) Name() string {
	return "Install"
}

// Run the self-configuration phase for the deployment. This means
// checking if the user toml for the deployment service has changed
// and writing it out to disk if it has. If we write out the
// configuration, we also write a sentinel file to ensure we don't
// proceed past this part of the converge process until the
// reconfiguration has been processed.
func (phase *SelfConfigurePhase) Run(writer *eventWriter) error {
	if phase == nil {
		// Allow skipping
		return nil
	}

	// We already have one reconfigure pending that hasn't been
	// applied yet.
	pending, err := phase.target.GetDeploymentServiceReconfigurePending()
	if err != nil {
		return errors.Wrap(err, "Failed to determine if deployment-service reconfiguration is already pending")
	}

	if pending {
		return api.ErrSelfReconfigurePending
	}

	modified := false
	writer.ConfiguringService(phase.deploymentServicePackage)
	currentConfig, err := phase.target.GetUserToml(phase.deploymentServicePackage)
	if err != nil {
		err = errors.Wrap(err, "Failed to get current configuration")
		writer.ConfiguringFailed(phase.deploymentServicePackage, err)
		return err
	}

	if currentConfig != phase.userToml {
		err = phase.target.SetDeploymentServiceReconfigurePending()
		if err != nil {
			return errors.Wrap(err, "Failed to mark deployment-service as reconfigure-pending")
		}

		err := phase.target.SetUserToml(phase.deploymentServicePackage.Name(), phase.userToml)
		if err != nil {
			// Rollback configuration pending marker
			unsetErr := phase.target.UnsetDeploymentServiceReconfigurePending()
			if unsetErr != nil {
				logrus.WithError(err).Error("could not unset upgrade-pending configuration marker after failure to write configuration")
			}
			err = errors.Wrap(err, "Failed to set configuration")
			writer.ConfiguringFailed(phase.deploymentServicePackage, err)
			return err
		}
		modified = true
		// TODO(ssd) 2018-05-15: We need this because we can't
		// reliably determine whether Habitat is actually
		// going to restart us. Remove when we do a DeepEqual
		// on the config.
		go func() {
			time.Sleep(30 * time.Second)
			pending, err := phase.target.GetDeploymentServiceReconfigurePending()
			if err != nil {
				logrus.WithError(err).Warn("Failed to determine if deployment-service is still awaiting reconfigure.")
			}

			if !pending {
				return
			}

			logrus.Warn("Expected HUP from habitat but none received after 30 seconds, sending HUP to self")
			myPid := os.Getpid()
			err = syscall.Kill(myPid, syscall.SIGHUP)
			if err != nil {
				logrus.WithError(err).Warnf("Failed to send HUP to self (pid = %d)", myPid)
			}
		}()
	}

	writer.ConfiguringSuccess(phase.deploymentServicePackage, modified)
	if modified {
		return api.ErrSelfReconfigurePending
	}
	return nil

}

// Name returns "Configure"
func (phase *SelfConfigurePhase) Name() string {
	return "Configure"
}

// RunningPhase ensures that the correct version of a service is
// loaded with the correct configuration.
func (phase *RunningPhase) Run(writer *eventWriter) error {
	deployedServicesByTarget := make(map[target.Target]map[string]target.DeployedService)

	restartMitigation := NewSafeServiceRestartRunner()
	for _, step := range phase.steps {
		if step.pkg.Name() == deploymentServiceName {
			continue
		}
		deployedServices, exist := deployedServicesByTarget[step.target]
		if !exist {
			var err error
			deployedServices, err = step.target.DeployedServices(context.TODO())
			if err != nil {
				return errors.Wrap(err, "failed to get deployed services from habitat")
			}
			deployedServicesByTarget[step.target] = deployedServices
		}

		writer.StartingService(step.pkg)

		deployedService, deployed := deployedServices[step.pkg.Name()]

		configWriteNeeded, err := configOutOfDate(step)
		if err != nil {
			writer.StartingFailed(step.pkg, err)
			return err
		}

		// Reload the service if we are not already deployed
		// or if we are out of date.
		reloadNeeded := !deployed || pkgOutOfDate(deployedService, step.pkg)

		// Also reload the service if it requires a reload to
		// safely reconfigure.
		if configWriteNeeded && configWriteRequiresReload(step.pkg) {
			reloadNeeded = true
		}

		// Nothing to do, early return
		if !(reloadNeeded || configWriteNeeded) {
			writer.StartingSuccess(step.pkg, false)
			continue
		}

		if deployed && reloadNeeded {
			err := restartMitigation.RunIfRequired(step.target, step.pkg)
			if err != nil {
				err = errors.Wrap(err, "failed to run restart mitigation")
				writer.StartingFailed(step.pkg, err)
				return err
			}
		}

		if reloadNeeded {
			err := step.target.UnloadService(context.TODO(), step.pkg)
			if err != nil {
				err = errors.Wrap(err, "failed to unload service")
				writer.StartingFailed(step.pkg, err)
				return err
			}

			client := step.target.HabAPIClient()
			err = habapi.WaitForUnload(client, []habpkg.VersionedPackage{step.pkg}, unloadWaitTime)
			if err != nil {
				logrus.WithError(err).Warnf("service %q did not unload before timeout", step.pkg.Name())
			}
		}

		if configWriteNeeded {
			err := step.target.SetUserToml(step.pkg.Name(), step.userToml)
			if err != nil {
				err = errors.Wrap(err, "failed to set configuration")
				writer.StartingFailed(step.pkg, err)
				return err
			}
		}

		if reloadNeeded {
			err := step.target.LoadService(context.TODO(), step.pkg, target.BindMode(step.bindMode), target.Binds(step.binds))
			if err != nil {
				err = errors.Wrap(err, "Failed to start service")
				writer.StartingFailed(step.pkg, err)
				return err
			}
		}

		writer.StartingSuccess(step.pkg, true)
	}

	err := restartMitigation.RestartServices()
	if err != nil {
		return errors.Wrap(err, "Failed to restart services that were stopped by restartMitigation")
	}

	return nil
}

func configWriteRequiresReload(pkg habpkg.Installable) bool {
	return pkg.Name() == postgresqlServiceName || pkg.Name() == elasticsearchServiceName
}

func configOutOfDate(step runningPhaseStep) (bool, error) {
	currentConfig, err := step.target.GetUserToml(step.pkg)
	if err != nil {
		err = errors.Wrap(err, "Failed to get current configuration")
		return false, err
	}
	return currentConfig != step.userToml, nil
}

func pkgOutOfDate(deployedService target.DeployedService, desiredPackage habpkg.Installable) bool {
	currentPackage := &deployedService.Pkg

	logrus.WithFields(logrus.Fields{
		"current": habpkg.Ident(currentPackage),
		"desired": habpkg.Ident(desiredPackage),
	}).Debug("Checking package")

	// The "at-once" UpdateStrategy was used in early versions of
	// A2. We now use an UpdateStrategy of "none" everywhere.
	if deployedService.UpdateStrategy == "at-once" {
		return true
	}

	// Restart any services that have been stopped
	if deployedService.DesiredProcessState == target.ProcessStateDown {
		return true
	}

	if desiredPackage.Version() != "" {
		if currentPackage.Version() != desiredPackage.Version() {
			return true
		}
	}
	if desiredPackage.Release() != "" {
		if currentPackage.Release() != desiredPackage.Release() {
			return true
		}
	}

	return false
}

// Name returns "Running"
func (phase *RunningPhase) Name() string {
	return "Running"
}

// CleanupPhase represents the phase responsible for removing services that no
// longer should be running
type CleanupPhase struct {
	steps              []cleanupPhaseStep
	packageCleanupMode string
}

type cleanupPhaseStep struct {
	target          target.Target
	dontClean       map[string]bool
	ignoredPackages []habpkg.HabPkg
}

// CleanupPhase builds the phase responsible for unloading services
// All services marked in the desired state as running or skip are safe
// from being stopped.
func buildCleanupPhase(desiredState DesiredState) *CleanupPhase {
	steps := []cleanupPhaseStep{}
	for target, services := range desiredState.topology {
		step := cleanupPhaseStep{
			target:    target,
			dontClean: make(map[string]bool),
		}
		ignoredPackages := make([]habpkg.HabPkg, 0, len(services)+len(desiredState.ignoredPackages)+3)
		for _, service := range services {
			var pkg habpkg.Installable
			if s, ok := service.ConvergeState.(*running); ok {
				step.dontClean[service.Name] = true
				pkg = s.installed.pkg
			} else if _, ok := service.ConvergeState.(*skip); ok {
				step.dontClean[service.Name] = true
			} else if s, ok := service.ConvergeState.(*installed); ok {
				pkg = s.pkg
			}

			if pkg != nil {
				ignoredPackages = append(ignoredPackages, habpkg.NewFQ(pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release()))
			}
		}

		ignoredPackages = append(ignoredPackages, desiredState.ignoredPackages...)
		ignoredPackages = append(ignoredPackages, desiredState.supervisor.BinPkg())
		ignoredPackages = append(ignoredPackages, desiredState.supervisor.SupPkg())
		ignoredPackages = append(ignoredPackages, desiredState.supervisor.LauncherPkg())

		step.dontClean[deploymentServiceName] = true
		step.ignoredPackages = ignoredPackages
		steps = append(steps, step)
	}
	return &CleanupPhase{
		steps:              steps,
		packageCleanupMode: desiredState.packageCleanupMode,
	}
}

func (phase *SupervisorUpgradePhase) Run(w *eventWriter) error {
	if phase.desiredSupState.IsSkip() {
		logrus.Info("Skipping systemd/hab-sup upgrade")
		return nil
	}

	anyPendingRestarts := false
	for target, services := range phase.topology {
		usingSystemd, err := target.SystemdRunning()
		if err != nil {
			return errors.Wrap(err, "failed checking if systemd is in use")
		}

		err = phase.installHabPackages(w, target)
		if err != nil {
			return errors.Wrap(err, "failed to install Habitat and the Habitat Supervisor")
		}

		if !usingSystemd {
			logrus.Warn("Cannot manage habitat supervisor since we are not running under systemd")
			continue
		}

		err = phase.updateHapSupSymlink(w, target)
		if err != nil {
			return errors.Wrap(err, "failed to update hab sup symlink")
		}

		err = phase.updateSystemdUnitFile(w, target)
		if err != nil {
			return errors.Wrap(err, "failed to update systemd unit file")
		}

		err = phase.reloadSystemd(w, target)
		if err != nil {
			return errors.Wrap(err, "failed to reload systemd daemon")
		}

		pendingRestart, err := phase.restartHabSup(w, target, services)
		if err != nil {
			return errors.Wrap(err, "failed to restart habitat supervisor")
		}

		anyPendingRestarts = anyPendingRestarts || pendingRestart
	}

	// This is here to protect against getting past this phase
	// before the hab-sup restart is finished.
	if anyPendingRestarts {
		return errors.New("hab-sup upgrade pending")
	}

	return nil
}

func (phase *SupervisorUpgradePhase) restartHabSup(w *eventWriter, target target.Target, services []Service) (bool, error) {
	w.RestartingHabSup()
	restartRequired, err := target.HabSupRestartRequired(phase.desiredSupState.SupPkg())
	if err != nil {
		err := errors.Wrap(err, "failed to determine if hab-sup restart is required")
		w.RestartingHabSupFailed(err)
		return false, err
	}

	if restartRequired {
		serviceList := make([]string, len(services))
		for i, svc := range services {
			serviceList[i] = svc.Name
		}
		waitForFullRestart, err := target.HabSupRestart(context.TODO(), serviceList)
		if err != nil {
			err = errors.Wrap(err, "failed to restart Habitat supervisor")
			w.RestartingHabSupFailed(err)
			return true, err
		}

		w.RestartingHabSupSuccess(true)
		return waitForFullRestart, nil
	}

	w.RestartingHabSupSuccess(false)
	return false, nil
}

func (phase *SupervisorUpgradePhase) installHabPackages(w *eventWriter, target target.Target) error {
	binPkg := phase.desiredSupState.BinPkg()
	supPkg := phase.desiredSupState.SupPkg()
	launchPkg := phase.desiredSupState.LauncherPkg()
	expectedPackages := []*habpkg.HabPkg{
		&binPkg,
		&supPkg,
		&launchPkg,
	}

	for _, expectedPackage := range expectedPackages {
		w.InstallingPackage(expectedPackage)
		isInstalled, err := target.IsInstalled(context.TODO(), expectedPackage)
		if err != nil {
			err := errors.Wrapf(err, "failed to get current install state of %s", habpkg.Ident(expectedPackage))
			w.InstallingPackageFailed(expectedPackage, err)
			return err
		}

		if !isInstalled {
			err := target.InstallService(context.TODO(), expectedPackage, "")
			if err != nil {
				err = errors.Wrapf(err, "failed to install %s", habpkg.Ident(expectedPackage))
				w.InstallingPackageFailed(expectedPackage, err)
				return err
			}

			w.InstallingPackageSuccess(expectedPackage, true)
		} else {
			w.InstallingPackageSuccess(expectedPackage, false)
		}

	}

	output, err := target.BinlinkPackage(context.TODO(), &binPkg, "hab")
	if err != nil {
		logrus.Debugf("Binlink of %q failed with output: %q", habpkg.Ident(&binPkg), output)
		logrus.Warnf("Could not binlink %q, some hab commands may not work", habpkg.Ident(&binPkg))
	}

	return nil
}

func (phase *SupervisorUpgradePhase) updateHapSupSymlink(_ *eventWriter, target target.Target) error {
	currentPkg, err := target.GetSymlinkedHabSup()
	if err != nil {
		if _, isPathErr := err.(*os.PathError); !isPathErr {
			return errors.Wrap(err, "failed to create hab sup symlink")
		}
	}

	desiredPkg := phase.desiredSupState.SupPkg()

	if err != nil || currentPkg != desiredPkg {
		if err := target.SymlinkHabSup(desiredPkg); err != nil {
			return errors.Wrap(err, "failed to create hab-sup symlink")
		}
	}
	return nil
}

func (phase *SupervisorUpgradePhase) updateSystemdUnitFile(w *eventWriter, target target.Target) error {
	w.ConfiguringSystemd()

	oldUnitFileContent, err := target.GetAutomateUnitFile()
	if err != nil {
		err = errors.Wrap(err, "failed to query current systemd unit file")
		w.ConfiguringSystemdFailed(err)
		return err
	}

	newUnitFileContent, err := target.RenderAutomateUnitFile(
		phase.desiredSupState.ProxyConfig(),
		phase.desiredSupState.BinPkg(),
		phase.desiredSupState.LauncherPkg(),
	)

	if err != nil {
		err = errors.Wrap(err, "generating systemd unit file")
		w.ConfiguringSystemdFailed(err)
		return err
	}

	if string(oldUnitFileContent) != newUnitFileContent {
		err := target.WriteAutomateUnitFile([]byte(newUnitFileContent))
		if err != nil {
			err = errors.Wrap(err, "failed to write new systemd unit file")
			w.ConfiguringSystemdFailed(err)
		} else {
			w.ConfiguringSystemdSuccess(true)
		}
	} else {
		w.ConfiguringSystemdSuccess(false)
	}

	return nil
}

func (phase *SupervisorUpgradePhase) reloadSystemd(w *eventWriter, target target.Target) error {
	w.ReloadingSystemd()
	reloadRequired, err := target.SystemdReloadRequired()
	if err != nil {
		err = errors.Wrap(err, "failed to determine if systemd daemon-reload is required")
		w.ReloadingSystemdFailed(err)
		return err
	}

	if reloadRequired {
		err = target.SystemdReload()
		if err != nil {
			err = errors.Wrap(err, "failed to reload systemd daemon")
			w.ReloadingSystemdFailed(err)
			return err
		}

		w.ReloadingSystemdSuccess(true)
		return nil
	}

	w.ReloadingSystemdSuccess(false)
	return nil
}

func (phase *SupervisorUpgradePhase) Name() string {
	return "supervisor upgrade"
}

func stopService(writer *eventWriter, t target.Target, pkg habpkg.Installable) error {
	writer.UnloadingService(pkg)
	logrus.WithFields(logrus.Fields{
		"pkg": pkg,
	}).Info("Unloading service. Desired state was not running.")
	err := t.UnloadService(context.TODO(), pkg)
	if err != nil {
		err = errors.Wrap(err, "Failed to unload service")
		writer.UnloadingFailed(pkg, err)
		return err
	}
	writer.UnloadingSuccess(pkg, true)
	return nil
}

// Run runs the phase
// For cleanup phase, that means unloading services that are supervised by Habitat
// but not marked as running or skip in the desired state.
func (phase *CleanupPhase) Run(writer *eventWriter) error {
	logrus.WithField("phase", phase).Debug("Starting cleanup phase")

	for _, step := range phase.steps {
		stopMitigation := NewSafeServiceShutdownRunner()
		stopMitigation.stopAllowed = func(t target.Target, pkg habpkg.VersionedPackage) bool {
			return !step.dontClean[pkg.Name()]
		}
		stopMitigation.stopServiceFunc = func(t target.Target, pkg habpkg.Installable) error {
			return stopService(writer, t, pkg)
		}

		deployedServices, err := step.target.DeployedServices(context.TODO())
		if err != nil {
			return err
		}

		gcRootsSet := newPkgSet(len(deployedServices) + len(step.ignoredPackages))
		gcRootsSet.Add(step.ignoredPackages...)
		for _, deployedService := range deployedServices {
			gcRootsSet.Add(deployedService.Pkg)
			if !step.dontClean[deployedService.Pkg.Name()] {
				if err := stopMitigation.RunIfRequired(step.target, &deployedService.Pkg); err != nil {
					logrus.WithError(err).WithField("pkg", deployedService.Pkg).Error("failed to run stop mitigation")
					return err
				}
				if err := stopService(writer, step.target, &deployedService.Pkg); err != nil {
					logrus.WithError(err).WithField("pkg", deployedService.Pkg).Error("failed to stop service")
					return err
				}
				err := habapi.WaitForDown(step.target.HabAPIClient(), []habpkg.VersionedPackage{&deployedService.Pkg}, waitForDownTimeout)
				if err != nil {
					logrus.WithError(err).WithField("pkg", deployedService.Pkg).Error("failed waiting for down")

				}

			}
		}

		// Clean up unused packages. We will not clean up things we just took
		// down so that we may give them time to go down cleanly
		gcRoots := gcRootsSet.List()
		gc := depot.NewGarbageCollector(step.target.HabCache())
		if err := gc.Collect(gcRoots, phase.packageCleanupMode); err != nil {
			logrus.WithError(err).Warn("Failed to clean up unused packages")
		}
	}
	return nil
}

type pkgSet struct {
	memberMap map[habpkg.HabPkg]struct{}
}

func newPkgSet(size int) *pkgSet {
	return &pkgSet{
		memberMap: make(map[habpkg.HabPkg]struct{}, size),
	}
}

func (p *pkgSet) Add(pkgs ...habpkg.HabPkg) {
	for _, pkg := range pkgs {
		if _, found := p.memberMap[pkg]; !found {
			p.memberMap[pkg] = struct{}{}
		}
	}
}

func (p *pkgSet) List() []habpkg.HabPkg {
	arr := make([]habpkg.HabPkg, 0, len(p.memberMap))
	for k := range p.memberMap {
		arr = append(arr, k)
	}
	return arr
}

// Name returns "Cleanup"
func (phase *CleanupPhase) Name() string {
	return "Cleanup"
}
