package converge

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

const (
	waitForDownTimeout = 30 * time.Second
	waitForUpTimeout   = 30 * time.Second
)

// safeServiceShutdownRunner provides a small API for mitigating the unsafe
// unloading of services.
//
// The goal of this mitigation is to avoid database services from
// taking more than 8 seconds to shutdown. After 8 seconds Habitat
// will send SIGKILL to the service.
//
// To speed up the service shutdown, we stop all potential clients of
// those services by stopping all services that are bound to
// them.
//
// Our service may still, of course, take more than 8 seconds to
// shutdown. For instance, perhaps there are clients (such as
// monitoring tools) outside of Habitat or perhaps the service has a
// lot of internal cleanup to do before shutdown.
//
type safeServiceShutdownRunner struct {
	stopServiceFunc func(target.Target, habpkg.Installable) error
	stopAllowed     func(target.Target, habpkg.VersionedPackage) bool
	stoppedServices []stoppedService
}

// safeServiceRestartRunner extends the stopMitigation API to be able to restart
// services instead of keeping them down.
// It uses Hab's start and stop function instead of load and unload
type safeServiceRestartRunner struct {
	*safeServiceShutdownRunner
}

type stoppedService struct {
	target target.Target
	pkg    habpkg.HabPkg
}

func NewSafeServiceShutdownRunner() *safeServiceShutdownRunner {
	return &safeServiceShutdownRunner{
		stoppedServices: []stoppedService{},
		stopServiceFunc: func(t target.Target, pkg habpkg.Installable) error {
			return t.UnloadService(context.TODO(), pkg)
		},
	}
}

func NewSafeServiceRestartRunner() *safeServiceRestartRunner {
	return &safeServiceRestartRunner{
		safeServiceShutdownRunner: &safeServiceShutdownRunner{
			stopServiceFunc: func(t target.Target, pkg habpkg.Installable) error {
				return t.StopService(context.TODO(), pkg)
			},
		},
	}
}

func (s stoppedService) Name() string { return s.pkg.Name() }

func (r *safeServiceShutdownRunner) RunIfRequired(t target.Target, pkg habpkg.VersionedPackage) error {
	required, err := mitigationRequiredForPkg(t, pkg)
	if err != nil {
		return err
	}

	if !required {
		return nil
	}
	logrus.Infof("Restart mitigation is required for service %s", pkg.Name())
	return r.run(t, pkg)
}

func (r *safeServiceShutdownRunner) run(t target.Target, pkg habpkg.VersionedPackage) error {
	servicesToStop, err := servicesToStopForPkg(t, pkg)
	if err != nil {
		return errors.Wrap(err, "could not determine services to stop")
	}

	for _, svc := range servicesToStop {
		if r.stopAllowed == nil || r.stopAllowed(t, &svc) {
			err := r.stopServiceFunc(t, &svc)
			if err != nil {
				return errors.Wrapf(err, "could not stop %q", habpkg.Ident(&svc))
			}

			r.stoppedServices = append(r.stoppedServices, stoppedService{
				// TODO(multinode) 2018-10-18: In a multi-node
				// world, these services could live on
				// different targets. We will need some
				// mapping of service->target
				target: t,
				pkg:    svc,
			})
		}
	}

	if len(r.stoppedServices) > 0 {
		client := r.stoppedServices[0].target.HabAPIClient()
		err := habapi.WaitForDown(client, stoppedToVersioned(r.stoppedServices), waitForDownTimeout)
		if err != nil {
			logrus.WithError(err).Warn("Some reverse dependencies failed to stop")
		}
	}
	logrus.Infof("Restart mitigation for service %s complete", pkg.Name())
	return nil
}

func (r *safeServiceRestartRunner) RestartServices() error {
	if len(r.stoppedServices) == 0 {
		return nil
	}

	var finalError error
	for _, svc := range r.stoppedServices {
		logrus.Infof("Restarting %s which was stopped by restartMitigation", svc.Name())
		// StartService is safe to call on a started service,
		// so we don't filter based on current status.
		err := svc.target.StartService(context.TODO(), &svc.pkg)
		if err != nil {
			// We are going to try to restart as much as
			// possible so we collect up the errors here.
			logrus.WithError(err).WithField("service", svc.Name()).Error("failed to restart service")
			finalError = multierr.Combine(finalError, err)
		}
	}

	logrus.Info("Waiting for restarted services to enter UP state")
	client := r.stoppedServices[0].target.HabAPIClient()
	err := habapi.WaitForUp(client, stoppedToVersioned(r.stoppedServices), waitForUpTimeout)
	if err != nil {
		// We don't return the error here because it is
		// possible that we can't start this service now until
		// it is upgraded.
		logrus.WithError(err).Warn("Some reverse dependencies failed to start")
	}

	r.stoppedServices = []stoppedService{}
	return finalError
}

func mitigationRequiredForPkg(t target.Target, pkg habpkg.VersionedPackage) (bool, error) {
	// Because this mitigation is expensive, we only do this for
	// our main data services.
	// The reason PG gateway is in this list is because if we update pg, then pg gateway goes down.
	// It comes up very late in the whole upgrade process. This causes extra slowness because hab
	// spends a bunch of time trying to restart services that are going to keep dying until pg gateway
	// comes up. By having it in this list, we can rebuild pg gateway when we update postgres and things
	// will happen in a sane order.
	pkgIsImportant := pkg.Name() == postgresqlServiceName ||
		pkg.Name() == elasticsearchServiceName ||
		pkg.Name() == pgGatewayServiceName ||
		pkg.Name() == esGatewayServiceName
	if !pkgIsImportant {
		return false, nil
	}

	supPkg, err := t.HabSup().SupPkg()
	if err != nil {
		return false, err
	}

	if target.SupportsCleanShutdown(supPkg) {
		return false, nil
	}

	return true, nil
}

func servicesToStopForPkg(target target.Target, pkg habpkg.VersionedPackage) ([]habpkg.HabPkg, error) {
	consumingServices, err := habapi.AllConsumingServices(target.HabAPIClient(), pkg)
	if err != nil {
		return nil, err
	}

	// These cases should never really happen unless we ship
	// something with every strange binds
	ret := consumingServices[:0]
	for _, svc := range consumingServices {
		if svc.Name() == deploymentServiceName {
			continue
		}

		if svc.Name() == pkg.Name() {
			continue
		}

		ret = append(ret, svc)
	}

	return ret, nil
}

func stoppedToVersioned(in []stoppedService) []habpkg.VersionedPackage {
	ret := make([]habpkg.VersionedPackage, len(in))
	for i := range in {
		ret[i] = &in[i].pkg
	}
	return ret
}
