package server

import (
	"context"

	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

// UpgradeStatus is the gRPC handler for the UpgradeStatus rpc
// call. It attempts to detect if an upgrade is running.
//
// KNOWN ISSUES:
//  - Doesn't handle supplementary packages (not actually upgraded right now!)
//  - Returns an incomplete response if the deployment service itself hasn't been upgraded.
//  - Duplicates a lot of behavior in our main converge loop
//
// I think this should get a lot more simple as we integrate the
// converger. In that world we can potentially have a function that
// takes an expected state and tells us if it is true or not.
func (s *server) UpgradeStatus(ctx context.Context, _ *api.UpgradeStatusRequest) (*api.UpgradeStatusResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	response := &api.UpgradeStatusResponse{}
	latestManifest, err := s.releaseManifestProvider.GetCurrentManifest(ctx, s.deployment.Channel())
	if err != nil {
		return response, err
	}

	desiredManifest := s.deployment.CurrentReleaseManifest
	if s.shouldFetchManifest() {
		if desiredManifest == nil || latestManifest.Version() > desiredManifest.Version() {
			desiredManifest = latestManifest
		}
	}

	response.LatestAvailableVersion = latestManifest.Version()
	response.DesiredVersion = desiredManifest.Version()

	if s.deployment.CurrentReleaseManifest == nil {
		response.CurrentVersion = ""
	} else {
		response.CurrentVersion = s.deployment.CurrentReleaseManifest.Version()
	}

	// TODO(ssd) 2018-02-06: This address now exists in a few
	// places, would be nice to make it configurable and refer to
	// that config item.
	c := habapi.New("http://127.0.0.1:9631")
	runningServices, err := c.ListServices(ctx)
	if err != nil {
		return response, err
	}

	serviceIDs, err := deployment.ExpectedServiceIDsForConfig(s.deployment.Config.GetDeployment())
	if err != nil {
		return response, errors.Wrap(err, "unable to get list of services for automate-full")
	}

	response.RemainingServices, err = detectUpgradingServices(desiredManifest, runningServices, serviceIDs)
	if err != nil {
		return response, err
	}

	if len(response.RemainingServices) > 0 {
		response.State = api.UpgradeStatusResponse_UPGRADING
	} else {
		response.State = api.UpgradeStatusResponse_IDLE
	}

	return response, nil
}

// Return a list of services that are due for removal, addition, or
// upgrade per the current manifest.
func detectUpgradingServices(a2Manifest *manifest.A2,
	runningServices []habapi.ServiceInfo,
	canonicalServices []habpkg.HabPkg) ([]*api.UpgradingService, error) {

	ret := make([]*api.UpgradingService, 0, len(runningServices))
	// The manifest currently contains both service- and
	// non-service-packages. Thus, we want to read the service
	// list from the internal manifest. But, if *we* aren't up to
	// date, we might be wrong. So, first check ourselves.
	deploymentService := manifest.VersionedPackageFromManifest(a2Manifest, deploymentServiceName)

	if deploymentService == nil {
		return ret, errors.New("Could not find deployment-service in the manifest")
	}

	for _, svc := range runningServices {
		if svc.Pkg.Name == deploymentServiceName {
			// We don't do equality match here because in
			// development we are almost always on a newer
			// version of the deployment service.
			//
			// We could also see this in production if we
			// check in the middle of the deployment
			// process.
			//
			// Our standard data format of YYYYMMDDHHMMSS
			// is lexicographically sortable.
			if svc.Pkg.Release < deploymentService.Release() {
				upgradingService := makeUpgradingService(&svc, deploymentService)
				ret = append(ret, upgradingService)
				return ret, nil
			}
			break
		}
	}

	for _, svc := range canonicalServices {
		manifestSvc := manifest.VersionedPackageFromManifest(a2Manifest, svc.Name())
		if manifestSvc == nil {
			return ret, errors.Errorf("required service %s not found in manifest", svc.Name())
		}

		// Check for undeployed services or version-mismatch
		runningSvc, found := habapi.ServiceInfoByName(runningServices, manifestSvc.Name())
		if (!found) || (manifestSvc.Release() != runningSvc.Pkg.Release) {
			upgradingService := makeUpgradingService(runningSvc, manifestSvc)
			ret = append(ret, upgradingService)
		}
	}

	// Removed services
	for _, svc := range runningServices {
		found, _ := a2Manifest.PackageForServiceName(svc.Pkg.Name)
		if !found {
			upgradingService := makeUpgradingService(&svc, nil)
			ret = append(ret, upgradingService)
		}
	}

	return ret, nil
}

func makeUpgradingService(actual *habapi.ServiceInfo, target habpkg.VersionedPackage) *api.UpgradingService {
	ret := &api.UpgradingService{}

	if target != nil {
		ret.Target = &api.ServiceVersion{}
		ret.Target.Name = target.Name()
		ret.Target.Origin = target.Origin()
		ret.Target.Version = target.Version()
		ret.Target.Release = target.Release()
	}

	if actual != nil {
		ret.Actual = &api.ServiceVersion{}
		ret.Actual.Name = actual.Pkg.Name
		ret.Actual.Origin = actual.Pkg.Origin
		ret.Actual.Version = actual.Pkg.Version
		ret.Actual.Release = actual.Pkg.Release
	}
	return ret
}
