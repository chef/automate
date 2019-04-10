package depot

import (
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// TODO(jaym): I'm not sure what to call this thing
type HabCache interface {
	TDepsForPackage(habpkg.VersionedPackage) ([]habpkg.HabPkg, error)
	ListAllPackages() ([]habpkg.HabPkg, error)
	Delete(habpkg.HabPkg) error
}

type GarbageCollector struct {
	cache HabCache
}

func NewGarbageCollector(cache HabCache) *GarbageCollector {
	return &GarbageCollector{
		cache: cache,
	}
}

func (gc *GarbageCollector) Collect(roots []habpkg.HabPkg, cleanupMode string) error {
	switch cleanupMode {
	case "conservative":
		return gc.ConservativeCollect(roots)
	case "aggressive":
		return gc.AggressiveCollect(roots)
	case "disabled":
		return nil
	default:
		return errors.Errorf("invalid package cleanup mode %q given to gc.Collect()", cleanupMode)
	}
}

// ConservativeCollect deletes old versions of the rootPackages. Transitive
// dependencies and packages not managed by deployment service are not deleted.
// This is a safe compromise that will not delete any packages installed by the
// user, at the cost of leaving multiple versions of Automate's transitive
// dependencies on-disk. Automate packages update much more frequently than
// dependencies so this should give most of the benefits of package cleanup.
func (gc *GarbageCollector) ConservativeCollect(rootPackages []habpkg.HabPkg) error {
	installedPkgs, err := gc.cache.ListAllPackages()
	if err != nil {
		return errors.Wrap(err, "Failed to list installed packages")
	}

	keepersByShortIdent := make(map[string][]habpkg.HabPkg, len(rootPackages))
	for _, pkg := range rootPackages {
		s := habpkg.ShortIdent(&pkg)
		keepersByShortIdent[s] = append(keepersByShortIdent[s], pkg)
	}

	pkgsToDelete := []habpkg.HabPkg{}

	for _, candidate := range installedPkgs {
		// only check packages like chef/deployment-service (part of automate) but
		// skip packages like core/curl or chef/chef (not in automate)
		keepers, isAutomatePkg := keepersByShortIdent[habpkg.ShortIdent(&candidate)]
		if !isAutomatePkg {
			continue
		}

		// don't delete if the version is the one we want
		isKeeper := false
		for _, keeper := range keepers {
			if habpkg.Ident(&keeper) == habpkg.Ident(&candidate) {
				isKeeper = true
				break
			}
		}
		if isKeeper {
			continue
		}

		pkgsToDelete = append(pkgsToDelete, candidate)

	}

	if len(pkgsToDelete) > 0 {
		logrus.WithFields(logrus.Fields{
			"roots": rootPackages,
			"mode":  "conservative",
		}).Info("Cleaning up unused packages")
	}

	for _, pkg := range pkgsToDelete {
		logrus.WithField("package", pkg).Info("Removing package")
		if err := gc.cache.Delete(pkg); err != nil {
			return errors.Wrapf(err, "Failed to delete %s", pkg)
		}
	}

	return nil
}

// AggressiveCollect deletes all habitat packages except those that are listed
// in the roots list or are transitive dependencies of those packages. This can
// result in deletion of packages that have been intentionally installed by the
// user, as there is no way to differentiate packages installed by the
// deployment service but are no longer used from those installed by the user.
func (gc *GarbageCollector) AggressiveCollect(roots []habpkg.HabPkg) error {
	if len(roots) == 0 {
		return errors.New("refusing to delete all the things")
	}

	installedPkgs, err := gc.cache.ListAllPackages()
	if err != nil {
		return errors.Wrap(err, "Failed to list installed packages")
	}

	markMap := makeMarkAndSweepMap(installedPkgs)
	for _, root := range roots {
		deps, err := gc.cache.TDepsForPackage(&root)
		if err != nil {
			if IsErrNotFound(err) {
				logrus.Debugf("Root %s not found", root)
				continue
			}
			return errors.Wrapf(err, "failed to get dependencies for %s", root)
		}

		markMap[root] = true
		for _, dep := range deps {
			markMap[dep] = true
		}
	}

	loggedOnce := false
	// TODO(jaym): we should probably sort installed packages such
	// that things are topologically sorted by their dependencies
	for _, pkg := range installedPkgs {
		if !markMap[pkg] {
			if !loggedOnce {
				logrus.WithFields(logrus.Fields{
					"roots": roots,
					"mode":  "aggressive",
				}).Info("Cleaning up unused packages")
				loggedOnce = true
			}
			logrus.WithField("package", pkg).Info("Removing package")
			if err := gc.cache.Delete(pkg); err != nil {
				return errors.Wrapf(err, "Failed to delete %s", pkg)
			}
		}
	}

	return nil
}

func makeMarkAndSweepMap(pkgs []habpkg.HabPkg) map[habpkg.HabPkg]bool {
	markMap := make(map[habpkg.HabPkg]bool, len(pkgs))
	for _, pkg := range pkgs {
		markMap[pkg] = false
	}
	return markMap
}
