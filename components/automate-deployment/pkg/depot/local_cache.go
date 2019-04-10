package depot

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

type LocalCacheOpt func(*LocalCache)

type LocalCache struct {
	root string
}

func WithLocalHabRoot(root string) LocalCacheOpt {
	return func(conf *LocalCache) {
		conf.root = root
	}
}

func FromLocalCache(opts ...LocalCacheOpt) *LocalCache {
	local := LocalCache{
		root: "/hab",
	}

	for _, o := range opts {
		o(&local)
	}

	return &local
}

func (local *LocalCache) TDepsForPackage(pkg habpkg.VersionedPackage) ([]habpkg.HabPkg, error) {
	pkgPath := path.Join(local.root, "pkgs", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	if _, err := os.Stat(pkgPath); err != nil {
		if os.IsNotExist(err) {
			return nil, &errPackageNotFound{
				path: pkgPath,
			}
		}
		return nil, errors.Wrapf(err, "Could not access %s. Make sure the package has been installed", pkgPath)
	}

	tdepsPath := path.Join(pkgPath, "TDEPS")
	if _, err := os.Stat(tdepsPath); err != nil {
		if os.IsNotExist(err) {
			// if TDEPS does not exist, it means that the package doesn't have any dependencies (I'm looking at you automate-cli)
			return []habpkg.HabPkg{}, nil
		}
		return nil, errors.Wrapf(err, "Could not read dependencies from %s", tdepsPath)
	}

	data, err := ioutil.ReadFile(tdepsPath)
	if err != nil {
		return nil, errors.Wrap(err, "Could not read TDEPS")
	}

	tdeps, err := habpkg.FromStrings(string(data))
	if err != nil {
		return nil, errors.Wrap(err, "Failed to parse TDEPS")
	}

	return tdeps, nil
}

// DownloadPackage downloads the hartifact for the given package
func (local *LocalCache) DownloadPackage(pkg habpkg.VersionedPackage, w io.Writer) (*HartHeader, error) {
	pkgPath := path.Join(local.root, "cache", "artifacts", fmt.Sprintf("%s-%s-%s-%s-x86_64-linux.hart", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release()))
	f, err := os.Open(pkgPath)
	if err != nil {
		return nil, errors.Wrapf(err, "Could not open hartifact for %s/%s/%s/%s at %s. Make sure the package has been installed",
			pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release(), pkgPath)
	}

	defer f.Close()
	return copyPackage(f, w)
}

// DownloadOriginKey downloads the public origin key
func (local *LocalCache) DownloadOriginKey(keyName OriginKeyName, w io.Writer) error {
	keyPath := path.Join(local.root, "cache", "keys", fmt.Sprintf("%s.pub", string(keyName)))
	f, err := os.Open(keyPath)
	if err != nil {
		return errors.Wrapf(err, "Could not open key for %s at %s", string(keyName), keyPath)
	}
	defer f.Close()

	_, err = io.Copy(w, f)
	if err != nil {
		return errors.Wrapf(err, "Failed copy public key %s", keyName)
	}

	return nil
}

// ListAllPackages lists all locally installed habitat packages
func (local *LocalCache) ListAllPackages() ([]habpkg.HabPkg, error) {
	pkgsPath := path.Join(local.root, "pkgs")
	pkgs := []habpkg.HabPkg{}
	err := filepath.Walk(pkgsPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			return nil
		}

		if strings.Contains(path, ".hab-pkg-install") {
			return nil
		}

		relPath, err := filepath.Rel(pkgsPath, path)
		if err != nil {
			return errors.Wrapf(err, "Could not get relative path for %s", path)
		}

		parts := strings.Split(relPath, "/")
		if len(parts) != 4 {
			return nil
		}

		pkgs = append(pkgs, habpkg.NewFQ(parts[0], parts[1], parts[2], parts[3]))
		return filepath.SkipDir
	})

	return pkgs, err
}

func (local *LocalCache) Delete(pkg habpkg.HabPkg) error {
	if !habpkg.IsFullyQualified(&pkg) {
		return errors.Errorf("%s must be fully qualified to delete", pkg)
	}

	pkgPath := path.Join(local.root, "pkgs", pkg.InstallIdent())
	if _, err := os.Stat(pkgPath); err != nil {
		if !os.IsNotExist(err) {
			return errors.Wrapf(err, "Could not access %s", pkgPath)
		}
	} else {
		if err := os.RemoveAll(pkgPath); err != nil {
			return errors.Wrapf(err, "Failed to delete %s", pkgPath)
		}
	}

	hartPath := path.Join(local.root, "cache", "artifacts", strings.Join([]string{
		pkg.Origin(),
		pkg.Name(),
		pkg.Version(),
		pkg.Release(),
		"x86_64-linux.hart",
	}, "-"))

	if _, err := os.Stat(hartPath); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return errors.Wrapf(err, "Could not access %s", hartPath)
	}

	if err := os.Remove(hartPath); err != nil {
		return errors.Wrapf(err, "Failed to delete %s", hartPath)
	}

	return nil
}
