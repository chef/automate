package airgap

import (
	"fmt"
	"io"
	"os"
	"path"

	"github.com/chef/automate/lib/io/fileutils"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// FileCacher caches files
type FileCacher interface {
	IsCached(filename string) bool
	CacheFile(filename string, w func(io.Writer) error) error
}

type fileCacher struct {
	cacheDir string
}

func (cache *fileCacher) IsCached(filename string) bool {
	_, err := os.Stat(path.Join(cache.cacheDir, filename))
	return err == nil
}

func (cache *fileCacher) CacheFile(filename string, w func(io.Writer) error) error {
	filepath := path.Join(cache.cacheDir, filename)
	err := fileutils.AtomicWriter(filepath, w)
	if err != nil {
		return errors.Wrap(err, "Could not cache artifact")
	}
	return nil
}

// HartifactCache stores harts
type HartifactCache interface {
	// IsCached returns true if the given package already exists
	IsCached(habpkg.VersionedPackage) bool
	// CacheArtifact stores harts in the cache
	CacheArtifact(habpkg.VersionedPackage, func(io.Writer) error) error
}

type hartifactCache struct {
	cacher FileCacher
}

// NewHartifactCache returns a new HartifactCache
func NewHartifactCache(cacheDir string) HartifactCache {
	return &hartifactCache{
		cacher: &fileCacher{
			cacheDir: cacheDir,
		},
	}
}

func (cache *hartifactCache) IsCached(pkg habpkg.VersionedPackage) bool {
	return cache.cacher.IsCached(hartFilename(pkg))
}

func (cache *hartifactCache) CacheArtifact(pkg habpkg.VersionedPackage, w func(io.Writer) error) error {
	return cache.cacher.CacheFile(hartFilename(pkg), w)
}

func hartFilename(pkg habpkg.VersionedPackage) string {
	return fmt.Sprintf("%s-%s-%s-%s-x86_64-linux.hart", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
}

// KeyCache stores origin public keys
type KeyCache interface {
	// IsCached returns true if the given key already exists
	IsCached(depot.OriginKeyName) bool
	// CacheKey stores the key
	CacheKey(depot.OriginKeyName, func(io.Writer) error) error
}

type keyCache struct {
	cacher FileCacher
}

// NewKeyCache returns a new KeyCache
func NewKeyCache(cacheDir string) KeyCache {
	return &keyCache{
		cacher: &fileCacher{
			cacheDir: cacheDir,
		},
	}
}

func (cache *keyCache) IsCached(keyName depot.OriginKeyName) bool {
	return cache.cacher.IsCached(keyFilename(keyName))
}

func (cache *keyCache) CacheKey(keyName depot.OriginKeyName, w func(io.Writer) error) error {
	return cache.cacher.CacheFile(keyFilename(keyName), w)
}

func keyFilename(keyName depot.OriginKeyName) string {
	return fmt.Sprintf("%s.pub", string(keyName))
}
