package airgap

// An InstallBundle is a binary file with a magic number AIB-1\n\n followed by a tar:
//
// hab/cache/artifacts/
//     contains all the required hartifacts. These will be moved into /hab/cache/artifacts
//
// hab/cache/keys/
//     contains all the keys for the artifacts. These will be moved into /hab/cache/keys
//
// bin/hab
//     is the hab executable to use for bootstrapping
//
// manifest.json
//     is the manifest that uses the bundled harts. deployment-service will be tied to this
//     version of the manifest until an update is installed.

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/io/fileutils"
)

// HabBinaryDownloader can download a hab binary
type HabBinaryDownloader interface {
	// Download the hab binary for the given version, release tuple. It is copied
	// to the writer
	DownloadHabBinary(version string, release string, w io.Writer) error
}

const (
	// Last version for which hab tars lived on bintray
	lastBintrayVersion = "0.89.0"

	pcioBaseURLFmt    = "https://packages.chef.io/files/habitat/%s/hab-x86_64-linux.tar.gz"
	bintrayBaseURLFmt = "https://habitat.bintray.com/stable/linux/x86_64/hab-%s-%s-x86_64-linux.tar.gz"
)

// netHabDownloader downloads the habitat binary from either
// habitat.bintray.com or packages.chef.io.
type netHabDownloader struct{}

// NewNetHabDownloader returns a new HabBinaryDownloader that pulls
// from bintray or packages.chef.io based on the version being
// requested.
func NewNetHabDownloader() HabBinaryDownloader {
	return &netHabDownloader{}
}

func urlForVersion(version string, release string) (string, error) {
	targetVersion, err := habpkg.ParseSemverishVersion(version)
	if err != nil {
		return "", err
	}

	switchoverVersion, err := habpkg.ParseSemverishVersion(lastBintrayVersion)
	if err != nil {
		return "", err
	}

	if habpkg.CompareSemverish(targetVersion, switchoverVersion) == habpkg.SemverishGreater {
		return fmt.Sprintf(pcioBaseURLFmt, version), nil
	} else {
		return fmt.Sprintf(bintrayBaseURLFmt, version, release), nil
	}
}

func (dl *netHabDownloader) DownloadHabBinary(version string, release string, w io.Writer) error {
	url, err := urlForVersion(version, release)
	if err != nil {
		return errors.Wrap(err, "Could not determine hab binary download URL")
	}

	resp, err := http.Get(url)
	if err != nil {
		return errors.Wrap(err, "Could not download hab binary")
	}
	defer resp.Body.Close() // nolint errcheck

	if resp.StatusCode != 200 {
		return errors.Errorf("Could not get hab binary. Got status %s", resp.Status)
	}

	gzipReader, err := gzip.NewReader(resp.Body)

	if err != nil {
		return errors.Wrap(err, "Failed to download hab binary. Could not open gzip")
	}

	tarReader := tar.NewReader(gzipReader)
	for {
		hdr, err := tarReader.Next()
		if err == io.EOF {
			break
		}

		if err != nil {
			return errors.Wrap(err, "Failed to unpack archive")
		}

		if hdr.Typeflag == tar.TypeReg {
			if hdr.FileInfo().Name() == "hab" {
				_, err := io.Copy(w, tarReader)
				if err != nil {
					return errors.Wrap(err, "Could not untar hab binary tarball")
				}
				return nil
			}
		}
	}

	return errors.New("Could not find hab binary in tarball")
}

// InstallBundleCreatorProgress gets progress reports from the creator
type InstallBundleCreatorProgress interface {
	Downloading(name string, tries int)
	DownloadComplete(name string, wasCached bool)
	RetriableDownloadError(name string, info string, delay time.Duration)
}

type noopInstallBundleCreatorProgress struct{}

func (noopInstallBundleCreatorProgress) Downloading(string, int)                              {}
func (noopInstallBundleCreatorProgress) DownloadComplete(string, bool)                        {}
func (noopInstallBundleCreatorProgress) RetriableDownloadError(string, string, time.Duration) {}

var _ InstallBundleCreatorProgress = noopInstallBundleCreatorProgress{}

// InstallBundleCreator creates installation bundles
type InstallBundleCreator struct {
	manifestFile        string
	channel             string
	version             string
	outputFile          string
	workspacePath       string
	depotClient         depot.Client
	hartCache           HartifactCache
	keyCache            KeyCache
	habBinaryDownloader HabBinaryDownloader
	retries             int
	retryDelay          int

	// For development
	hartifactsPath string
	overrideOrigin string
}

// InstallBundleCreatorOpt are functional options for the InstallBundleCreator
type InstallBundleCreatorOpt func(*InstallBundleCreator)

// WithInstallBundleOutputPath tells the install bundle creator the exact file to use
func WithInstallBundleOutputPath(outputPath string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.outputFile = outputPath
	}
}

// WithInstallBundleHartifactsPath sets the path to search for override harts
func WithInstallBundleHartifactsPath(hartifactsPath string, overrideOrigin string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.overrideOrigin = overrideOrigin
		c.hartifactsPath = hartifactsPath
	}
}

// WithInstallBundleManifestFile sets the path for manifest
func WithInstallBundleManifestFile(path string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.manifestFile = path
	}
}

// WithInstallBundleChannel sets the release channel from which to download
// the manifest.
func WithInstallBundleChannel(channel string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.channel = channel
	}
}

// WithInstallBundleVersion sets the version whose manifest should be used.
func WithInstallBundleVersion(version string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.version = version
	}
}

// WithInstallBundleWorkspacePath sets the path for caching artifacts
func WithInstallBundleWorkspacePath(path string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.workspacePath = path
	}
}

// WithInstallBundleRetries sets the number of retries to
// attempt for failed package downloads.
func WithInstallBundleRetries(r int) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.retries = r
	}
}

// WithInstallBundleRetryDelay sets the number of seconds between
// retries, if not set, an exponential backoff based on the number of
// tries is used.
func WithInstallBundleRetryDelay(d int) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.retryDelay = d
	}
}

// NewInstallBundleCreator initializes a new artifact creator
func NewInstallBundleCreator(opts ...InstallBundleCreatorOpt) *InstallBundleCreator {
	creator := &InstallBundleCreator{
		channel:             "current",
		depotClient:         depot.NewClient(),
		habBinaryDownloader: &netHabDownloader{},
		retryDelay:          -1, // auto-calculate by default
	}

	for _, o := range opts {
		o(creator)
	}

	creator.hartCache = NewHartifactCache(creator.hartCacheDir())
	creator.keyCache = NewKeyCache(creator.keyCacheDir())

	return creator
}

// Create creates the install bundle
func (creator *InstallBundleCreator) Create(progress InstallBundleCreatorProgress) (string, error) {
	m, err := creator.loadManifest()
	if err != nil {
		return "", err
	}

	if creator.outputFile == "" {
		creator.outputFile = fmt.Sprintf("automate-%s.aib", m.Build)
	}

	if err := creator.createDirectories(); err != nil {
		return "", err
	}

	if err := creator.downloadHab(m, progress); err != nil {
		return "", err
	}

	if err := creator.gatherOverrides(m, progress); err != nil {
		return "", err
	}

	if err := creator.downloadDependencies(m, progress); err != nil {
		return "", err
	}

	// Write the manifest after downloading dependencies because the
	// written manifest will remove the hartifact override information.
	if err := creator.writeManifest(m); err != nil {
		return "", err
	}

	if err := creator.createTar(progress); err != nil {
		return "", err
	}

	return creator.outputFile, nil
}

func (creator *InstallBundleCreator) hartCacheDir() string {
	return path.Join(creator.workspacePath, "hab", "cache", "artifacts")
}

func (creator *InstallBundleCreator) keyCacheDir() string {
	return path.Join(creator.workspacePath, "hab", "cache", "keys")
}

func (creator *InstallBundleCreator) binDir() string {
	return path.Join(creator.workspacePath, "bin")
}

func (creator *InstallBundleCreator) createDirectories() error {
	for _, d := range []string{creator.workspacePath, creator.binDir(), creator.hartCacheDir(), creator.keyCacheDir()} {
		if err := os.MkdirAll(d, 0755); err != nil {
			return status.Wrapf(err, status.FileAccessError, "Creating directory %s failed", d)
		}
	}
	return nil
}

// mergeOverridesIntoPackages takes an A2 manifest and merges the overrides
// into the regular packages. This allows us to do development and testing
// with overrides without placing the override artifacts in the same location.
func (creator *InstallBundleCreator) mergeOverridesIntoPackages(m *manifest.A2) {
	if len(m.HartOverrides) == 0 {
		return
	}

	// Gather all packages that don't have a hart override
	packages := []habpkg.HabPkg{}
	for _, pkg := range m.Packages {
		isOverride := false
		for _, override := range m.HartOverrides {
			if pkg.Name() == override.Name() {
				isOverride = true
			}
		}

		if !isOverride {
			packages = append(packages, pkg)
		}
	}

	// Convert all override harts into hab packages and add them to the packages
	for _, hart := range m.HartOverrides {
		pkg := habpkg.NewFQ(
			hart.Origin(),
			hart.Name(),
			hart.Version(),
			hart.Release(),
		)

		packages = append(packages, pkg)
	}

	m.Packages = packages
	m.HartOverrides = []habpkg.Hart{}
}

// writeManifest takes the manifest and writes it into the bundles workspace.
// If the manifest has local hart overrides it will update them to be normal
// package dependencies and clear the overrides so that upon installation the
// override hartifacts will be found.
func (creator *InstallBundleCreator) writeManifest(m *manifest.A2) error {
	creator.mergeOverridesIntoPackages(m)

	data, err := json.Marshal(m)
	if err != nil {
		return status.Wrap(err, status.MarshalError, "Marshaling the manifest.json failed")
	}

	err = fileutils.AtomicWrite(path.Join(creator.workspacePath, "manifest.json"), bytes.NewReader(data))
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing the manifest.json failed")
	}

	return nil
}

func (creator *InstallBundleCreator) loadManifest() (*manifest.A2, error) {
	var manifestProvider manifest.ReleaseManifestProvider
	manifestProvider = client.NewDefaultClient(creator.manifestFile)

	if creator.hartifactsPath != "" && creator.overrideOrigin != "" {
		manifestProvider = manifest.NewLocalHartManifestProvider(manifestProvider, creator.hartifactsPath, creator.overrideOrigin)
	}

	if creator.version != "" {
		return manifestProvider.GetManifest(context.Background(), creator.version)
	}
	return manifestProvider.GetCurrentManifest(context.Background(), creator.channel)
}

func (creator *InstallBundleCreator) downloadHab(m *manifest.A2, progress InstallBundleCreatorProgress) error {
	habInManifest, hab := m.PackageForServiceName("hab")
	if !habInManifest {
		return errors.New("hab not in the manifest")
	}

	habFilePath := path.Join(creator.binDir(), "hab")

	habBinaryName := fmt.Sprintf("%s binary", habpkg.Ident(&hab))
	err := fileutils.AtomicWriter(habFilePath, func(w io.Writer) error {
		progress.Downloading(habBinaryName, 1)
		err := creator.habBinaryDownloader.DownloadHabBinary(hab.Version(), hab.Release(), w)
		if err != nil {
			return errors.Wrap(err, "Failed to download hab binary")
		}
		return nil
	}, fileutils.WithAtomicWriteFileMode(0755))

	if err != nil {
		return status.Wrap(err, status.DownloadError, "Failed to get the hab binary")
	}

	progress.DownloadComplete(habBinaryName, false)

	return nil
}

func (creator *InstallBundleCreator) gatherOverrides(m *manifest.A2, progress InstallBundleCreatorProgress) error {
	if m.HartOverrides != nil && len(m.HartOverrides) > 0 {
		localDepotCache := depot.FromLocalCache()
		versionedPkgs := make([]habpkg.VersionedPackage, len(m.HartOverrides))
		for i, p := range m.HartOverrides {
			pkg := p
			versionedPkgs[i] = &pkg
		}
		return creator.fetchDependencies(localDepotCache, versionedPkgs, progress)
	}
	return nil
}

func (creator *InstallBundleCreator) downloadDependencies(m *manifest.A2, progress InstallBundleCreatorProgress) error {
	versionedPkgs := make([]habpkg.VersionedPackage, 0, len(m.Packages))
	for _, p := range m.Packages {
		// Skip anything that we have a hart override
		// for since we have already gathered those.
		hasOverride := false
		for _, h := range m.HartOverrides {
			if h.Name() == p.Name() {
				hasOverride = true
			}
		}

		if !hasOverride {
			pkg := p
			versionedPkgs = append(versionedPkgs, &pkg)
		}
	}
	return creator.fetchDependencies(creator.depotClient, versionedPkgs, progress)
}

func (creator *InstallBundleCreator) fetchDependencies(depotClient depot.Client, pkgs []habpkg.VersionedPackage, progress InstallBundleCreatorProgress) error {
	deps := []habpkg.VersionedPackage{}

	for _, pkg := range pkgs {
		tdeps, err := depotClient.TDepsForPackage(pkg)
		if err != nil {
			return status.Wrap(err, status.DownloadError, "Could not get package deps")
		}
		for _, tdep := range tdeps {
			d := tdep
			deps = append(deps, &d)
		}
		deps = append(deps, pkg)
	}

	deps = uniq(deps)

	for _, p := range deps {
		err := creator.downloadPackageAndKeys(p, depotClient, progress)
		if err != nil {
			return status.Wrapf(err, status.DownloadError, "Failed to download package %s", p)
		}
	}
	return nil
}

func (creator *InstallBundleCreator) downloadPackageAndKeys(p habpkg.VersionedPackage,
	depotClient depot.Client, progress InstallBundleCreatorProgress) error {
	if creator.hartCache.IsCached(p) {
		// Since the hart is only cached if the key is
		// successfully cached, we know we have everything if
		// the hart is in the cache.
		name := habpkg.Ident(p)
		progress.Downloading(name, 1)
		progress.DownloadComplete(name, true)
		return nil
	}

	return creator.downloadWithRetries(p, depotClient, progress)
}

func (creator *InstallBundleCreator) downloadWithRetries(p habpkg.VersionedPackage, depotClient depot.Client, progress InstallBundleCreatorProgress) error {
	name := habpkg.Ident(p)
	return creator.withRetry(func(tryCount int, ri retryInfo) error {
		return creator.hartCache.CacheArtifact(p, func(writer io.Writer) error {
			progress.Downloading(name, tryCount)
			header, err := depotClient.DownloadPackage(p, writer)
			if err != nil {
				if ri.willRetry {
					progress.RetriableDownloadError(name, err.Error(), ri.delay)
				}
				return err
			}

			progress.DownloadComplete(name, false)
			if creator.keyCache.IsCached(header.KeyName) {
				return nil // We don't log here since nearly every key is cached.
			}

			// NOTE(ssd) 2019-07-16: If this download
			// fails, we unfortunately have to retry the
			// package download as well. This is a bit
			// odd, but I think more refactoring is needed
			// to disentangle them.
			progress.Downloading(string(header.KeyName), tryCount)
			err = creator.keyCache.CacheKey(header.KeyName, func(writer io.Writer) error {
				return depotClient.DownloadOriginKey(header.KeyName, writer)
			})
			if err != nil {
				if ri.willRetry {
					progress.RetriableDownloadError(string(header.KeyName), err.Error(), ri.delay)
				}
				return err
			}
			progress.DownloadComplete(string(header.KeyName), false)
			return nil
		})
	})
}

type retryInfo struct {
	willRetry bool
	delay     time.Duration
}

type retriableFunc func(int, retryInfo) error

func (creator *InstallBundleCreator) withRetry(f retriableFunc) error {
	var err error
	var nextRetryDelay time.Duration

	for try := 1; try <= creator.retries+1; try++ {
		if nextRetryDelay > 0 {
			time.Sleep(nextRetryDelay)
		}

		if creator.retryDelay >= 0 {
			nextRetryDelay = time.Second * time.Duration(creator.retryDelay)
		} else {
			nextRetryDelay = time.Second * time.Duration(1<<uint(try))
		}

		err = f(try, retryInfo{
			willRetry: try+1 <= creator.retries+1,
			delay:     nextRetryDelay})
		if err == nil {
			return nil
		}
	}
	return err
}

func (creator *InstallBundleCreator) createTar(InstallBundleCreatorProgress) error {
	err := fileutils.AtomicWriter(creator.outputFile, func(w io.Writer) error {
		if _, err := io.Copy(w, strings.NewReader("AIB-1\n\n")); err != nil {
			return errors.Wrap(err, "Could not write archive header")
		}

		tarWriter := tar.NewWriter(w)

		err := filepath.Walk(creator.workspacePath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to create archive")
			}

			if creator.workspacePath == path {
				return nil
			}

			header, err := tar.FileInfoHeader(info, path)
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to create archive file header")
			}
			p, err := filepath.Rel(creator.workspacePath, path)
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to get base archive path")
			}

			header.Name = p

			if err := tarWriter.WriteHeader(header); err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to write archive file header")
			}

			if info.IsDir() {
				return nil
			}

			file, err := os.Open(path)
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to open archive file")
			}
			defer file.Close() // nolint errcheck
			_, err = io.Copy(tarWriter, file)

			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to write archive file")
			}

			return file.Close()
		})

		if err != nil {
			return err
		}

		if err := tarWriter.Close(); err != nil {
			return status.Wrap(err, status.FileAccessError, "Failed to close archive")
		}

		return nil
	})

	if err != nil {
		return errors.Wrap(err, "Could not write archive")
	}

	return err
}

func uniq(xs []habpkg.VersionedPackage) []habpkg.VersionedPackage {
	m := make(map[string]bool)
	ys := []habpkg.VersionedPackage{}

	for _, x := range xs {
		ident := fmt.Sprintf("%s/%s/%s/%s", x.Origin(), x.Name(), x.Version(), x.Release())
		if ok, _ := m[ident]; !ok {
			ys = append(ys, x)
			m[ident] = true
		}
	}

	return ys
}
