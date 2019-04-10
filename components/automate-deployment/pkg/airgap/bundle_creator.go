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

type bintrayHabDownloader struct{}

// NewBintrayHabDownloader returns a new HabBinaryDownloader that pulls from bintray
func NewBintrayHabDownloader() HabBinaryDownloader {
	return &bintrayHabDownloader{}
}

func (dl *bintrayHabDownloader) DownloadHabBinary(version string, release string, w io.Writer) error {
	resp, err := http.Get(fmt.Sprintf("https://habitat.bintray.com/stable/linux/x86_64/hab-%s-%s-x86_64-linux.tar.gz",
		version, release))

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
	Downloading(name string)
	DownloadComplete(name string, wasCached bool)
}

type noopInstallBundleCreatorProgress struct{}

func (noopInstallBundleCreatorProgress) Downloading(string)            {}
func (noopInstallBundleCreatorProgress) DownloadComplete(string, bool) {}

// InstallBundleCreator creates installation bundles
type InstallBundleCreator struct {
	manifestFile        string
	channel             string
	outputFile          string
	workspacePath       string
	depotClient         depot.Client
	hartCache           HartifactCache
	keyCache            KeyCache
	habBinaryDownloader HabBinaryDownloader

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

// WithInstallBundleWorkspacePath sets the path for caching artifacts
func WithInstallBundleWorkspacePath(path string) InstallBundleCreatorOpt {
	return func(c *InstallBundleCreator) {
		c.workspacePath = path
	}
}

// NewInstallBundleCreator initializes a new artifact creator
func NewInstallBundleCreator(opts ...InstallBundleCreatorOpt) *InstallBundleCreator {
	creator := &InstallBundleCreator{
		channel:             "current",
		depotClient:         depot.NewClient(),
		habBinaryDownloader: &bintrayHabDownloader{},
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
		progress.Downloading(habBinaryName)
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
		name := fmt.Sprintf("%s/%s/%s/%s", p.Origin(), p.Name(), p.Version(), p.Release())
		progress.Downloading(name)
		if !creator.hartCache.IsCached(p) {
			// Cache the artifact. If we successfully download the hart
			// but fail to get the key, we will not cache the hart
			err := creator.hartCache.CacheArtifact(p, func(writer io.Writer) error {
				header, err := depotClient.DownloadPackage(p, writer)
				if err == nil {
					if !creator.keyCache.IsCached(header.KeyName) {
						return creator.keyCache.CacheKey(header.KeyName, func(writer io.Writer) error {
							return depotClient.DownloadOriginKey(header.KeyName, writer)
						})
					}
				}
				return err
			})

			if err != nil {
				return status.Wrapf(err, status.DownloadError, "Failed to download package %s", p)
			}
			progress.DownloadComplete(name, false)
		} else {
			progress.DownloadComplete(name, true)
		}
	}
	return nil
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
