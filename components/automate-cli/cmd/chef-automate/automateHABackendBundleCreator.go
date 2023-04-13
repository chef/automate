package main

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
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

// BackendBundleCreatorcreates installation bundles
type BackendBundleCreator struct {
	manifestFile  string
	channel       string
	version       string
	outputFile    string
	workspacePath string
	hartCache     airgap.HartifactCache
	keyCache      airgap.KeyCache

	// For development
	hartifactsPath string
	overrideOrigin string

	//only for testing
	optionalURL  string
	versionsPath string
}

// InstallBundleCreatorOpt are functional options for the InstallBundleCreator
type InstallBundleCreatorOpt func(*BackendBundleCreator)

// WithInstallBundleOutputPath tells the install bundle creator the exact file to use
func WithInstallBundleOutputPath(outputPath string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.outputFile = outputPath
	}
}

// WithInstallBundleHartifactsPath sets the path to search for override harts
func WithInstallBundleHartifactsPath(hartifactsPath string, overrideOrigin string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.overrideOrigin = overrideOrigin
		c.hartifactsPath = hartifactsPath
	}
}

// WithInstallBundleVersionsPath sets the path to search for override versions
func WithInstallBundleVersionsPath(versionsPath string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.versionsPath = versionsPath
	}
}

// WithInstallBundleManifestFile sets the path for manifest
func WithInstallBundleManifestFile(path string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.manifestFile = path
	}
}

// WithInstallBundleChannel sets the release channel from which to download
// the manifest.
func WithInstallBundleChannel(channel string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.channel = channel
	}
}

// WithInstallBundleVersion sets the version whose manifest should be used.
func WithInstallBundleVersion(version string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.version = version
	}
}

// WithInstallBundleWorkspacePath sets the path for caching artifacts
func WithInstallBundleWorkspacePath(path string) InstallBundleCreatorOpt {
	return func(c *BackendBundleCreator) {
		c.workspacePath = path
	}
}

// NewBackendBundleCreatorinitializes a new artifact creator
func NewInstallBundleCreator(opts ...InstallBundleCreatorOpt) *BackendBundleCreator {
	creator := &BackendBundleCreator{
		channel: "current",
	}

	for _, o := range opts {
		o(creator)
	}

	creator.hartCache = airgap.NewHartifactCache(creator.hartCacheDir())
	creator.keyCache = airgap.NewKeyCache(creator.keyCacheDir())

	return creator
}

// Create creates the bundle
func (creator *BackendBundleCreator) Create(requiredBackendPackages []string) (string, error) {
	m, err := creator.loadManifest()
	if err != nil {
		return "", err
	}

	if creator.outputFile == "" {
		creator.outputFile = fmt.Sprintf("automate-%s.aib", m.Version())
	}

	if err := creator.createDirectories(); err != nil {
		return "", err
	}

	// Write the manifest after downloading dependencies because the
	// written manifest will remove the hartifact override information.
	if err := creator.writeManifest(m); err != nil {
		return "", err
	}

	if err := creator.createTar(requiredBackendPackages); err != nil {
		return "", err
	}

	return creator.outputFile, nil
}

func (creator *BackendBundleCreator) hartCacheDir() string {
	return path.Join(creator.workspacePath, "hab", "cache", "artifacts")
}

func (creator *BackendBundleCreator) keyCacheDir() string {
	return path.Join(creator.workspacePath, "hab", "cache", "keys")
}

func (creator *BackendBundleCreator) binDir() string {
	return path.Join(creator.workspacePath, "bin")
}

func (creator *BackendBundleCreator) createDirectories() error {
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
func (creator *BackendBundleCreator) mergeOverridesIntoPackages(m *manifest.A2) {
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
func (creator *BackendBundleCreator) writeManifest(m *manifest.A2) error {
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

func (creator *BackendBundleCreator) loadManifest() (*manifest.A2, error) {
	var manifestProvider manifest.ReleaseManifestProvider
	manifestProvider = client.NewDefaultClient(creator.manifestFile)

	if creator.hartifactsPath != "" && creator.overrideOrigin != "" {
		manifestProvider = manifest.NewLocalHartManifestProvider(manifestProvider, creator.hartifactsPath, creator.overrideOrigin)
	}

	var m *manifest.A2
	var err error
	ctx := context.Background()
	if creator.version != "" {
		m, err = manifestProvider.GetManifest(ctx, creator.version)
	} else {
		m, err = manifestProvider.GetCurrentManifest(ctx, creator.channel)
	}
	if err != nil {
		return m, err
	}

	minCurrentVersion, err := manifest.GetMinimumCurrentManifestVersion(ctx, m.Version(), creator.channel, creator.versionsPath, creator.optionalURL)
	if err != nil {
		return nil, err
	}
	m.MinCompatibleVer = minCurrentVersion
	return m, nil
}

func (creator *BackendBundleCreator) createTar(requiredBackendPackages []string) error {
	err := fileutils.AtomicWriter(creator.outputFile, func(w io.Writer) error {
	tarWriter := tar.NewWriter(w)
	defer tarWriter.Close()
	var airgapMetadata airgap.UnpackMetadata
	requiredBackendPackages = backendBundles(airgapMetadata)
	for _, path := range requiredBackendPackages{
		var info os.FileInfo
		header, err := tar.FileInfoHeader(info, info.Name())
		if err != nil {
			return status.Wrap(err, status.FileAccessError, "Failed to create archive file header")
		}
		p, err := filepath.Rel(creator.workspacePath, path)
		if err != nil {
			return status.Wrap(err, status.FileAccessError, "Failed to get base archive path")
		}

		header.Name = p

		// write the header
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

		// copy file data into tar writer
		if _, err := io.Copy(tarWriter, file); err != nil {
			return status.Wrap(err, status.FileAccessError, "Failed to write archive file")
		}

		// manually close here after each file operation; defering would cause each file close
		// to wait until all operations have completed.
		file.Close()
	}
	if err := tarWriter.Close(); err != nil {
		return status.Wrap(err, status.FileAccessError, "Failed to close archive")
	}

	return nil
	})
	if err != nil {
		return errors.Wrap(err, "Could not write archive")
	}
	return nil
}
