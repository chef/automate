package main

import (
	"archive/tar"
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

func generateMiniBackendBundles(airgapMetadata airgap.UnpackMetadata, bundleName string) error {
	writer.Println("Generating backend bundles for Automate HA")
	lists := getMiniBackendBundlePackages(airgapMetadata)
	return createTar(lists, airgapMetadata, bundleName)
}

func getMiniBackendBundlePackages(airgapMetadata airgap.UnpackMetadata) []string {
	var requiredPkgs []string
	for _, pkgPath := range airgapMetadata.HartifactPaths {
		for _, haBackendPkg := range AUTOMATE_HA_BACKEND_PKGS {
			if strings.Contains(pkgPath, haBackendPkg) {
				requiredPkgs = append(requiredPkgs, pkgPath)
			}
		}
	}
	return requiredPkgs
}

func getHABackendManifestPkgs(manifest *manifest.A2) []habpkg.HabPkg {
	var requiredHABackendPkgs []habpkg.HabPkg
	for _, pkg := range manifest.Packages {
		for _, haBackendPkgs := range AUTOMATE_HA_BACKEND_PKGS {
			if strings.Contains(pkg.Name(), haBackendPkgs) {
				requiredHABackendPkgs = append(requiredHABackendPkgs, pkg)
			}
		}
	}
	return requiredHABackendPkgs
}

func writeManifest(workspacePath string, airgapMetadata airgap.UnpackMetadata) error {
	manifestData, err := ioutil.ReadFile(airgapMetadata.ManifestPath) // nosemgrep
	if err != nil {
		writer.FailWrap(err, "reading manifest data")
		os.Exit(1)
	}
	manifest := &manifest.A2{}
	err = json.Unmarshal(manifestData, manifest)
	if err != nil {
		writer.FailWrap(err, "parsing manifest data")
		os.Exit(1)
	}
	manifest.Packages = getHABackendManifestPkgs(manifest)
	automateHABackendManifest, err := json.Marshal(manifest)
	if err != nil {
		return err
	}
	err = fileutils.AtomicWrite(path.Join(workspacePath, "manifest.json"), bytes.NewReader(automateHABackendManifest))
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing the manifest.json failed")
	}
	return nil
}

func createTar(pkgs []string, airgapMetadata airgap.UnpackMetadata, bundleName string) error {
	workspacePath, err := ioutil.TempDir("", "aib-workspace") // nosemgrep
	if err != nil {
		return err
	}
	// copy packages to workspace path
	workspaceAirtifactPath := path.Join(workspacePath, "hab", "cache", "artifacts")
	// copy keys to workspace path
	workspaceKeysPath := path.Join(workspacePath, "hab", "cache", "keys")
	// copy hab to worksapce path
	workspaceHabBinPath := path.Join(workspacePath, "bin")

	for _, d := range []string{workspacePath, workspaceAirtifactPath, workspaceKeysPath, workspaceHabBinPath} {
		if err := os.MkdirAll(d, 0755); err != nil {
			return status.Wrapf(err, status.FileAccessError, "Creating directory %s failed", d)
		}
	}

	// copy pkgs
	for _, pkgFiles := range pkgs {
		copyFileContents(pkgFiles, path.Join(workspaceAirtifactPath, filepath.Base(pkgFiles)))
	}

	// copy keys
	for _, keyFiles := range airgapMetadata.OriginKeyPaths {
		copyFileContents(keyFiles, path.Join(workspaceKeysPath, filepath.Base(keyFiles)))
	}

	//copyhab
	copyFileContents(airgapMetadata.HabBinPath, path.Join(workspaceHabBinPath, filepath.Base(airgapMetadata.HabBinPath)))

	//copyManifest
	writeManifest(workspacePath, airgapMetadata)

	err = fileutils.AtomicWriter(bundleName, func(w io.Writer) error {
		tarWriter := tar.NewWriter(w)

		err := filepath.Walk(workspacePath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to create archive")
			}

			if workspacePath == path {
				return nil
			}

			header, err := tar.FileInfoHeader(info, path)
			if err != nil {
				return status.Wrap(err, status.FileAccessError, "Failed to create archive file header")
			}
			p, err := filepath.Rel(workspacePath, path)
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
	err = generateChecksumFile(bundleName, bundleName+".md5")
	if err != nil {
		return err
	}
	return nil
}
