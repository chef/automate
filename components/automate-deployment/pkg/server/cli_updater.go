package server

import (
	"crypto/sha256"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/io/chunks"
)

func sha256sumExecutable(path string) error {
	checksumBytes, err := ioutil.ReadFile(path + ".sha256sum")
	if err != nil {
		return errors.Wrapf(err, "Could not open sha256sum file for %s", path)
	}

	expectedChecksum := strings.TrimSpace(string(checksumBytes))

	f, err := os.Open(path)
	if err != nil {
		return errors.Wrap(err, "Failed to open executable")
	}
	defer f.Close()

	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return errors.Wrap(err, "Failed to sha256sum executable")
	}

	foundChecksum := fmt.Sprintf("%x", h.Sum(nil))

	if foundChecksum != expectedChecksum {
		return errors.Errorf("Incorrect sha256 for %s, Found %s; Expected %s.",
			path, foundChecksum, expectedChecksum)
	}
	return nil
}

func (s *server) GetCLIExecutable(d *api.GetCLIExecutableRequest, stream api.Deployment_GetCLIExecutableServer) error {
	if s.deployment == nil {
		return ErrorNoDeployment
	}

	if s.deployment.CurrentReleaseManifest == nil {
		return ErrorNoReleaseManifest
	}

	executable := "chef-automate"
	platform := d.Platform
	switch platform {
	case "linux", "":
		platform = "linux"
	default:
		return status.Errorf(codes.NotFound, "No executable for platform '%s'", platform)
	}

	m := s.deployment.CurrentReleaseManifest
	pkg := manifest.VersionedPackageFromManifest(m, "automate-cli")
	if pkg == nil {
		logrus.Error("automate-cli not found in manifest")
		return errors.New("automate-cli not found in manifest")
	}

	logctx := logrus.WithFields(logrus.Fields{
		"origin":   pkg.Origin(),
		"name":     pkg.Name(),
		"version":  pkg.Version(),
		"release":  pkg.Release(),
		"platform": platform,
	})

	packagePath := path.Join("/hab/pkgs/", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	executablePath := path.Join(packagePath, "static", platform, executable)

	err := sha256sumExecutable(executablePath)
	if err != nil {
		logrus.WithError(err).
			WithField("executablePath", executablePath).
			Error("sha256 sum check failed")
		return err
	}

	f, err := os.Open(executablePath)
	if err != nil {
		logctx.WithError(err).Error("Failed to open cli executable")
		return err
	}
	defer f.Close()

	buffer := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.GetCLIExecutableResponse{
			Version: pkg.Release(),
			Data:    p,
		})
	})
	_, err = io.CopyBuffer(writer, f, buffer)
	if err != nil {
		logctx.WithError(err).Error("error transfering executable")
	}
	return err
}
