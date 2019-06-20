package server

import (
	"compress/gzip"
	"io"
	"os"
	"path/filepath"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/sirupsen/logrus"
)

type bootstrapFile struct {
	Filename string `json:"filename"`
	Path     string `json:"path"`
}

type serviceForExport struct {
	Service        string          `json:"service"`
	BootstrapFiles []bootstrapFile `json:"bootstrap_files"`
}

type servicesForExport struct {
	Services []serviceForExport `json:"services"`
}

// BootstrapBundle makes and downloads a bootstrap bundle
func (s *server) BootstrapBundle(req *api.BootstrapBundleRequest, stream api.Deployment_BootstrapBundleServer) error {
	// staging directory is where the tarball lands
	stagingDir := stagingDir(s.serverConfig)
	tgzFilepath := filepath.Join(stagingDir, "bootstrap-bundle.tgz")
	f, _ := os.Create(tgzFilepath)
	gzw := gzip.NewWriter(f)
	defer gzw.Close()

	bundleCreator := bootstrap.NewBundleCreator()

	pkgs := make([]string, 0)
	for _, e := range s.deployment.ExpectedServices {
		pkgs = append(pkgs, e.Name())
	}
	err := bundleCreator.Create(pkgs, f)
	if err != nil {
		return err
	}
	f.Close()

	file, err := os.Open(tgzFilepath)
	if err != nil {
		return err
	}
	defer file.Close()
	defer func() {
		err := os.Remove(tgzFilepath)
		if err != nil {
			// Do something with the error.
			logrus.WithError(err).Warn("Failed to remove bootstrap bundle tarball.")
		}
	}()

	buffer := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})
	_, err = io.CopyBuffer(writer, file, buffer)
	return err
}
