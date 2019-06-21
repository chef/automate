package server

import (
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/lib/io/chunks"
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
	/*
		stagingDir := stagingDir(s.serverConfig)
		tarFilepath := filepath.Join(stagingDir, "bootstrap-bundle.tar")
		f, _ := os.Create(tarFilepath)

		buffer := make([]byte, defaultChunkSize)
	*/
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})

	bundleCreator := bootstrap.NewBundleCreator()

	pkgs := make([]string, 0)
	for _, e := range s.deployment.ExpectedServices {
		pkgs = append(pkgs, e.Name())
	}
	err := bundleCreator.Create(pkgs, writer)
	if err != nil {
		return err
	}

	//_, err = io.CopyBuffer(writer, file, buffer)
	return err
}
