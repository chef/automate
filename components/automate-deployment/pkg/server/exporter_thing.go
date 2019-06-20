package server

import (
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

//
// We want to be able to gather up files installed on one node and package them up so they can be made available when installing a second node.
// Then we want to unpackage them and put them in the right place, with the right ownership.
// LATER: We need to make sure a new version of the package gets generated when a file in that thing changes, but that is beyond
// current scope.
// PUT IT ELSEWHERE: For now I'm going to stuff the list of files in here, but we should probably put it elsewhere.
// Also, we probably need to do it per service, which means we need to know who the running services in the installation are.
//

// BootstrapBundleCreate makes a bootstrap bundle
func (s *server) BootstrapBundle(req *api.BootstrapBundleRequest, stream api.Deployment_BootstrapBundleServer) error {
	// staging directory is where the tarball lands
	stagingDir := stagingDir(s.serverConfig)
	logrus.Infof("STAGING IS %s", stagingDir)
	tgzFilepath := filepath.Join(stagingDir, "bootstrap-bundle.tar")
	logrus.Infof("TARBALL IS %s", tgzFilepath)
	f, _ := os.Create(tgzFilepath)

	bundleCreator := bootstrap.NewBundleCreator()
	pkgs := []string{"chef/automate-cs-oc-erchef"}
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
	// need to remove the tarball

	buffer := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})
	_, err = io.CopyBuffer(writer, file, buffer)
	return err
}
