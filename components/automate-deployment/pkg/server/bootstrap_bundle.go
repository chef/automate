package server

import (
	"bufio"
	"bytes"
	"io"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/pkg/errors"
)

// BootstrapBundle makes and downloads a bootstrap bundle
func (s *server) BootstrapBundle(req *api.BootstrapBundleRequest, stream api.Deployment_BootstrapBundleServer) error {
	var b bytes.Buffer
	tarWriter := bufio.NewWriter(&b)
	bundleCreator := bootstrap.NewBundleCreator()

	pkgs := make([]string, 0)
	for _, e := range s.deployment.ExpectedServices {
		pkgs = append(pkgs, e.Name())
	}
	err := bundleCreator.Create(pkgs, tarWriter)
	if err != nil {
		return errors.Wrap(err, "Failed to create the bootstrap bundle.")
	}

	tarWriter.Flush()
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})

	_, err = io.Copy(writer, &b)
	return err
}
