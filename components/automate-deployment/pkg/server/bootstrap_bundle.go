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

	pkgs := make([]string, len(s.deployment.ExpectedServices))
	for i, e := range s.deployment.ExpectedServices {
		pkgs[i] = e.Name()
	}
	err := bundleCreator.Create(pkgs, tarWriter)
	if err != nil {
		return errors.Wrap(err, "Failed to create the bootstrap bundle.")
	}

	err = tarWriter.Flush()
	if err != nil {
		return errors.Wrap(err, "Failed to flush the bootstrap bundle.")
	}
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})

	_, err = io.Copy(writer, &b)
	return err
}
