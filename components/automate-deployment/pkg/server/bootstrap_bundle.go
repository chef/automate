package server

import (
	"bufio"
	"bytes"
	"io"

	"github.com/chef/automate/components/automate-deployment/pkg/services"

	"github.com/chef/automate/lib/product"

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

	pkgsMeta := make([]*product.PackageMetadata, 0, len(s.deployment.ExpectedServices))
	for _, e := range s.deployment.ExpectedServices {
		if metadata := services.MetadataForPackage(e.Name()); metadata != nil {
			pkgsMeta = append(pkgsMeta, metadata)
		}
	}

	err := bundleCreator.Create(pkgsMeta, tarWriter)
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
