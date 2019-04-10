package client

import (
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/lib/tls/certs"
)

func LoadLocalCerts() (*certs.ServiceCerts, error) {
	c := certs.TLSConfig{
		CertPath:       constants.CertPath,
		KeyPath:        constants.KeyPath,
		RootCACertPath: constants.RootCertPath,
	}
	certData, err := c.ReadCerts()
	if err != nil {
		return nil, errors.Wrap(err, "could not read deployment-service certificates (deployment-service may be starting up)")
	}
	return certData, nil
}
