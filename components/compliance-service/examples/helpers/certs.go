package helpers

import (
	"path"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	cert_helper "github.com/chef/automate/lib/tls/test/helpers"
)

// LoadCerts loads the dev certs for compliance service
func LoadCerts() *certs.ServiceCerts {
	name := "compliance-service"
	cfg := certs.TLSConfig{
		CertPath:       cert_helper.DevCertPath(name),
		KeyPath:        cert_helper.DevKeyPath(name),
		RootCACertPath: cert_helper.DevRootCACert(),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		logrus.WithError(err).Fatal("Could not load certs")
	}

	return serviceCerts
}

// uses the certs in a running hab env
func LoadCertsHab() *certs.ServiceCerts {
	dirname := "/hab/svc/compliance-service/config"
	logrus.Infof("certs dir is %s", dirname)

	cfg := certs.TLSConfig{
		CertPath:       path.Join(dirname, "service.crt"),
		KeyPath:        path.Join(dirname, "service.key"),
		RootCACertPath: path.Join(dirname, "root_ca.crt"),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		logrus.WithError(err).Fatal("Could not load certs:")
	}

	return serviceCerts
}

// uses the certs in a running hab env
func LoadCertsFromDeploymentServiceHab() *certs.ServiceCerts {
	// use the deployment service certs since they can call grpc endpoints
	// that exist in the gateway
	dirname := "/hab/svc/deployment-service/data"
	logrus.Infof("certs dir is %s", dirname)

	cfg := certs.TLSConfig{
		CertPath:       path.Join(dirname, "deployment-service.crt"),
		KeyPath:        path.Join(dirname, "deployment-service.key"),
		RootCACertPath: path.Join(dirname, "root.crt"),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		logrus.WithError(err).Fatal("Could not load certs:")
	}

	return serviceCerts
}

func SecureConnFactory() *secureconn.Factory {
	serviceCerts := LoadCerts()
	return secureconn.NewFactory(*serviceCerts)
}

func SecureConnFactoryHab() *secureconn.Factory {
	certs := LoadCertsHab()
	return secureconn.NewFactory(*certs)
}

func SecureConnFactoryHabWithDeploymentServiceCerts() *secureconn.Factory {
	certs := LoadCertsFromDeploymentServiceHab()
	return secureconn.NewFactory(*certs)
}
