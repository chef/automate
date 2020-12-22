package main

import (
	"fmt"
	"net"

	"github.com/chef/automate/api/interservice/keyval"
	kvs "github.com/chef/automate/components/keyval-service/pkg/server"
	"github.com/chef/automate/lib/grpc/secureconn"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/pkg/errors"
	"google.golang.org/grpc/reflection"
)

func main() {
	platformCfg, err := platform_config.ConfigFromEnvironment()
	if err != nil {
		panic(err)
	}
	tlsConf := platformCfg.GetService().GetTls()
	tlsConfig := certs.TLSConfig{
		CertPath:       tlsConf.GetCertPath(),
		KeyPath:        tlsConf.GetKeyPath(),
		RootCACertPath: tlsConf.GetRootCaPath(),
	}
	serviceCerts, err := tlsConfig.ReadCerts()
	if err != nil {
		panic(err)
	}
	factory := secureconn.NewFactory(*serviceCerts)
	grpcServer := factory.NewServer()

	keyvalServer := kvs.New()

	keyval.RegisterKeyvalServiceServer(grpcServer, keyvalServer)
	reflection.Register(grpcServer)

	listenAddr := fmt.Sprintf("%s:%d", "127.0.0.1", 10199)
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		panic(errors.Wrapf(err, "failed to listen on port %s", listenAddr))
	}

	grpcServer.Serve(lis)
}
