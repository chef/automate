package main

import (
	"fmt"
	"time"

	"github.com/chef/automate/api/interservice/user_settings"
	"github.com/chef/automate/lib/grpc/secureconn"

	uss "github.com/chef/automate/components/user-settings-service/pkg/server"

	"net"

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

	userSettingsServer := uss.New()

	user_settings.RegisterUserSettingsServiceServer(grpcServer, userSettingsServer)
	reflection.Register(grpcServer)

	listenAddr := fmt.Sprintf("%s:%d", "127.0.0.1", 10107)
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		panic(errors.Wrapf(err, "failed to listen on port %s", listenAddr))
	}

	grpcServer.Serve(lis)

	//for {
	//	fmt.Println("user-settings-service")
	//	time.Sleep(5 * time.Second)
	//
	//user_settings.RegisterUserSettingsServiceServer(grpcServer, userSettingsServer)
	//reflection.Register(grpcServer)
	//
	//listenAddr := fmt.Sprintf("%s:%d", "127.0.0.1", 10199)
	//lis, err := net.Listen("tcp", listenAddr)
	//if err != nil {
	//	panic(errors.Wrapf(err, "failed to listen on port %s", listenAddr))
	//}
	//
	//grpcServer.Serve(lis)

	for {
		fmt.Println("user-settings-service")
		time.Sleep(5 * time.Second)
	}
}
