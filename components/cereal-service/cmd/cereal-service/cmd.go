package main

import (
	"context"
	"fmt"
	"net"
	"os"
	"os/signal"
	"syscall"

	"github.com/chef/automate/lib/grpc/health"

	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/spf13/viper"

	grpcceral "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/components/cereal-service/pkg/server"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/tls/certs"

	"google.golang.org/grpc"
)

const megabyte = 1 << 20

type config struct {
	Service struct {
		Host             string `mapstructure:"host"`
		Port             int    `mapstructure:"port"`
		DisableTLS       bool   `mapstructure:"disable_tls"`
		MaxRecvSizeBytes int    `mapstructure:"max_recv_size_bytes"`
	} `mapstructure:"service"`
	TLS certs.TLSConfig `mapstructure:"tls"`
	Log struct {
		Level string `mapstructure:"level"`
	} `mapstructure:"log"`
	Database struct {
		URL string `mapstructure:"url"`
	} `mapstructure:"database"`
}

var cfgFile string
var C config

var rootCmd = &cobra.Command{
	Use: "cereal-service",
}

var serveCmd = &cobra.Command{
	Use:  "serve",
	Args: cobra.NoArgs,
	RunE: serve,
}

func initConfig() error {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
		if err := viper.ReadInConfig(); err != nil {
			return err
		}
	}

	if err := viper.Unmarshal(&C); err != nil {
		return err
	}

	switch C.Log.Level {
	case "trace":
		logrus.SetLevel(logrus.TraceLevel)
	case "debug":
		logrus.SetLevel(logrus.DebugLevel)
	case "info":
		logrus.SetLevel(logrus.InfoLevel)
	case "warn":
		logrus.SetLevel(logrus.WarnLevel)
	case "error":
		logrus.Error(logrus.ErrorLevel)
	}

	if C.Database.URL == "" {
		var err error
		C.Database.URL, err = platform.PGURIFromEnvironment("cereal_service")
		if err != nil {
			return err
		}
	}

	if C.TLS.CertPath == "" {
		platformCfg, err := platform.ConfigFromEnvironment()
		if err != nil {
			return err
		}
		tlsConfig := platformCfg.GetService().GetTls()
		if tlsConfig == nil {
			return errors.New("could not load TLS config")
		}
		C.TLS.CertPath = tlsConfig.GetCertPath()
		C.TLS.KeyPath = tlsConfig.GetKeyPath()
		C.TLS.RootCACertPath = tlsConfig.GetRootCaPath()
	}

	return nil
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}
func init() {
	serveCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file")
	serveCmd.PersistentFlags().String("host", "", "listen host")
	serveCmd.PersistentFlags().Int("port", 0, "port to listen on")
	serveCmd.PersistentFlags().Bool("no-tls", false, "disable tls")
	must(viper.BindPFlag("service.host", serveCmd.PersistentFlags().Lookup("host")))
	must(viper.BindPFlag("service.port", serveCmd.PersistentFlags().Lookup("port")))
	must(viper.BindPFlag("service.disable_tls", serveCmd.PersistentFlags().Lookup("no-tls")))
	must(viper.BindEnv("database.url", "PG_URL"))
	viper.SetDefault("service.port", 10101)
	viper.SetDefault("service.disable_tls", false)
	viper.SetDefault("service.max_recv_size_bytes", 64*megabyte)

	rootCmd.AddCommand(serveCmd)
}

func serve(*cobra.Command, []string) error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := initConfig(); err != nil {
		return errors.Wrap(err, "failed to load config")
	}

	pgBackend := postgres.NewPostgresBackend(C.Database.URL)
	if err := pgBackend.Init(); err != nil {
		return errors.Wrap(err, "could not initialize database")
	}

	var grpcServer *grpc.Server
	grpcServerOpts := []grpc.ServerOption{
		grpc.MaxRecvMsgSize(C.Service.MaxRecvSizeBytes),
	}

	if C.Service.DisableTLS {
		grpcServer = grpc.NewServer(grpcServerOpts...)
	} else {
		serviceCerts, err := C.TLS.ReadCerts()
		if err != nil {
			return errors.Wrap(err, "could not read tls certs")
		}
		f := secureconn.NewFactory(*serviceCerts)
		grpcServer = f.NewServer(grpcServerOpts...)
	}

	svc := server.NewCerealService(ctx, pgBackend)
	grpcceral.RegisterCerealServer(grpcServer, svc)

	healthSvc := health.NewService()
	health.RegisterHealthServer(grpcServer, healthSvc)

	listenAddr := fmt.Sprintf("%s:%d", C.Service.Host, C.Service.Port)
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		return errors.Wrapf(err, "failed to listen on port %s", listenAddr)
	}

	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		sig := <-ch
		healthSvc.Shutdown()
		grpcServer.GracefulStop()
		logrus.WithField("signal", sig).Info("Exiting")
	}()

	return grpcServer.Serve(lis)
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}
