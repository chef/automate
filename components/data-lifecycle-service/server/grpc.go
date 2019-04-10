package server

import (
	"context"
	"net"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/components/data-lifecycle-service/storage"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

// StartServer starts the data lifecycle server
func StartServer(ctx context.Context, config Config) error {
	switch config.LogLevel {
	case "debug":
		logrus.SetLevel(logrus.DebugLevel)
	case "error":
		logrus.SetLevel(logrus.ErrorLevel)
	case "warn":
		logrus.SetLevel(logrus.WarnLevel)
	default:
		logrus.SetLevel(logrus.InfoLevel)
	}

	serviceCerts, err := config.ReadCerts()
	if err != nil {
		return err
	}

	connFactory := secureconn.NewFactory(*serviceCerts)
	if err != nil {
		return errors.Wrap(err, "Could not create secure connection")
	}

	address := config.getAddressString()

	listener, err := net.Listen("tcp", address)
	if err != nil {
		return errors.Wrapf(err, "could not listen on address: %s", address)
	}

	logrus.WithFields(logrus.Fields{
		"version": version.Version,
		"sha":     version.GitSHA,
		"address": address,
	}).Info("Starting the Data Lifecycle Service")

	for k, v := range config.ManagedServices {
		logrus.WithFields(logrus.Fields{
			"service": k,
			"address": v.Address,
		}).Info("Managing data lifecycle")
	}

	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())

	dls := NewServer(
		version.Version,
		storage.NewArrayServiceConfigStore(config.ManagedServices),
		config.DataLifeCycleInfo,
		connFactory,
	)

	data_lifecycle.RegisterDataLifecycleServer(grpcServer, dls)
	data_lifecycle.RegisterDataLifecycleManageableServer(grpcServer, dls)

	reflection.Register(grpcServer)

	errChan := make(chan error)
	go func(c chan error) {
		err := grpcServer.Serve(listener)
		c <- err
	}(errChan)

	dailyTrigger := NewDailyTriggerServer(dls, config.DailyRunAt)
	dailyTrigger.Start()

	select {
	case <-ctx.Done():
		grpcServer.Stop()
	case err = <-errChan:
		return err
	}

	dailyTrigger.Stop()

	return nil
}
