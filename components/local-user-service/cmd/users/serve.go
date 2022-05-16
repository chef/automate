package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/ghodss/yaml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"go.uber.org/zap"

	"github.com/chef/automate/components/local-user-service/config"
	"github.com/chef/automate/components/local-user-service/server"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tracing"
)

func commandServe() *cobra.Command {
	return &cobra.Command{
		Use:     "serve [config file]",
		Short:   "Begin serving requests.",
		Long:    ``,
		Example: "users serve config.yaml",
		Run: func(cmd *cobra.Command, args []string) {
			closer, err := tracing.NewGlobalTracer("local-user-service")
			if err == nil {
				defer tracing.CloseQuietly(closer)
			}

			if err := serve(cmd, args); err != nil {
				/* TODO (tyler) this gives an error:
				   `Errors unhandled.,LOW,HIGH (gas)`
				   We should probably fix it since gas is a security linter,
				   but I don't have enough context to fix it now or know why
				   it's failing. */
				fmt.Fprintln(os.Stderr, err) // #nosec
				os.Exit(2)
			}
		},
	}
}

func serve(cmd *cobra.Command, args []string) error {
	switch len(args) {
	default:
		return errors.New("surplus arguments")
	case 0:
		return errors.New("no arguments provided")
	case 1:
	}

	ctx := context.Background()
	configFile := args[0]
	configData, err := ioutil.ReadFile(configFile)
	if err != nil {
		return errors.Wrapf(err, "failed to read config file %s", configFile)
	}

	var c config.Config
	if err := yaml.Unmarshal(configData, &c); err != nil { // nolint: vetshadow
		return errors.Wrapf(err, "error parse config file %s", configFile)
	}

	c.FixupRelativeTLSPaths(configFile)

	serviceCerts, err := c.ReadCerts()
	if err != nil {
		return errors.Wrapf(err, "failed to read tls keys/certs")
	}

	logger, err := newLogger(c.Logger.Level, c.Logger.Format) // nolint: vetshadow
	if err != nil {
		return errors.Wrapf(err, "failed to init logger")
	}

	// use sugared logger here for convenience
	l := logger.Sugar()

	if c.TeamsAddress == "" {
		return errors.New("missing config value teams_address")
	}

	if c.AuthzAddress == "" {
		return errors.New("missing config value authz_address")
	}

	serverConfig := server.Config{
		Logger:          logger,
		Users:           c.Users.Config,
		A1UserData:      c.A1UserData,
		A1UserRolesData: c.A1UserRolesData,
		ServiceCerts:    serviceCerts,
		TeamsAddress:    c.TeamsAddress,
		AuthzAddress:    c.AuthzAddress,
	}

	serv, err := server.NewServer(ctx, serverConfig)
	if err != nil {
		return errors.Wrap(err, "failed to initialize server")
	}

	if err := serv.MigrateA1Users(ctx); err != nil {
		return errors.Wrap(err, "A1 users migration failed")
	}
	if err := serv.MigrateA1UserRoles(ctx); err != nil {
		return errors.Wrap(err, "A1 user roles migration failed")
	}

	if c.GRPC == "" {
		return errors.New("GRPC config required")
	}

	l.Infof("grpc endpoint listening (grpc) on %s", c.GRPC)
	// serv.GRPC only returns with a non-nil error
	return errors.Wrapf(serv.GRPC(c.GRPC), "grpc endpoint listening on %s failed", c.GRPC)
}

func newLogger(level, format string) (*zap.Logger, error) {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(logger.ParseZapLevel(level))
	cfg.Encoding = logger.ParseZapEncoding(format)
	logger, err := cfg.Build()
	if err != nil {
		return nil, errors.Wrapf(err, "can't initialize zap logger")
	}
	defer logger.Sync() // nolint: errcheck

	// redirect stdlib's log to zap
	zap.RedirectStdLog(logger)

	return logger, nil
}
