package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/url"
	"os"

	"github.com/ghodss/yaml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authn-service/server"
	"github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/authz/project_purge"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

func commandServe() *cobra.Command {
	return &cobra.Command{
		Use:     "serve [config file]",
		Short:   "Begin serving requests.",
		Long:    ``,
		Example: "authn serve config.yaml",
		Run: func(cmd *cobra.Command, args []string) {
			closer, err := tracing.NewGlobalTracer("authn-service")
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

	configFile := args[0]
	configData, err := ioutil.ReadFile(configFile)
	if err != nil {
		return errors.Wrapf(err, "failed to read config file %s", configFile)
	}

	var c Config
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

	authenticatorConfigMap := make(map[string]server.AuthenticatorConfig, len(c.Authenticators))

	for _, a := range c.Authenticators {
		if a.ID == "" || a.Type == "" {
			return fmt.Errorf("invalid config: id and type fields are required for an authenticator")
		}
		if a.Config == nil {
			return fmt.Errorf("invalid config: no config field for authenticator %q", a.ID)
		}

		l.Infof("config authenticator: %s", a.ID)
		authenticatorConfigMap[a.ID] = a.Config
	}

	var upstream *url.URL
	if c.Upstream != "" {
		upstream, err = url.Parse(c.Upstream)
		if err != nil {
			return errors.Wrap(err, "could not parse upstream url")
		} else if upstream.Scheme == "" || upstream.Host == "" {
			return fmt.Errorf("could not parse upstream url '%v'", upstream)
		}
	}

	if c.TeamsAddress == "" {
		return fmt.Errorf("missing address for teams service")
	}

	if c.AuthzAddress == "" {
		return fmt.Errorf("missing address for authz service")
	}

	if c.CerealAddress == "" {
		return fmt.Errorf("missing address for cereal service")
	}

	factory := secureconn.NewFactory(*serviceCerts, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	cerealConn, err := factory.Dial("cereal-service", c.CerealAddress)
	if err != nil {
		return errors.Wrapf(err, "dial cereal-service (%s)", c.CerealAddress)
	}

	grpcBackend := authz.ProjectUpdateBackend(cerealConn)
	defer grpcBackend.Close() // nolint: errcheck
	manager, err := cereal.NewManager(grpcBackend)
	if err != nil {
		return errors.Wrap(err, "error starting project update backend")
	}

	serverConfig := server.Config{
		Upstream:                 upstream,
		Logger:                   logger,
		Authenticators:           authenticatorConfigMap,
		Token:                    c.Token.Config,
		ServiceCerts:             serviceCerts,
		TeamsAddress:             c.TeamsAddress,
		AuthzAddress:             c.AuthzAddress,
		LegacyDataCollectorToken: c.LegacyDataCollectorToken,
	}

	// TODO (tc): We are dialing twice now. We should move all the client dials out here.
	authzConn, err := factory.Dial("authz-service", c.AuthzAddress)
	if err != nil {
		return errors.Wrapf(err, "dial authz-service (%s)", c.AuthzAddress)
	}
	authzV2Client := authz_v2.NewAuthorizationClient(authzConn)

	serv, err := server.NewServer(context.Background(), serverConfig, authzV2Client)
	if err != nil {
		return errors.Wrap(err, "failed to initialize server")
	}

	err = project_purge.RegisterTaskExecutors(manager, "authn", serv.TokenStorage)
	if err != nil {
		return errors.Wrap(err, "failed to register project purge task executor")
	}

	if c.GRPC == "" {
		return fmt.Errorf("GRPC endpoint not configured")
	}

	err = manager.Start(context.Background())
	if err != nil {
		return errors.Wrap(err, "could not start project update manager")
	}
	defer manager.Stop() // nolint: errcheck

	l.Infof("grpc endpoint listening on %s", c.GRPC)
	l.Infof("http1 endpoint listening on %s", c.HTTP1)
	err = serv.Serve(c.GRPC, c.HTTP1)
	if err != nil {
		return errors.Wrapf(err, "grpc endpoint listening on %s failed", c.GRPC)
	}

	return nil
}

func newLogger(level, format string) (*zap.Logger, error) {
	cfg := zap.NewProductionConfig()
	// log timestamps in ISO8601
	cfg.EncoderConfig.EncodeTime = zapcore.ISO8601TimeEncoder
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
