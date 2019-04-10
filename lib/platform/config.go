package platform

import (
	"fmt"
	"os"
	"path"
	"strings"

	"github.com/golang/protobuf/jsonpb"
	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/platform"
	"github.com/chef/automate/lib/platform/command"
)

const (
	defaultPGSuperuserName         = "automate"
	defaultPGSuperuserCertPath     = "/hab/svc/automate-postgresql/config/server.crt"
	defaultPGSuperuserKeyPath      = "/hab/svc/automate-postgresql/config/server.key"
	defaultPGSuperuserRootCertPath = "/hab/svc/automate-postgresql/config/root.crt"
)

var ErrNoPlatformEnvironment = errors.New("Platform environment not correctly configured." +
	" Both A2_SVC_NAME and A2_SVC_PATH must be set in the environment")

type Config struct {
	*platform.Config

	SvcDBUser string
}

func ConfigFromEnvironment() (*Config, error) {
	svcName := os.Getenv("A2_SVC_NAME")
	svcPath := os.Getenv("A2_SVC_PATH")
	svcDBUser := os.Getenv("A2_SVC_DB_USER")
	if svcName == "" || svcPath == "" {
		return nil, ErrNoPlatformEnvironment
	}

	platformConfigPath := path.Join(svcPath, "config", "_a2_platform.json")
	f, err := os.Open(platformConfigPath)
	if err != nil {
		return nil, errors.Wrapf(err, "Could not read %s", platformConfigPath)
	}
	defer f.Close() // nolint: errcheck

	platformConfig := Config{
		Config:    &platform.Config{},
		SvcDBUser: svcDBUser,
	}
	unmarshaler := jsonpb.Unmarshaler{
		AllowUnknownFields: true,
	}

	if err := unmarshaler.Unmarshal(f, platformConfig.Config); err != nil {
		return nil, errors.Wrapf(err, "Could not decode %s", platformConfigPath)
	}

	return &platformConfig, nil
}

func PGURIFromEnvironment(database string) (string, error) {
	platformConfig, err := ConfigFromEnvironment()
	if err != nil {
		return "", err
	}
	return platformConfig.GetPGURI(database)
}

func (c *Config) IsExternalPG() bool {
	return c.GetPlatform().GetExternalPostgresql().GetEnable().GetValue()
}

func (c *Config) PGServiceUser() (string, error) {
	if c.IsExternalPG() {
		auth := c.GetPlatform().GetExternalPostgresql().GetAuth()
		switch auth.GetScheme().GetValue() {
		case "password":
			passwordAuth := auth.GetPassword()
			user := passwordAuth.GetDbuser().GetUsername().GetValue()

			if user == "" {
				return "", errors.New("External postgres password auth missing user")
			}
			return user, nil
		default:
			return "", errors.Errorf("Unsupported postgres auth mode %s", auth.GetScheme().GetValue())
		}
	}

	svcUser := c.GetService().GetName()
	if c.SvcDBUser != "" {
		// We allow the service to export A2_SVC_DB_USER in cases where
		// patterns diverge in picking the database user. For example,
		// the chef server services start with automate and do not
		// remove the prefix automate, while dex does
		return c.SvcDBUser, nil
	}
	return strings.TrimSuffix(svcUser, "-service"), nil
}

func (c *Config) PGSuperUser() (string, error) {
	if c.IsExternalPG() {
		auth := c.GetPlatform().GetExternalPostgresql().GetAuth()
		switch auth.GetScheme().GetValue() {
		case "password":
			passwordAuth := auth.GetPassword()
			user := passwordAuth.GetSuperuser().GetUsername().GetValue()

			if user == "" {
				return "", errors.New("External postgres password auth missing user")
			}
			return user, nil
		default:
			return "", errors.Errorf("Unsupported postgres auth mode %s", auth.GetScheme().GetValue())
		}
	}
	return "automate", nil
}

func (c *Config) GetPGURI(dbname string) (string, error) {
	svcUser, err := c.PGServiceUser()
	if err != nil {
		return "", err
	}
	return c.GetPGURIForUser(dbname, svcUser)
}

func (c *Config) GetPGURIForUser(dbname string, user string) (string, error) {
	connInfo, err := c.GetPGConnInfoURI(user)
	if err != nil {
		return "", err
	}
	return connInfo.ConnURI(dbname), nil
}

func (c *Config) GetPGConnInfoForSuperuserWithRole(role string) (*PGConnInfo, error) {
	superuser, err := c.PGSuperUser()
	if err != nil {
		return nil, err
	}
	connInfo, err := c.GetPGConnInfoURI(superuser, WithPGRole(role))
	if err != nil {
		return nil, err
	}
	return connInfo, nil
}

type PGConnInfo struct {
	environment []command.Opt
	fmtStr      string
	debugStr    string
}

func (c *PGConnInfo) ConnURI(dbname string) string {
	return fmt.Sprintf(c.fmtStr, dbname)
}

func (c *PGConnInfo) PsqlCmdOptions() []command.Opt {
	return c.environment
}

func (c *PGConnInfo) String() string {
	return c.debugStr
}

type connInfoConfig struct {
	role string
}
type ConnInfoOpts func(c *connInfoConfig)

func WithPGRole(role string) ConnInfoOpts {
	return func(c *connInfoConfig) {
		c.role = role
	}
}

func (c *Config) GetPGConnInfoURI(user string, opts ...ConnInfoOpts) (*PGConnInfo, error) {
	if c.GetPostgresql() == nil {
		return nil, errors.New("Postgresql config missing")
	}

	config := connInfoConfig{}
	for _, o := range opts {
		o(&config)
	}
	if config.role == "" {
		config.role = user
	}

	if c.IsExternalPG() {
		opts := []string{}

		if ssl := c.GetPlatform().GetExternalPostgresql().GetSsl(); ssl != nil {
			if ssl.GetMode().GetValue() != "" {
				opts = append(opts, fmt.Sprintf("sslmode=%s", ssl.GetMode().GetValue()))
			}
			// TODO: certs
		}

		if auth := c.GetPlatform().GetExternalPostgresql().GetAuth(); auth != nil {
			switch auth.GetScheme().GetValue() {
			case "password":
				passwordAuth := auth.GetPassword()
				password := ""

				if user == passwordAuth.GetDbuser().GetUsername().GetValue() {
					password = passwordAuth.GetDbuser().GetPassword().GetValue()
				} else if user == passwordAuth.GetSuperuser().GetUsername().GetValue() {
					password = passwordAuth.GetSuperuser().GetPassword().GetValue()
				} else {
					return nil, errors.Errorf("Invalid external postgres user %q", user)
				}

				if user == "" {
					return nil, errors.New("External postgres password auth missing user")
				}
				if password == "" {
					return nil, errors.Errorf("External postgres password auth missing password")
				}

				// TODO: what to do about role

				fmtStr := fmt.Sprintf("postgresql://%s:%s@%s:%d/%%s?%s",
					user, password, c.GetPostgresql().GetIp(), c.GetPostgresql().GetCfg().GetPort(), strings.Join(opts, "&"))
				debugStr := fmt.Sprintf("postgresql://%s:<redacted>@%s:%d/<database>?%s",
					user, c.GetPostgresql().GetIp(), c.GetPostgresql().GetCfg().GetPort(), strings.Join(opts, "&"))

				return &PGConnInfo{
					debugStr:    debugStr,
					fmtStr:      fmtStr,
					environment: []command.Opt{},
				}, nil
			default:
				return nil, errors.Errorf("Unsupported postgres auth mode %s", auth.GetScheme().GetValue())
			}
		}

		return nil, errors.New("External postgres is unsupported")
	} else {
		certPath := c.GetService().GetTls().GetCertPath()
		keyPath := c.GetService().GetTls().GetKeyPath()
		rootCertPath := c.GetService().GetTls().GetRootCaPath()

		if user == defaultPGSuperuserName {
			certPath = defaultPGSuperuserCertPath
			keyPath = defaultPGSuperuserKeyPath
			rootCertPath = defaultPGSuperuserRootCertPath
		}

		fmtStr := fmt.Sprintf("postgresql://%s@%s:%d/%%s?sslmode=verify-ca&sslcert=%s&sslkey=%s&sslrootcert=%s",
			config.role, c.GetPostgresql().GetIp(), c.GetPostgresql().GetCfg().GetPort(), certPath, keyPath, rootCertPath)

		// We need this for sqitch
		environment := []command.Opt{
			command.Envvar("PGSSLKEY", keyPath),
			command.Envvar("PGSSLCERT", certPath),
			command.Envvar("PGSSLROOTCERT", rootCertPath),
			command.Envvar("PGSSLMODE", "verify-ca"),
		}

		return &PGConnInfo{
			debugStr:    fmt.Sprintf(fmtStr, "<database>"),
			fmtStr:      fmtStr,
			environment: environment}, nil

	}
}
