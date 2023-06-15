package config

import (
	"fmt"
	"net/url"
	"os"
	"path"
	"strings"

	"github.com/golang/protobuf/jsonpb"
	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/platform"
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

func ConfigFromParams(svcName, svcPath, svcDBUser string) (*Config, error) {

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

func PGURIFromEnvironmentWithParams(database, svc, path, svcDBUser string) (string, error) {
	platformConfig, err := ConfigFromParams(svc, path, svcDBUser)
	if err != nil {
		return "", err
	}
	return platformConfig.GetPGURI(database)
}

func (c *Config) IsExternalPG() bool {
	return c.GetPlatform().GetExternalPostgresql().GetEnable().GetValue()
}

func (c *Config) ExternalPGRootCertPath() string {
	return path.Join(c.GetService().GetPath(), "config", "_a2_platform_external_pg_root_ca.crt")
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
	return defaultPGSuperuserName, nil
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

func (c *Config) GetPGConnInfoForSuperuser() (*PGConnInfo, error) {
	superuser, err := c.PGSuperUser()
	if err != nil {
		return nil, err
	}
	connInfo, err := c.GetPGConnInfoURI(superuser)
	if err != nil {
		return nil, err
	}
	return connInfo, nil
}

type pgConnURIRenderer func(dbname string) string

type PGConnInfo struct {
	connURIRenderer pgConnURIRenderer
	debugStr        string
}

func (c *PGConnInfo) ConnURI(dbname string) string {
	return c.connURIRenderer(dbname)
}

func (c *PGConnInfo) String() string {
	return c.debugStr
}

func externalConnURIRenderer(ip string, port int, user string, password string, opts []string) (pgConnURIRenderer, string) {
	fmtStr := "postgresql://%s@%s:%d/%s?%s"

	userInfoDebugStr := user
	if password != "" {
		userInfoDebugStr += ":<redacted>"
	}
	debugStr := fmt.Sprintf(fmtStr, userInfoDebugStr, ip, port, "<database>", strings.Join(opts, "&"))

	userInfo := url.UserPassword(user, password)
	return func(dbname string) string {
		return fmt.Sprintf(fmtStr, userInfo.String(), ip, port, dbname, strings.Join(opts, "&"))
	}, debugStr
}

func internalConnURIRenderer(ip string, port int, user string, certPath string,
	keyPath string, rootCertPath string) (pgConnURIRenderer, string) {
	fmtStr := "postgresql://%s@%s:%d/%s?sslmode=verify-ca&sslcert=%s&sslkey=%s&sslrootcert=%s"
	return func(dbname string) string {
		return fmt.Sprintf(fmtStr,
			user, ip, port, dbname, certPath, keyPath, rootCertPath)
	}, fmt.Sprintf(fmtStr, user, ip, port, "<database>", certPath, keyPath, rootCertPath)
}

func (c *Config) GetPGConnInfoURI(user string) (*PGConnInfo, error) {
	if c.GetPostgresql() == nil {
		return nil, errors.New("Postgresql config missing")
	}

	if c.IsExternalPG() {
		opts := []string{}

		if ssl := c.GetPlatform().GetExternalPostgresql().GetSsl(); ssl != nil {
			if ssl.GetEnable().GetValue() {
				opts = append(opts, "sslmode=verify-ca", "sslrootcert="+c.ExternalPGRootCertPath())
			} else {
				opts = append(opts, "sslmode=disable")
			}
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

				connURIRenderer, debugStr := externalConnURIRenderer(c.GetPostgresql().GetIp(),
					int(c.GetPostgresql().GetCfg().GetPort()), user, password, opts)

				return &PGConnInfo{
					debugStr:        debugStr,
					connURIRenderer: connURIRenderer,
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

		connURIRenderer, debugStr := internalConnURIRenderer(c.GetPostgresql().GetIp(),
			int(c.GetPostgresql().GetCfg().GetPort()), user, certPath, keyPath, rootCertPath)
		return &PGConnInfo{
			debugStr:        debugStr,
			connURIRenderer: connURIRenderer,
		}, nil

	}
}
