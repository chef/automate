package pg

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/command"
	platform_config "github.com/chef/automate/lib/platform/config"
)

type ConnInfoURI interface {
	// ConnURI returns a string suitable for passing to the
	// database/sql libraries or other tools that accept libpq
	// connection URIs. See
	//     https://www.postgresql.org/docs/9.6/static/libpq-connect.html#LIBPQ-CONNSTRING
	// for details on the format.
	ConnURI(string) string
}

// ConnInfo provides connection information for PostgreSQL databases
type ConnInfo interface {
	ConnInfoURI
	// PsqlCmdOptions returns an array of command.Opts that set
	// the environment variables required for psql (or other tools
	// that use libpq environment variables) to connect to the
	// given database. See
	//     https://www.postgresql.org/docs/9.6/static/libpq-envars.html
	// for details on the various variables.
	PsqlCmdOptions() []command.Opt
}

// A certDir is a directory containing TLS certificates that can be
// used for TLS postgresql connections.
type TLSCertPaths struct {
	Cert     string
	Key      string
	RootCert string
}

func TLSCertPathsFromDir(dir string) TLSCertPaths {
	return TLSCertPaths{
		Cert:     filepath.Join(dir, "service.crt"),
		Key:      filepath.Join(dir, "service.key"),
		RootCert: filepath.Join(dir, "root_ca.crt"),
	}
}

var A2SuperuserCerts = TLSCertPaths{
	Cert:     "/hab/svc/automate-postgresql/config/server.crt",
	Key:      "/hab/svc/automate-postgresql/config/server.key",
	RootCert: "/hab/svc/automate-postgresql/config/root.crt",
}

// NOTE(ssd) 2019-08-19: This is a bit of duplication with code in
// platform/config to ensure that you can depend on the platform
// config without pulling in the command class.
func SuperuserPsqlCmdOptionsFromPlatformConfig(c *platform_config.Config) []command.Opt {
	if c.IsExternalPG() {
		return []command.Opt{
			command.Envvar("PGTZ", "UTC"),
		}
	}
	return []command.Opt{
		command.Envvar("PGSSLKEY", A2SuperuserCerts.Key),
		command.Envvar("PGSSLCERT", A2SuperuserCerts.Cert),
		command.Envvar("PGSSLROOTCERT", A2SuperuserCerts.RootCert),
		command.Envvar("PGSSLMODE", "verify-ca"),
		command.Envvar("PGTZ", "UTC"),
	}
}

// A2ConnInfo represents the connection information for an Automate 2
// PostgreSQL database. It assumes connections can be made via TLS and
// authentication happens via client certificates.
type A2ConnInfo struct {
	User  string
	Host  string
	Port  uint64
	Certs TLSCertPaths
}

func (c *A2ConnInfo) String() string {
	return fmt.Sprintf("ConnectionInfo{user: %s, host: %s, port: %d}", c.User, c.Host, c.Port)
}

func (c *A2ConnInfo) ConnURI(dbname string) string {
	return fmt.Sprintf("postgresql://%s@%s:%d/%s?sslmode=verify-ca&sslcert=%s&sslkey=%s&sslrootcert=%s",
		c.User, c.Host, c.Port, dbname, c.Certs.Cert, c.Certs.Key, c.Certs.RootCert)
}

func (c *A2ConnInfo) PsqlCmdOptions() []command.Opt {
	return []command.Opt{
		command.Envvar("PGUSER", c.User),
		command.Envvar("PGHOST", c.Host),
		command.Envvar("PGPORT", strconv.FormatUint(c.Port, 10)),
		command.Envvar("PGSSLMODE", "verify-ca"),
		command.Envvar("PGSSLKEY", c.Certs.Key),
		command.Envvar("PGSSLCERT", c.Certs.Cert),
		command.Envvar("PGSSLROOTCERT", c.Certs.RootCert),
		command.Envvar("PGTZ", "UTC"),
	}
}

// A1ConnInfo represents the connection information for an Automate 1
// PostgreSQL database. It assumes connections should be made without
// TLS and that password authentication can be used.
//
// To avoid leaking passwords via the environment, we use the
// PGPASSFILE feature of libpq. Users should call InitPgPassfile()
// before using the options returned by PsqlCmdOptions. Users are
// responsible for calling CleanupPgPassfile() when they are done.
type A1ConnInfo struct {
	User     string
	Pass     string
	Host     string
	Port     uint64
	passfile string
}

func (c *A1ConnInfo) String() string {
	return fmt.Sprintf("ConnInfo{user: %s, host: %s, port: %d, pass: REDACTED (len = %d)}", c.User, c.Host, c.Port, len(c.Pass))
}

func (c *A1ConnInfo) PsqlCmdOptions() []command.Opt {
	return []command.Opt{
		command.Envvar("PGUSER", c.User),
		command.Envvar("PGHOST", c.Host),
		command.Envvar("PGPORT", strconv.FormatUint(c.Port, 10)),
		command.Envvar("PGPASSFILE", c.passfile),
	}
}

func (c *A1ConnInfo) ConnURI(dbname string) string {
	return fmt.Sprintf("postgres://%s:%s@%s:%d/%s?sslmode=disable", c.User, c.Pass, c.Host, c.Port, dbname)
}

// InitPgPassfile creates a temporary pgpass file. The format of the file
// is defined by:
//
//     https://www.postgresql.org/docs/9.6/static/libpq-pgpass.html
//
// The filename is returned if the file has been successfully written
// and synced to disk. Otherwise, an error is returned. The caller is
// responsible for cleaning up the file after use.
func (c *A1ConnInfo) InitPgPassfile() (string, error) {
	if c.passfile != "" {
		return c.passfile, nil
	}

	file, err := ioutil.TempFile("", "ssapgp1a")
	if err != nil {
		return "", err
	}
	defer file.Close() // nolint: errcheck

	_, err = fmt.Fprintf(file, "%s:%d:*:%s:%s", c.Host, c.Port, c.User, c.Pass)
	if err != nil {
		return "", errors.Wrap(err, "failed to write to pgpass temporary file")
	}
	err = file.Sync()
	if err != nil {
		return "", errors.Wrap(err, "failed to sync pgpass temporary file")
	}

	c.passfile = file.Name()
	return file.Name(), nil
}

func (c *A1ConnInfo) CleanupPgPassfile() error {
	if c.passfile != "" {
		err := os.Remove(c.passfile)
		if err != nil {
			return err
		}
		c.passfile = ""
	}
	return nil
}
