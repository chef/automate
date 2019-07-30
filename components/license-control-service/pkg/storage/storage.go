package storage

import (
	"context"
	"database/sql"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// NoLicenseError is the error returned by a backend when it does not
// have a configured License.
type NoLicenseError struct{ backend string }

func (n *NoLicenseError) Error() string {
	return fmt.Sprintf("No license found in %s storage backend", n.backend)
}

// RetriableBackendError is the error returned by a backend when an
// update has failed but may work if retried.
type RetriableBackendError struct{ err error }

func (r *RetriableBackendError) Error() string { return r.err.Error() }

// A CurrentBackend can be used as a storage backend for the
// license-control-service.
type CurrentBackend interface {
	// Init prepares the backend and migrates data from any known
	// Upgradeable Backends. Must be called before GetLicense and
	// SetLicense.
	Init(context.Context, *keys.LicenseParser) error
	// GetLicense returns the currently configured autoamte
	// license. If no license is configured, NoLicense is
	// returend.
	GetLicense(context.Context) (string, keys.LicenseMetadata, error)
	// SetLicense stores the given license in the backend.
	SetLicense(context.Context, string) error
}

// An UpgradeableBackend is an old backend that can no longer be used
// as a backend for the license-control-service but which might have
// data that needs to be migrated into the CurrentBackend.
type UpgradeableBackend interface {
	// GetLicense returns the currently configured autoamte
	// license. If no license is configured, ErrNoLicense is
	// returend.
	GetLicense(context.Context) (string, error)
	// Cleanup removes the state kept by this backend. After
	// Cleanup is called, GetLicense should return a
	// NoLicenseError.
	Cleanup(context.Context) error
}

var _ CurrentBackend = &PGBackend{}
var _ UpgradeableBackend = &FileBackend{}

func NewCurrentBackend(pgURL string, migrationPath string, legacyFilePath string) CurrentBackend {
	return &PGBackend{
		pgURL:             pgURL,
		migrationPath:     migrationPath,
		legacyFileBackend: &FileBackend{path: legacyFilePath},
	}
}

// PgBackend is the CurrentBackend used in production. It stores the
// raw key data in PostgreSQL.
type PGBackend struct {
	pgURL             string
	migrationPath     string
	legacyFileBackend UpgradeableBackend

	db *sql.DB
}

func (p *PGBackend) Init(ctx context.Context, l *keys.LicenseParser) error {
	db, err := sql.Open("postgres", p.pgURL)
	if err != nil {
		return errors.Wrap(err, "failed to open database connection")
	}
	p.db = db

	err = migrator.Migrate(p.pgURL, p.migrationPath, logger.NewLogrusStandardLogger(), false)
	if err != nil {
		return errors.Wrap(err, "failed to apply database schema")
	}

	_, _, err = p.GetLicense(ctx)
	switch err.(type) {
	case nil:
		// Already migrated
		return nil
	case *NoLicenseError:
		return p.migrateFromFileBackend(ctx, l)
	default:
		return err
	}
}

// TODO(ssd) 2019-07-29: This migration would be problematic in
// multi-node, but this should only happen on single node clusters.
func (p *PGBackend) migrateFromFileBackend(ctx context.Context, l *keys.LicenseParser) error {
	license, err := p.legacyFileBackend.GetLicense(ctx)
	switch err.(type) {
	case nil:
		logrus.Info("migrating from file-based storage to postgresql-based storage")
		_, err := l.Parse(license)
		if err != nil {
			logrus.WithError(err).Warn("invalid license found on disk")
			// this key is never going to be valid, so we
			// just start up.
			return nil
		}

		err = p.SetLicense(ctx, license)
		if err != nil {
			return errors.Wrap(err, "failed to migrate license to postgresql")
		}
		err = p.legacyFileBackend.Cleanup(ctx)
		if err != nil {
			logrus.WithError(err).Warn("failed to clean up old license path")
		}
		return nil
	case *NoLicenseError:
		return nil
	default:
		return errors.Wrap(err, "failed to read license from disk")
	}
}

func (p *PGBackend) GetLicense(ctx context.Context) (string, keys.LicenseMetadata, error) {
	row := p.db.QueryRowContext(ctx, "SELECT configured_at, data FROM licenses WHERE active=TRUE")

	md := keys.LicenseMetadata{}
	var licenseData string
	err := row.Scan(&md.ConfiguredAt, &licenseData)
	if err == sql.ErrNoRows {
		return "", keys.LicenseMetadata{}, &NoLicenseError{backend: "sql"}
	}
	if err != nil {
		return "", keys.LicenseMetadata{}, err
	}
	return licenseData, md, nil
}

func (p *PGBackend) SetLicense(ctx context.Context, data string) error {
	_, err := p.db.ExecContext(ctx, "SELECT set_active_license_v1($1)", data)
	if err != nil {
		if isPGConflict(err) {
			// This can happen during concurrent
			// applications of new licenses. One of the
			// writers will fail but can retry if they
			// want.
			err := errors.Wrap(
				err,
				"conflict when attempting to set license (likely caused by concurrent license update), please retry",
			)
			return &RetriableBackendError{err: err}
		}
	}

	return err
}

func isPGConflict(err error) bool {
	if pqErr, ok := err.(*pq.Error); ok {
		if pqErr.Code == "23505" {
			return true
		}
	}
	return false
}

// The FileBackend is a UpgradeableBackend for the previous file-based
// license storage.
type FileBackend struct {
	path string
}

func NewFileBackend(path string) *FileBackend {
	return &FileBackend{
		path: path,
	}
}

func (f *FileBackend) GetLicense(context.Context) (string, error) {
	licenseBytes, err := ioutil.ReadFile(f.path)
	if err != nil {
		if os.IsNotExist(err) {
			return "", &NoLicenseError{backend: "file"}
		}
		return "", errors.Wrapf(err, "failed to read license data from %s", f.path)
	}
	return strings.TrimSpace(string(licenseBytes)), nil
}

func (f *FileBackend) Cleanup(context.Context) error {
	err := os.Remove(f.path)
	if os.IsNotExist(err) {
		return nil
	}
	return err
}

// MemBackend is a CurrentBackend for testing purposes only.
type MemBackend struct {
	licenseData  string
	configuredAt time.Time

	sync.RWMutex
}

func (m *MemBackend) Init(context.Context, *keys.LicenseParser) error {
	return nil
}

func (m *MemBackend) GetLicense(context.Context) (string, keys.LicenseMetadata, error) {
	m.RLock()
	defer m.RUnlock()

	md := keys.LicenseMetadata{
		ConfiguredAt: m.configuredAt,
	}
	if m.licenseData == "" {
		return "", md, &NoLicenseError{backend: "memory"}
	}

	return m.licenseData, md, nil
}

func (m *MemBackend) SetLicense(ctx context.Context, s string) error {
	m.Lock()
	defer m.Unlock()

	m.configuredAt = time.Now()
	m.licenseData = s
	return nil
}
