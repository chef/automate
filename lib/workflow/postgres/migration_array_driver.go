package postgres

import (
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/mattes/migrate/source"
)

func init() {
	source.Register("array", &ArrayMigrationDriver{})
}

type sqlMigration struct {
	desc    string
	upSQL   string
	downSQL string
}

// ArrayMigrationDriver implement the migratesource.Driver interface
// so that we can use the migrate library without files on disk.
type ArrayMigrationDriver struct {
	migrations []sqlMigration
}

func (a *ArrayMigrationDriver) Open(_ string) (source.Driver, error) {
	return &ArrayMigrationDriver{
		// TODO(ssd) 2019-05-14: If we wanted this to be
		// re-usable, we would need to take the passed string
		// and lookup this data. For now, we just assume it is
		// defined in this package.
		migrations: sqlMigrations,
	}, nil
}

func (a *ArrayMigrationDriver) Close() error { return nil }
func (a *ArrayMigrationDriver) First() (uint, error) {
	if len(a.migrations) == 0 {
		return 0, os.ErrNotExist
	}
	return 0, nil
}

func (a *ArrayMigrationDriver) Prev(version uint) (uint, error) {
	if version == 0 {
		return 0, os.ErrNotExist
	}

	return version - 1, nil
}

func (a *ArrayMigrationDriver) Next(version uint) (uint, error) {
	if version+1 == uint(len(a.migrations)) {
		return 0, os.ErrNotExist
	}

	return version + 1, nil
}

func (a *ArrayMigrationDriver) ReadUp(version uint) (io.ReadCloser, string, error) {
	if version >= uint(len(a.migrations)) {
		return nil, "", os.ErrNotExist
	}

	m := a.migrations[version]
	if m.upSQL == "" {
		return nil, m.desc, os.ErrNotExist
	}

	r := strings.NewReader(m.upSQL)
	rc := ioutil.NopCloser(r)
	return rc, m.desc, nil
}

func (a *ArrayMigrationDriver) ReadDown(version uint) (io.ReadCloser, string, error) {
	if version >= uint(len(a.migrations)) {
		return nil, "", os.ErrNotExist
	}

	m := a.migrations[version]
	if m.downSQL == "" {
		return nil, m.desc, os.ErrNotExist
	}

	r := strings.NewReader(m.downSQL)
	rc := ioutil.NopCloser(r)
	return rc, m.desc, nil
}
