package pg_test

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

type mockDBProvider struct {
	mockDB *pg.MockDB
}

func (m mockDBProvider) Connect(c pg.ConnInfoURI, dbname string) (pg.DB, error) {
	return m.mockDB, nil
}

func NewDBMock() (pg.DBProvider, *pg.MockDB) {
	db := pg.MockDB{}
	return mockDBProvider{
		mockDB: &db,
	}, &db

}

var testExpectedEnv = []string{
	"PGUSER=test-user",
	"PGHOST=test-db.example.com",
	"PGPORT=5432",
	"PGSSLMODE=verify-ca",
	"PGSSLKEY=/hab/svc/automate-postgresql/config/server.key",
	"PGSSLCERT=/hab/svc/automate-postgresql/config/server.crt",
	"PGSSLROOTCERT=/hab/svc/automate-postgresql/config/root.crt"}

var connInfo = &pg.A2ConnInfo{
	Host:  "test-db.example.com",
	User:  "test-user",
	Port:  5432,
	Certs: pg.A2SuperuserCerts,
}

var connURITemplate = "postgresql://test-user@test-db.example.com:5432/%s?sslmode=verify-ca&sslcert=/hab/svc/automate-postgresql/config/server.crt&sslkey=/hab/svc/automate-postgresql/config/server.key&sslrootcert=/hab/svc/automate-postgresql/config/root.crt"

func connURI(dbName string) string {
	return fmt.Sprintf(connURITemplate, dbName)
}

func TestExists(t *testing.T) {
	setup := func() (*pg.MockDB, func()) {
		provider, mockDB := NewDBMock()
		mockDB.On("Close").Return(nil)
		pg.CurrentDBProvider = provider
		cleanup := func() { pg.CurrentDBProvider = pg.DefaultDBProvider }
		return mockDB, cleanup
	}

	exporter := pg.DatabaseExporter{
		Name:     "test_database",
		ConnInfo: connInfo,
	}

	t.Run("it returns true if the underlying DB returns true", func(t *testing.T) {
		mockDB, cleanup := setup()
		defer cleanup()

		mockDB.On("DatabaseExists", "test_database").Return(true, nil)
		actual, err := exporter.Exists()
		require.NoError(t, err)
		assert.True(t, actual)
	})
	t.Run("it returns an error if the underlying DB errors", func(t *testing.T) {
		mockDB, cleanup := setup()
		defer cleanup()

		mockDB.On("DatabaseExists", "test_database").Return(false, errors.New("test error"))
		_, err := exporter.Exists()
		require.Error(t, err)
	})
}

func TestExport(t *testing.T) {
	setup := func(t *testing.T) (pg.DatabaseExporter, *command.MockExecutor) {
		mockExec := command.NewMockExecutor(t)
		exporter := pg.DatabaseExporter{
			Name:        "test_database",
			ConnInfo:    connInfo,
			CmdExecutor: mockExec,
		}
		return exporter, mockExec
	}

	connURI := connURI("test_database")
	stdArgs := []string{"pkg", "exec", "chef/automate-postgresql", "pg_dump", "--if-exists", "--verbose", "--clean", "--file", "test_database.sql", "--no-privileges", "--no-owner", connURI}

	tests := []struct {
		desc           string
		user           string
		excludedTables []string
		expectedArgs   []string
		cmdErr         error
		expectedErr    bool
		customFormat   bool
	}{
		{"it exports the db with pg_dump", "", []string{}, stdArgs, nil, false, false},
		{"it runs pg_dump with the right --exclude-table options", "", []string{"table1", "table2"},
			[]string{"pkg", "exec", "chef/automate-postgresql", "pg_dump",
				"--if-exists", "--verbose", "--clean", "--file", "test_database.sql", "--no-privileges", "--no-owner",
				"--exclude-table", "table1", "--exclude-table", "table2",
				connURI},
			nil, false, false},
		{"it runs pg_dump without --no-privileges and --no-owner if User is set", "testuser", []string{},
			[]string{"pkg", "exec", "chef/automate-postgresql", "pg_dump", "--if-exists", "--verbose", "--clean", "--file", "test_database.sql", connURI},
			nil, false, false},
		{"it runs pg_dump with -Fc when UseCustomFormat is set to true", "testuser", []string{},
			[]string{"pkg", "exec", "chef/automate-postgresql", "pg_dump", "--if-exists",
				"--verbose", "--clean", "--file", "test_database.fc", "-Fc", connURI},
			nil, false, true},
		{"it returns an error in psql resturns an error", "", []string{}, stdArgs, errors.New("test-error"), true, false},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			exporter, mockExec := setup(t)
			exporter.ExcludedTables = tt.excludedTables
			exporter.User = tt.user
			exporter.UseCustomFormat = tt.customFormat
			mockExec.Expect("Run", command.ExpectedCommand{
				Cmd:  "hab",
				Env:  testExpectedEnv,
				Args: tt.expectedArgs,
			}).Return(tt.cmdErr)
			err := exporter.Export()
			if tt.expectedErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			mockExec.AssertAllCalled()
		})
	}
}

func TestImport(t *testing.T) {
	setup := func(t *testing.T) (pg.DatabaseExporter, *command.MockExecutor, *pg.MockDB, func()) {
		provider, mockDB := NewDBMock()
		mockDB.On("Close").Return(nil)
		pg.CurrentDBProvider = provider

		tmpDataDir, _ := ioutil.TempDir("", "DatabaseExporterTestDir")

		mockExec := command.NewMockExecutor(t)
		exporter := pg.DatabaseExporter{
			Name:        "test_database",
			DataDir:     tmpDataDir,
			ConnInfo:    connInfo,
			CmdExecutor: mockExec,
		}

		cleanup := func() {
			os.RemoveAll(tmpDataDir)
			pg.CurrentDBProvider = pg.DefaultDBProvider
		}

		return exporter, mockExec, mockDB, cleanup
	}
	testExportContent := []byte("--- TEST EXPORT SQL")

	t.Run("returns ErrNoExport if the given database file does not exist", func(t *testing.T) {
		exporter, _, _, cleanup := setup(t)
		defer cleanup()

		err := exporter.Import(true)
		assert.Equal(t, pg.ErrNoExport, err)
	})

	t.Run("it runs an import without creating a user or restoring ownership when no User is set", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)

		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabase", "test_database").Return(nil)

		err := exporter.Import(true)
		require.NoError(t, err)
	})

	t.Run("it runs an import without ON_ERROR_STOP set if onErrorExit parameter is false", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)

		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabase", "test_database").Return(nil)

		err := exporter.Import(false)
		require.NoError(t, err)
	})

	t.Run("it creates a user and creates the DB with an owner when a User is set", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		exporter.User = "testuser"
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)

		mockDB.On("CreateRole", "testuser").Return(nil)
		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabaseWithOwner", "test_database", "testuser").Return(nil)

		err := exporter.Import(true)
		require.NoError(t, err)
	})

	t.Run("it returns an error if CreateRole fails", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		exporter.User = "testuser"
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)

		mockDB.On("CreateRole", "testuser").Return(errors.New("test error"))
		err := exporter.Import(true)
		require.Error(t, err)
	})

	t.Run("it returns an error if DropDatabase fails", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile, "-v", "ON_ERROR_STOP=true",
				connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)
		mockDB.On("DropDatabase", "test_database").Return(errors.New("test error"))
		err := exporter.Import(true)
		require.Error(t, err)
	})

	t.Run("it returns an error if CreateDatabase fails", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)
		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabase", "test_database").Return(errors.New("test error"))
		err := exporter.Import(true)
		require.Error(t, err)
	})

	t.Run("it returns an error if CreateDatabaseWithOwner fails", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exporter.User = "testuser"
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(nil)
		mockDB.On("CreateRole", "testuser").Return(nil)
		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabaseWithOwner", "test_database", "testuser").Return(errors.New("test-error"))
		err := exporter.Import(true)
		require.Error(t, err)
	})

	t.Run("it returns an error if psql command fails", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.sql")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "psql", "-f", exportFile,
				"-v", "ON_ERROR_STOP=true", connURI("test_database")},
			Env: testExpectedEnv,
		}).Return(errors.New("test-error"))
		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabase", "test_database").Return(nil)
		err := exporter.Import(true)
		require.Error(t, err)
	})

	t.Run("it uses pg_restore and the custom file format on when UseCustomFormat is set", func(t *testing.T) {
		exporter, mockExec, mockDB, cleanup := setup(t)
		defer cleanup()
		exportFile := path.Join(exporter.DataDir, "test_database.fc")
		ioutil.WriteFile(exportFile, testExportContent, 0700)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{"pkg", "exec", "chef/automate-postgresql", "pg_restore", "-Fc", "-d",
				connURI("test_database"), "-e", "--no-owner", "--no-acl", exportFile,
			},
			Env: testExpectedEnv,
		}).Return(nil)

		mockDB.On("DropDatabase", "test_database").Return(nil)
		mockDB.On("CreateDatabase", "test_database").Return(nil)

		exporter.UseCustomFormat = true

		err := exporter.Import(true)
		require.NoError(t, err)
	})
}
