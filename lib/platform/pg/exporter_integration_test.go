package pg_test

import (
	"database/sql"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"os/user"
	"path"
	"strconv"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

var testConnInfo *pg.A1ConnInfo

var goodSQL = `

--- TEST SQL FOR IMPORT ROUND TRIP TESTS

CREATE TABLE table_one(
     bar text
);

CREATE TABLE table_two(
     bar text
);

CREATE TABLE table_three(
     bar text
);
`

var badSQL = `

--- TEST SQL FOR IMPORT ROUND TRIP TEST

INVALID SQL COMMAND

`

var testSQLUser = "exporter_integration_db_user"
var testConnectionUser = "exporter_integration_test_user"
var testConnectionPassword = "exporter_integration_test_password"

func testSetup(t *testing.T) (string, func()) {
	originalPSQLCmd := pg.DefaultPSQLCmd
	originalPGDumpCmd := pg.DefaultPGDumpCmd
	for _, requiredTool := range []string{"pg_ctl", "initdb", "psql", "pg_isready", "pg_dump"} {
		path, err := exec.LookPath(requiredTool)
		if err != nil {
			t.Skipf("Skipping exporter integration tests because %s was not found on path! To run these tests, you must have PostgreSQL installed", requiredTool)
			return "", func() {}
		}

		switch requiredTool {
		case "psql":
			pg.DefaultPSQLCmd = []string{path}
		case "pg_dump":
			pg.DefaultPGDumpCmd = []string{path}
		}
		t.Logf("Found %s at %s\n", requiredTool, path)
	}

	addr, err := net.ResolveTCPAddr("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("error looking for free local port: %s", err.Error())
	}

	l, err := net.ListenTCP("tcp", addr)
	if err != nil {
		t.Fatalf("error looking for free local port: %s", err.Error())
	}
	l.Close()

	testSQLPort := l.Addr().(*net.TCPAddr).Port
	t.Logf("Pre-start resolved address was: %s", l.Addr().String())
	t.Logf("PostgreSQL listen port: %d", testSQLPort)
	testConnInfo = &pg.A1ConnInfo{
		User: testConnectionUser,
		Pass: testConnectionPassword,
		Port: uint64(testSQLPort),
		Host: "127.0.0.1",
	}

	testConnInfo.InitPgPassfile()

	exporterDataDir, _ := ioutil.TempDir("", "DatabaseExporterIntegrationDBDir")
	ioutil.WriteFile(path.Join(exporterDataDir, "good.sql"), []byte(goodSQL), 0700)
	ioutil.WriteFile(path.Join(exporterDataDir, "bad.sql"), []byte(badSQL), 0700)

	dbDataDir := setupPostgreSQL(t)
	return exporterDataDir, func() {
		pg.DefaultPGDumpCmd = originalPGDumpCmd
		pg.DefaultPSQLCmd = originalPSQLCmd
		testConnInfo.CleanupPgPassfile()
		stopPostgreSQL(dbDataDir)
		os.RemoveAll(dbDataDir)
		os.RemoveAll(exporterDataDir)
	}
}

func setupPostgreSQL(t *testing.T) string {
	var pgUser string
	var userOpt command.Opt
	if os.Geteuid() == 0 {
		pgUser = os.Getenv("PG_USER")
		if pgUser == "" {
			t.Fatalf("PG_USER is not set and you are running as root. We won't be able to start postgresql.")
		}
		t.Logf("Using %s as postgresql user\n", pgUser)
		userOpt = command.AsUser(pgUser)
	}

	dbDataDir, _ := ioutil.TempDir("", "DatabaseExporterIntegrationDBDir")
	if pgUser != "" {
		u, err := user.Lookup(pgUser)
		require.NoError(t, err, "looking up uid/gid for PG_USER")
		uid, err := strconv.Atoi(u.Uid)
		require.NoError(t, err, "converting uid to int")
		gid, err := strconv.Atoi(u.Gid)
		require.NoError(t, err, "converting gid to int")
		err = os.Chown(dbDataDir, uid, gid)
		require.NoError(t, err, "chowning postgresql data dir")
	}

	logFile := path.Join(dbDataDir, "pg.log")
	output, err := command.CombinedOutput("initdb", makeCommandOpts(userOpt, "-D", dbDataDir)...)
	t.Logf("pg_ctl output: %s\n", output)
	require.NoError(t, err, "initdb")
	options := fmt.Sprintf("-h %s -p %d", testConnInfo.Host, testConnInfo.Port)

	output, err = command.CombinedOutput("pg_ctl", makeCommandOpts(userOpt,
		"-D", dbDataDir,
		"-l", logFile,
		"-o", options,
		"start")...)
	t.Logf("pg_ctl output: %s\n", output)
	require.NoError(t, err, "starting PostgreSQL")
	err = waitForPG()
	if err != nil {
		logContent, err := ioutil.ReadFile(logFile)
		if err != nil {
			t.Fatalf("PG failed to start and I could not read the log file %s", logFile)
		}
		t.Fatalf("PG failed to start. Postgresql log content: %s", logContent)
	}

	output, err = command.CombinedOutput("psql", makeCommandOpts(userOpt,
		"-p", PGPortAsStr(),
		"-c", `CREATE ROLE "exporter_integration_test_user" SUPERUSER LOGIN PASSWORD 'exporter_integration_test_password'`,
		"postgres")...)
	t.Logf("create role output: %s", output)
	require.NoError(t, err, "creating test user")

	return dbDataDir
}

func makeCommandOpts(userOpt command.Opt, args ...string) []command.Opt {
	if userOpt != nil {
		return []command.Opt{userOpt, command.Args(args...)}
	}
	return []command.Opt{command.Args(args...)}
}

func PGPortAsStr() string {
	return strconv.Itoa(int(testConnInfo.Port))
}

func waitForPG() error {
	maxAttempts := 5
	for i := 0; i < maxAttempts; i++ {
		err := command.Run("pg_isready", command.Args("-p", PGPortAsStr()))
		if err == nil {
			return nil
		}
		time.Sleep(1 * time.Second)
	}
	return errors.New("timeout")
}

func stopPostgreSQL(dir string) {
	command.Run("pg_ctl", command.Args("-D", dir, "stop"))
}

// TODO(ssd) 2018-09-19: These tests are incomplete, much of the
// interface is tested in the exporter tests below.
func TestDBInterface(t *testing.T) {
	_, cleanup := testSetup(t)
	defer cleanup()

	t.Run("Ping returns no error if the database connection is good", func(t *testing.T) {
		db, err := pg.Connect(testConnInfo, "postgres")
		require.NoError(t, err)
		defer db.Close()

		err = db.Ping()
		assert.NoError(t, err)
	})

	t.Run("Ping returns no error if the database connection is good", func(t *testing.T) {
		db, err := pg.Connect(testConnInfo, "definitely_not_a_real_database_right")
		require.NoError(t, err)
		defer db.Close()

		err = db.Ping()
		assert.Error(t, err)
	})

	t.Run("RenameDatabase can rename an existing database", func(t *testing.T) {
		db, err := pg.Connect(testConnInfo, "postgres")
		require.NoError(t, err)
		defer db.Close()

		err = db.CreateDatabase("test_rename_source")
		require.NoError(t, err)
		err = db.RenameDatabase("test_rename_source", "test_rename_target")
		require.NoError(t, err)

		oldExists, err := db.DatabaseExists("test_rename_source")
		require.NoError(t, err)
		newExists, err := db.DatabaseExists("test_rename_target")
		require.NoError(t, err)

		assert.True(t, newExists)
		assert.False(t, oldExists)
	})

	t.Run("StringQuery runs a query that returns a single string", func(t *testing.T) {
		db, err := pg.Connect(testConnInfo, "postgres")
		require.NoError(t, err)
		defer db.Close()

		actualRet, err := db.StringQuery("SELECT 'test return value' as res")
		require.NoError(t, err)
		assert.Equal(t, "test return value", actualRet)
	})
}

func TestExporter(t *testing.T) {
	dir, cleanup := testSetup(t)
	defer cleanup()

	cleanupGoodDB := func(t *testing.T) {
		db, err := pg.Connect(testConnInfo, "postgres")
		require.NoError(t, err)
		db.DropDatabase("good")
		dropRole(testSQLUser, t)
		ioutil.WriteFile(path.Join(dir, "good.sql"), []byte(goodSQL), 0700)
	}

	t.Run("Exists returns false for a database that doesn't exist", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			Name:        "definitely_should_not_exist_are_you_mad",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		exists, err := exporter.Exists()
		require.NoError(t, err)
		assert.False(t, exists)
	})

	t.Run("Exists returns true for a database that exists", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			Name:        "template1",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		exists, err := exporter.Exists()
		require.NoError(t, err)
		assert.True(t, exists)
	})

	t.Run("Import ignores psql error when exitOnError is false", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			Name:        "bad",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		err := exporter.Import(false)
		defer func() {
			db, err := pg.Connect(testConnInfo, "postgres")
			require.NoError(t, err)
			db.DropDatabase("bad")
		}()
		require.NoError(t, err)
	})

	t.Run("Import doesn't ignore psql error when exitOnError is true", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			Name:        "bad",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		err := exporter.Import(true)
		defer func() {
			db, err := pg.Connect(testConnInfo, "postgres")
			require.NoError(t, err)
			db.DropDatabase("bad")
		}()
		require.Error(t, err)
	})

	t.Run("Import creates a user when User is set", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			User:        testSQLUser,
			Name:        "good",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		err := exporter.Import(true)
		require.NoError(t, err)

		db, err := pg.Connect(testConnInfo, "postgres")
		require.NoError(t, err)
		defer func() {
			db.DropDatabase("good")
			dropRole(testSQLUser, t)
		}()

		exists, err := userExists(testSQLUser)
		require.NoError(t, err)
		assert.True(t, exists)
	})

	t.Run("Import/Export round trip", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:     dir,
			User:        testSQLUser,
			Name:        "good",
			ConnInfo:    testConnInfo,
			CmdExecutor: command.NewExecExecutor(),
		}
		err := exporter.Import(true)
		require.NoError(t, err)
		err = exporter.Export()
		require.NoError(t, err)
		err = exporter.Import(true)
		require.NoError(t, err)

		defer cleanupGoodDB(t)

		exists, err := userExists(testSQLUser)
		require.NoError(t, err)
		assert.True(t, exists)
		exists, err = tableExists("good", "table_one")
		require.NoError(t, err)
		assert.True(t, exists)
		exists, err = tableExists("good", "table_two")
		require.NoError(t, err)
		assert.True(t, exists)
		exists, err = tableExists("good", "table_three")
		require.NoError(t, err)
		assert.True(t, exists)

	})

	t.Run("Import with an excluded table", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:        dir,
			User:           testSQLUser,
			Name:           "good",
			ConnInfo:       testConnInfo,
			CmdExecutor:    command.NewExecExecutor(),
			ExcludedTables: []string{"table_two"},
		}

		err := exporter.Import(true)
		require.NoError(t, err)
		err = exporter.Export()
		require.NoError(t, err)
		err = exporter.Import(true)
		require.NoError(t, err)

		defer cleanupGoodDB(t)

		exists, err := tableExists("good", "table_one")
		require.NoError(t, err)
		assert.True(t, exists)
		exists, err = tableExists("good", "table_two")
		require.NoError(t, err)
		assert.False(t, exists)
		exists, err = tableExists("good", "table_three")
		require.NoError(t, err)
		assert.True(t, exists)
	})

	t.Run("Import with an included table", func(t *testing.T) {
		exporter := pg.DatabaseExporter{
			DataDir:        dir,
			User:           testSQLUser,
			Name:           "good",
			ConnInfo:       testConnInfo,
			CmdExecutor:    command.NewExecExecutor(),
			IncludedTables: []string{"table_two"},
		}
		err := exporter.Import(true)
		require.NoError(t, err)
		err = exporter.Export()
		require.NoError(t, err)
		err = exporter.Import(true)
		require.NoError(t, err)

		defer cleanupGoodDB(t)

		exists, err := tableExists("good", "table_one")
		require.NoError(t, err)
		assert.False(t, exists)
		exists, err = tableExists("good", "table_two")
		require.NoError(t, err)
		assert.True(t, exists)
		exists, err = tableExists("good", "table_three")
		require.NoError(t, err)
		assert.False(t, exists)
	})
}

func dropRole(name string, t *testing.T) {
	db, err := sql.Open("postgres", testConnInfo.ConnURI("postgres"))
	if err != nil {
		t.Logf("Failed to drop test user! %s", err.Error())
		return
	}
	defer db.Close()
	_, err = db.Exec(fmt.Sprintf("DROP ROLE \"%s\"", name))
	if err != nil {
		t.Logf("Failed to drop test user! %s", err.Error())
	}
}

func tableExists(dbname string, tabname string) (bool, error) {
	sql := `
SELECT EXISTS (
   SELECT 1
   FROM   pg_catalog.pg_class
   WHERE  relname = $1
   AND    relkind = 'r'
);
`
	return boolQuery(dbname, sql, tabname)
}

func userExists(username string) (bool, error) {
	return boolQuery("postgres", "SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = $1)", username)
}

func boolQuery(dbname string, query string, args ...interface{}) (bool, error) {
	db, err := pg.Connect(testConnInfo, dbname)
	if err != nil {
		return false, err
	}
	defer db.Close()

	return db.BoolQuery(query, args...)
}
