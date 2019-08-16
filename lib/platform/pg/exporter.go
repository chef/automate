package pg

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
)

// PGDumpCmd is the command we will run for pg_dump. This self-test
// harness replaces this with a stub.
var PGDumpCmd = []string{"hab", "pkg", "exec", "core/postgresql11-client", "pg_dump"}

// PGRestoreCmd is the command we will run for pg_restore.
var PGRestoreCmd = []string{"hab", "pkg", "exec", "core/postgresql11-client", "pg_restore"}

// PSQLCmd is the command we will run for psql.
var PSQLCmd = []string{"hab", "pkg", "exec", "core/postgresql11-client", "psql"}

// DatabaseExporter knows how to export and import a database. See
// Export and Import for further details.
type DatabaseExporter struct {
	DataDir           string
	Name              string
	User              string
	IncludedTables    []string
	ExcludedTables    []string
	ConnInfo          ConnInfo
	CmdExecutor       command.Executor
	Timeout           time.Duration
	DisableRoleCreate bool
	UseCustomFormat   bool
	TempDir           string

	Stdout io.Writer
	Stdin  io.Reader
}

var ErrNoExport = errors.New("No export of the requested database exists")

// Exists returns true if the database already exists in the
// PostgreSQL instance identified by the ConnInfo.
func (db DatabaseExporter) Exists() (bool, error) {
	conn, err := Connect(db.ConnInfo, "postgres")
	if err != nil {
		return false, errors.Wrap(err, "error connecting to PostgreSQL")
	}
	defer conn.Close() // nolint: errcheck

	exists, err := conn.DatabaseExists(db.Name)
	if err != nil {
		return false, errors.Wrap(err, "error querying PostgreSQL")
	}

	return exists, nil
}

// Export uses pg_dump to create a SQL (plain text) export of the
// database in the given DataDir. If no User is present on the
// DatabaseExporter the export will be stripped of ownership
// information. Any tables or views list in ExcludedTables will be
// excluded from the backup. If IncludedTables has been specified
// only tables listed in IncludedTables will be exported.
//
// See https://www.postgresql.org/docs/9.6/static/app-pgdump.html for
// all of the details.
func (db DatabaseExporter) Export() error {
	cmd := append(PGDumpCmd,
		"--if-exists",
		"--verbose",
		"--clean")

	if db.Stdout == nil {
		cmd = append(
			cmd,
			"--file", db.exportFilePath(),
		)
	}

	if db.User == "" && !db.UseCustomFormat {
		cmd = append(cmd,
			"--no-privileges",
			"--no-owner")
	}

	if db.UseCustomFormat {
		cmd = append(cmd, "-Fc")
	}

	for _, t := range db.IncludedTables {
		cmd = append(cmd, "--table", t)
	}

	for _, t := range db.ExcludedTables {
		cmd = append(cmd, "--exclude-table", t)
	}

	cmd = append(cmd, db.ConnInfo.ConnURI(db.Name))

	err := db.CmdExecutor.Run(
		cmd[0],
		append(db.ConnInfo.PsqlCmdOptions(),
			command.Args(cmd[1:]...),
			command.Timeout(db.Timeout),
			command.Stdout(db.Stdout))...)

	if err != nil {
		return errors.Wrapf(err, "error running pg_dump, stderr :%s", command.StderrFromError(err))
	}

	return nil
}

func (db DatabaseExporter) exportFilePath() string {
	if db.UseCustomFormat {
		return path.Join(db.DataDir, fmt.Sprintf("%s.fc", db.Name))
	}
	return path.Join(db.DataDir, fmt.Sprintf("%s.sql", db.Name))
}

// Import imports a SQL (plain text) export of the database from a
// previously created export in the DataDir. ErrNoExport is returned
// if no SQL file for the database can be found in the DataDir. If the
// DatabaseExporter has a User set, the user will be created before
// the import. The database is dropped before import. The exitOnError
// parameter controls whether the SQL import will exit on the first
// error or continue.
func (db DatabaseExporter) Import(exitOnError bool) error {
	logrus.Debugf("starting import of %q into Chef Automate 2 PostgreSQL", db.Name)

	err := db.maybeCreateUser()
	if err != nil {
		return errors.Wrapf(err, "error creating database user %q", db.User)
	}

	if db.Stdin == nil {
		file := db.exportFilePath()
		exists, err := fileutils.PathExists(file)
		if err != nil {
			return errors.Wrapf(err, "error locating export file for database %q", db.Name)
		}

		if !exists {
			return ErrNoExport
		}
	}

	err = db.dropDatabase()
	if err != nil {
		return errors.Wrapf(err, "error dropping database %q", db.Name)
	}

	err = db.createDatabase()
	if err != nil {
		return errors.Wrapf(err, "error creating database %q", db.Name)
	}

	if db.UseCustomFormat {
		err = db.restoreCustomFile(exitOnError)
	} else {
		err = db.restoreSQLFile(exitOnError)
	}
	if err != nil {
		return errors.Wrapf(err, "error importing database %q", db.Name)
	}

	return nil
}

func (db DatabaseExporter) restoreSQLFile(exitOnError bool) error {
	psqlCmd := PSQLCmd

	source := "stdin"
	if db.Stdin == nil {
		file := db.exportFilePath()
		psqlCmd = append(
			psqlCmd,
			"-f", file)
		source = file
	}

	if exitOnError {
		psqlCmd = append(psqlCmd, "-v", "ON_ERROR_STOP=true")
	}

	psqlCmd = append(psqlCmd, db.ConnInfo.ConnURI(db.Name))

	stderrBuff := new(strings.Builder)
	err := db.CmdExecutor.Run(
		psqlCmd[0],
		append(db.ConnInfo.PsqlCmdOptions(),
			command.Args(psqlCmd[1:]...),
			command.Stderr(stderrBuff),
			command.Timeout(db.Timeout),
			command.Stdin(db.Stdin))...)
	if err != nil {
		return errors.Wrapf(err, "failed to import SQL file from %q, stderr: %s", source, stderrBuff.String())
	}
	logrus.WithField("stderr", stderrBuff.String()).Debug("psql import complete")
	return nil
}

func removeFile(filename string) {
	if err := os.Remove(filename); err != nil {
		logrus.WithError(err).Errorf("failed to remove %q", filename)
	}
}

func (db DatabaseExporter) restoreCustomFile(exitOnError bool) error {
	pgRestoreCmd := append(PGRestoreCmd, "-Fc", "-d", db.ConnInfo.ConnURI(db.Name))

	if exitOnError {
		pgRestoreCmd = append(pgRestoreCmd, "-e")
	}

	pgRestoreCmd = append(pgRestoreCmd, "--no-owner", "--no-acl")
	if db.User != "" {
		pgRestoreCmd = append(pgRestoreCmd, "--role", db.User)
	}

	source := "stdin"
	var pgBackupFile string
	if db.Stdin == nil {
		source = db.exportFilePath()
		pgBackupFile = source

	} else {
		tmpfile, err := ioutil.TempFile(db.TempDir, "pg-restore-db")
		if err != nil {
			return errors.Wrap(err, "failed to create db backup buffer file")
		}
		defer removeFile(tmpfile.Name())
		defer tmpfile.Close()
		pgBackupFile = tmpfile.Name()
		if _, err := io.Copy(tmpfile, db.Stdin); err != nil {
			return errors.Wrap(err, "failed to buffer db backup")
		}
	}
	pgListFile, err := ioutil.TempFile(db.TempDir, "pg-restore-list")
	if err != nil {
		return errors.Wrap(err, "failed to create pg restore list file")
	}
	defer removeFile(pgListFile.Name())
	defer pgListFile.Close()

	pgListCmd := append(pgRestoreCmd, "--list")
	stderrListBuff := new(strings.Builder)
	stdoutListBuffReader, stdoutListBuffWriter := io.Pipe()
	defer stdoutListBuffReader.Close()
	defer stdoutListBuffWriter.Close()

	ctx, cancel := context.WithCancel(context.TODO())
	defer cancel()
	waitFunc, err := db.CmdExecutor.Start(
		pgListCmd[0],
		command.Args(pgListCmd[1:]...),
		command.Stdout(stdoutListBuffWriter),
		command.Stderr(stderrListBuff),
		command.Timeout(db.Timeout),
		command.Context(ctx))
	if err != nil {
		return errors.Wrapf(err, "failed to list SQL dump TOC from %q, stderr: %s", source, stderrListBuff.String())
	}
	scanner := bufio.NewScanner(stdoutListBuffReader)
	for scanner.Scan() {
		txt := scanner.Text()
		if !strings.Contains(txt, "2615 2200") {
			_, err := fmt.Fprintln(pgListFile, txt)
			if err != nil {
				return errors.Wrapf(err, "failed to write SQL dump TOC from %q, stderr: %s", source, stderrListBuff.String())
			}
		}
	}

	if err := scanner.Err(); err != nil {
		return errors.Wrapf(err, "failed to read SQL dump TOC from %q, stderr: %s", source, stderrListBuff.String())
	}

	if err := waitFunc(); err != nil {
		return errors.Wrapf(err, "failed to list SQL dump TOC from %q, stderr: %s", source, stderrListBuff.String())
	}

	if err := pgListFile.Close(); err != nil {
		return errors.Wrapf(err, "failed to write SQL dump TOC from %q, stderr: %s", source, stderrListBuff.String())
	}

	pgRestoreCmd = append(
		pgRestoreCmd, "--use-list", pgListFile.Name(), pgBackupFile)

	stderrBuff := new(strings.Builder)
	err = db.CmdExecutor.Run(
		pgRestoreCmd[0],
		append(db.ConnInfo.PsqlCmdOptions(),
			command.Args(pgRestoreCmd[1:]...),
			command.Stderr(stderrBuff),
			command.Timeout(db.Timeout))...)
	if err != nil {
		return errors.Wrapf(err, "failed to import SQL file from %q, stderr: %s", source, stderrBuff.String())
	}
	logrus.WithField("stderr", stderrBuff.String()).Debug("psql import complete")
	return nil
}

func (db DatabaseExporter) maybeCreateUser() error {
	if db.DisableRoleCreate {
		return nil
	}

	if db.User == "" {
		return nil
	}
	conn, err := Connect(db.ConnInfo, "postgres")
	if err != nil {
		return errors.Wrap(err, "error connecting to PostgreSQL")
	}
	defer conn.Close() // nolint: errcheck
	return conn.CreateRole(db.User)
}

func (db DatabaseExporter) dropDatabase() error {
	conn, err := Connect(db.ConnInfo, "postgres")
	if err != nil {
		return errors.Wrap(err, "error connecting to PostgreSQL")
	}
	defer conn.Close() // nolint: errcheck
	return conn.DropDatabase(db.Name)
}

func (db DatabaseExporter) createDatabase() error {
	conn, err := Connect(db.ConnInfo, "postgres")
	if err != nil {
		return errors.Wrap(err, "error connecting to PostgreSQL")
	}
	defer conn.Close() // nolint: errcheck

	if db.User == "" {
		return conn.CreateDatabase(db.Name)
	}
	return conn.CreateDatabaseWithOwner(db.Name, db.User)
}
