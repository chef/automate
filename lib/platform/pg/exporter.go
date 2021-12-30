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

const Pg13Client = "core/postgresql13-client"

var (
	// DefaultPGDumpCmd is the command we will run for
	// pg_dump.This package-level var is provided so a
	// stub-command can be injected by the tests.
	DefaultPGDumpCmd = []string{"hab", "pkg", "exec", Pg13Client, "pg_dump"}

	// PGRestoreCmd is the command we will run for
	// pg_restore. This package-level var is provided so a
	// stub-command can be injected by the tests.
	DefaultPGRestoreCmd = []string{"hab", "pkg", "exec", Pg13Client, "pg_restore"}

	// PSQLCmd is the command we will run for psql. This
	// package-level var is provided so a stub-command can be
	// injected by the tests.
	DefaultPSQLCmd = []string{"hab", "pkg", "exec", Pg13Client, "psql"}
)

func PGDumpCmd() []string {
	return copySlice(DefaultPGDumpCmd)
}

func PGRestoreCmd() []string {
	return copySlice(DefaultPGRestoreCmd)
}

func PSQLCmd() []string {
	return copySlice(DefaultPSQLCmd)
}

func copySlice(src []string) []string {
	dst := make([]string, len(src))
	copy(dst, src)
	return dst
}

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
	cmd := append(PGDumpCmd(),
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
			command.Envvar("HAB_LICENSE", "accept-no-persist"),
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
	psqlCmd := PSQLCmd()

	source := "stdin"
	if db.Stdin == nil {
		file := db.exportFilePath()
		psqlCmd = append(psqlCmd, "-f", file)
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
			command.Envvar("HAB_LICENSE", "accept-no-persist"),
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
	logrus.Debugf("removing %q", filename)

	if err := os.Remove(filename); err != nil {
		logrus.WithError(err).Errorf("failed to remove %q", filename)
	}
}

type cleanupFunc func()

func (db DatabaseExporter) downloadBackup(stream io.Reader) (filename string, cleanup cleanupFunc, err error) {
	tmpfile, err := ioutil.TempFile(db.TempDir, "pg-restore-db")
	if err != nil {
		return "", nil, errors.Wrap(err, "failed to create db backup buffer file")
	}
	defer tmpfile.Close() // nolint: errcheck
	if _, err := io.Copy(tmpfile, db.Stdin); err != nil {
		return "", nil, errors.Wrap(err, "failed to buffer db backup")
	}

	if err := tmpfile.Close(); err != nil {
		return "", nil, errors.Wrap(err, "failed to flush db backup")
	}

	return tmpfile.Name(), func() { removeFile(tmpfile.Name()) }, nil
}

func (db DatabaseExporter) buildSQLTOC(pgBackupFile string, filters []string) (filename string, cleanup cleanupFunc, err error) {
	pgListFile, err := ioutil.TempFile(db.TempDir, "pg-restore-list")
	if err != nil {
		return "", nil, errors.Wrap(err, "failed to create pg restore list file")
	}
	defer pgListFile.Close() // nolint: errcheck

	stderrListBuff := new(strings.Builder)
	pgListCmd := append(PGRestoreCmd(), filters...)
	pgListCmd = append(pgListCmd, "--list", pgBackupFile)

	reader, writer := io.Pipe()
	ctx, cancel := context.WithCancel(context.TODO())
	defer cancel()
	waitFunc, err := db.CmdExecutor.Start(
		pgListCmd[0],
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Args(pgListCmd[1:]...),
		command.Stderr(stderrListBuff),
		command.Stdout(writer),
		command.Timeout(db.Timeout),
		command.Context(ctx))
	if err != nil {
		return "", nil, errors.Wrapf(err, "failed to list SQL dump TOC from %q, stderr: %s", pgBackupFile, stderrListBuff.String())
	}

	errChan := make(chan error)
	go func() {
		logrus.Debug("Rewriting SQL dump TOC")
		defer close(errChan)
		scanner := bufio.NewScanner(reader)
		for scanner.Scan() {
			txt := scanner.Text()
			if !IsIncompatibleTOCLine(txt) {
				_, err := fmt.Fprintln(pgListFile, txt)
				if err != nil {
					errChan <- err
				}
			} else {
				logrus.Infof("Removing suspected incompatible line from SQL dump TOC: %s", txt)
			}
		}

		if err := scanner.Err(); err != nil {
			errChan <- err
		}
	}()

	if err := waitFunc(); err != nil {
		removeFile(pgListFile.Name())
		return "", nil, errors.Wrapf(err, "failed to list SQL dump TOC from %q, stderr: %s", pgBackupFile, stderrListBuff.String())
	}

	if err := writer.Close(); err != nil {
		removeFile(pgListFile.Name())
		return "", nil, errors.Wrapf(err, "failed to write SQL dump TOC from %q, stderr: %s", pgBackupFile, stderrListBuff.String())
	}

	if err := <-errChan; err != nil {
		removeFile(pgListFile.Name())
		return "", nil, errors.Wrapf(err, "failed to modify SQL dump TOC from %q, stderr: %s", pgBackupFile, stderrListBuff.String())
	}

	return pgListFile.Name(), func() { removeFile(pgListFile.Name()) }, nil
}

// isIncompatibleTOCLine takes in a line from TOC of a pg backup and decides if
// it should be excluded from the restore attempt. Some items in a backup
// cannot be restored in (e.g.) AWS RDS, where the superuser is not all that
// "super."
//
// To gather the data you will likely need to modify this function, you first
// need to make a database dump in the custom format. The command for that is
// of the form `pg_dump DB_URI --format c > database_dump.fc`. To view the TOC
// of that file, use a command like `pg_restore --list database_dump.fc`
//
// If using the habitat dev studio, you can access the postgresql CLI tools via
// `hab pkg exec core/postgresql-client COMMAND`
func IsIncompatibleTOCLine(line string) bool {
	// I think this refers to a TOC line like this:
	//   4; 2615 2200 SCHEMA - public automate
	if strings.Contains(line, "2615 2200") {
		return true
	}
	// This refers to TOC lines like this:
	//   2214; 0 0 COMMENT - SCHEMA public automate
	// AWS RDS superuser cannot run comands like this:
	//   COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';
	if strings.Contains(line, "COMMENT - EXTENSION") {
		return true
	}
	return false
}

func (db DatabaseExporter) restoreCustomFile(exitOnError bool) error {
	filters := []string{"--no-owner", "--no-acl"}

	pgRestoreCmd := append(PGRestoreCmd(), "-Fc", "-d", db.ConnInfo.ConnURI(db.Name))
	if exitOnError {
		pgRestoreCmd = append(pgRestoreCmd, "-e")
	}

	pgRestoreCmd = append(pgRestoreCmd, filters...)
	if db.User != "" {
		pgRestoreCmd = append(pgRestoreCmd, "--role", db.User)
	}

	source := "stdin"
	var pgBackupFile string
	if db.Stdin == nil {
		source = db.exportFilePath()
		pgBackupFile = source

	} else {
		filename, cleanup, err := db.downloadBackup(db.Stdin)
		if err != nil {
			return err
		}
		defer cleanup()
		pgBackupFile = filename
	}

	pgListModifiedFile, cleanup, err := db.buildSQLTOC(pgBackupFile, filters)
	if err != nil {
		return err
	}
	defer cleanup()

	pgRestoreCmd = append(
		pgRestoreCmd, "--use-list", pgListModifiedFile, pgBackupFile)

	stderrBuff := new(strings.Builder)
	err = db.CmdExecutor.Run(
		pgRestoreCmd[0],
		append(db.ConnInfo.PsqlCmdOptions(),
			command.Envvar("HAB_LICENSE", "accept-no-persist"),
			command.Args(pgRestoreCmd[1:]...),
			command.Stderr(stderrBuff),
			command.Timeout(db.Timeout))...)
	if err != nil {
		return errors.Wrapf(err, "failed to import FC format file from %q, stderr: %s", source, stderrBuff.String())
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
