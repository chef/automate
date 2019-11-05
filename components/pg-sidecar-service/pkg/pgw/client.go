package pgw

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"

	"github.com/lib/pq"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/platform/pg"
)

// Client represents the Postgres Sidecar Services database connection
type Client struct {
	pg.DB
	info           pg.ConnInfoURI
	platformConfig *platform_config.Config
	dbDatabase     string
	superuser      string
}

// ClientOpt are functional arguments used when creating a new connection
type ClientOpt func(*Client)

// NewClient takes optional functional arguments and attempts to make a
// new client.
func NewClient(opts ...ClientOpt) (*Client, error) {
	var err error

	client := &Client{
		dbDatabase: "postgres",
	}

	for _, opt := range opts {
		opt(client)
	}
	log := client.log()

	client.superuser, err = client.platformConfig.PGSuperUser()
	if err != nil {
		log.WithError(err).Error("failed to lookup pg superuser")
		return nil, err
	}
	connInfo, err := client.platformConfig.GetPGConnInfoURI(client.superuser)
	if err != nil {
		log.WithError(err).Error("failed to get connection info", log)
		return nil, err
	}
	client.info = connInfo

	client.DB, err = pg.Connect(client.info, client.dbDatabase)
	if err != nil {
		log.WithError(err).Error("failed to connect to database", log)
		return nil, err
	}

	err = client.DB.Ping()
	if err != nil {
		log.WithError(err).Error("failed to ping to database")
		if closeErr := client.DB.Close(); closeErr != nil {
			log.WithError(closeErr).Error("failed to close database")
		}
		return nil, err
	}

	log.Debug("database connection established")
	return client, nil
}

// WithDb sets the database connection database
func WithDb(db string) ClientOpt {
	return func(client *Client) {
		client.dbDatabase = db
	}
}

// WithPlatformConfig sets the platform config to use
func WithPlatformConfig(platformConfig *platform_config.Config) ClientOpt {
	return func(client *Client) {
		client.platformConfig = platformConfig
	}
}

const (
	grantAllOnSchemaQuery = `GRANT ALL ON SCHEMA %s TO %s`
	// chownAllInSchemaQuery changes the owner of as many objects
	// as I could figure out how to change in the given schema. We
	// could alter this function to change all objects across all
	// schemas at the expense of some more complicated WHERE
	// clauses.
	//
	// A good way to generate these queries is to inspect the
	// output of the relevant psql command with ECHO_HIDDEN off
	//
	//    > \set ECHO_HIDDEN on
	//    > \dT
	//
	// The following documentation is also helpful:
	//
	// https://www.postgresql.org/docs/9.6/static/catalog-pg-class.html
	// https://www.postgresql.org/docs/9.6/static/catalog-pg-type.html
	// https://www.postgresql.org/docs/9.6/static/catalog-pg-proc.html
	//
	// pg_tables and pg_views are views into pg_catalog.pg_class
	//
	// NOTE(ssd) 2018-09-25: This query makes use of PostgreSQL's
	// format() function. Since that also uses %-notation for
	// variable substitution, we need to make sure they are
	// properly escaped in this string since it is fed through
	// Sprintf in go first. Any %%I or %%s's in this string are
	// intentional and will produce format strings at the
	// postgresql level.
	//
	// TODO(ssd) 2018-09-25: I think we could probably clean this
	// up into 3 selects (pg_type, pg_proc, pg_class) with some
	// CASE statements in the main loop.
	chownAllInSchemaQuery = `
DO
$$
DECLARE t RECORD;
        in_schemaname VARCHAR := %s;
        in_rolename VARCHAR := %s;
BEGIN
  -- TABLES
  FOR t IN
    SELECT tablename FROM pg_tables WHERE schemaname = in_schemaname
  LOOP
    EXECUTE format('ALTER TABLE %%I.%%I OWNER TO %%I', in_schemaname, t.tablename, in_rolename);
  END LOOP;

  -- VIEWS
  FOR t IN
    SELECT viewname FROM pg_views WHERE schemaname = in_schemaname
  LOOP
    EXECUTE format('ALTER VIEW %%I.%%I OWNER TO %%I', in_schemaname, t.viewname, in_rolename);
  END LOOP;

  -- SEQUENCES
  --
  -- All sequences that are related to autoincr columns should already be
  -- covered by the table ownership changes above. This covers
  -- "free-standing" sequences.
  FOR t IN
    SELECT c.relname as "name"
    FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
     WHERE c.relkind IN ('S','')
     AND n.nspname = in_schemaname
     AND pg_catalog.pg_table_is_visible(c.oid)
  LOOP
     EXECUTE format('ALTER SEQUENCE %%I.%%I OWNER TO %%I', in_schemaname, t.name, in_rolename);
  END LOOP;

  -- MATVIEWS
  FOR t IN
    SELECT c.relname as "name"
    FROM pg_catalog.pg_class c
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('m','')
    AND n.nspname = in_schemaname
    AND pg_catalog.pg_table_is_visible(c.oid)
  LOOP
    EXECUTE format('ALTER MATERIALIZED VIEW %%I.%%I OWNER TO %%I', in_schemaname, t.name, in_rolename);
  END LOOP;

  -- TYPES
  FOR t IN
    SELECT pg_catalog.format_type(typ.oid, NULL) AS "name"
    FROM pg_catalog.pg_type typ
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = typ.typnamespace
    -- Filters out "composite" types that represent table rows
    WHERE (typ.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = typ.typrelid))
    -- Filters out "array" types
    AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = typ.typelem AND el.typarray = typ.oid)
    AND n.nspname = in_schemaname
    AND pg_catalog.pg_type_is_visible(typ.oid)
  LOOP
    EXECUTE format('ALTER TYPE %%I.%%I OWNER TO %%I', in_schemaname, t.name, in_rolename);
  END LOOP;

  -- FUNCTIONS (AND AGGREGATES)
  FOR t IN
    SELECT p.proname as "name", pg_get_function_identity_arguments(p.oid) AS "args"
    FROM pg_catalog.pg_proc p
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
    WHERE n.nspname = in_schemaname
    AND pg_catalog.pg_function_is_visible(p.oid)
  LOOP
    EXECUTE format('ALTER FUNCTION %%I.%%I(%%s) OWNER TO %%I', in_schemaname, t.name, t.args, in_rolename);
  END LOOP;
END
$$;
`
	schemaExistsQuery = `SELECT EXISTS(SELECT 1 FROM information_schema.schemata WHERE schema_name = %s)`
)

// AlterRole takes a rolename and role options and
func (client *Client) AlterRole(query *AlterRoleQuery) error {
	var err error
	log := client.log().WithFields(logrus.Fields{
		"action": "alter_role",
		"role":   query.Role,
	})

	if err = client.DB.ExecStatement(query.String()); err != nil {
		log.WithError(err).Error("failed to alter role")
		return err
	}

	log.Info("altered role")
	return nil
}

// Close closes the connection to the database
func (client *Client) Close() error {
	if client.DB != nil {
		return client.DB.Close()
	}

	return nil
}

// CreateDB takes a database name and role and creates a database
func (client *Client) CreateDB(db, role string) error {
	log := client.log().WithFields(logrus.Fields{
		"action": "create_db",
		"db":     db,
		"role":   role,
	})

	if !client.platformConfig.IsExternalPG() {
		// We will not create roles and change passwords for external PG
		if err := client.DB.CreateRole(role); err != nil {
			log.WithError(err).Error("failed to create role")
			return err
		}

		// CreateRole is "idempotent" so it's possible that we have a legacy role
		// that has a password. Remove that just in case to make sure we clean that
		// up.
		log.Infof("Removing password for %s", role)

		if err := client.DB.RemovePassword(role); err != nil {
			log.WithError(err).Error("failed to remove password from role")
			return err
		}
	}

	exists, err := client.DB.DatabaseExists(db)
	if err != nil {
		log.WithError(err).Error("failed to determine database existence")
		return err
	}

	if exists {
		log.Info("Database exists. Asserting correct owner")
		err := client.DB.AlterDatabaseOwner(db, role)
		if err != nil {
			log.WithError(err).Error("failed to alter database ownership")
		}
	} else {
		log.Info("Creating database with owner")
		err = client.DB.CreateDatabaseWithOwner(db, role)
		if err != nil {
			log.WithError(err).Error("failed to create database")
			return err
		}
	}

	if err = client.DB.GrantAll(db, role); err != nil {
		log.WithError(err).Error("failed to grant role onto database")
		return err
	}

	chownClient, err := NewClient(
		WithDb(db),
		WithPlatformConfig(client.platformConfig),
	)

	if err != nil {
		log.WithFields(logrus.Fields{
			"error": err,
			"db":    db,
		}).Error("failed to connect to database")

		return err
	}
	defer fileutils.LogClose(chownClient, log, "failed to close client")

	if err := chownClient.SetPublicSchemaRole(role); err != nil {
		log.WithFields(logrus.Fields{
			"error":  err,
			"schema": "public",
			"db":     db,
			"role":   role,
		}).Error("failed to chown objects in schema")

		return err
	}
	log.Info("created database")
	return nil
}

// CreateExtension takes a database name and role and creates a database
func (client *Client) CreateExtension(ext string) error {
	var err error
	log := client.log().WithFields(logrus.Fields{
		"action": "create_extension",
		"ext":    ext,
	})

	if err = client.DB.CreateExtension(ext); err != nil {
		log.WithError(err).Error("failed to create extension")
		return err
	}

	log.Info("created database extension")
	return nil
}

// DropTables takes a DropTablesQuery and executes it
func (con *Client) DropTables(query *DropTablesQuery) error {
	var err error
	log := con.log().WithFields(logrus.Fields{
		"action":  "drop_tables",
		"tables":  query.Tables,
		"cascade": query.Cascade,
	})

	if err = con.DB.ExecStatement(query.String()); err != nil {
		log.WithError(err).Error("failed to drop tables")
		return err
	}

	log.Info("dropped tables")
	return nil
}

// MigrateTables migrates tables from a given database to another
func (client *Client) MigrateTables(from, to string, tables []string, user string, failIfSrcMissing, skipCreate bool) error {
	var err error
	log := client.log().WithFields(logrus.Fields{
		"action":                  "migrate_tables",
		"origin_db":               from,
		"destination_db":          to,
		"tables":                  strings.Join(tables, ", "),
		"import_user":             user,
		"fail_if_src_db_missing":  failIfSrcMissing,
		"skip_create_destination": skipCreate,
	})

	_, err = exec.LookPath("pg_dump")
	if err != nil {
		log.WithError(err).Error("failed to locate pg_dump in PATH")
		return err
	}

	_, err = exec.LookPath("psql")
	if err != nil {
		log.WithError(err).Error("failed to locate psql in PATH")
		return err
	}

	// The origin database must exist
	exists, err := client.DB.DatabaseExists(from)
	if err != nil {
		log.WithError(err).Error("failed to determine origin database existence")
		return err
	}

	if !exists {
		if failIfSrcMissing {
			log.Error("source database does not exist")
			return ErrSrcDbMissing
		}

		log.Debug("skipped table migration because origin database does not exist")
		return nil
	}

	// The destination database must exist so we can migrate the tables
	exists, err = client.DB.DatabaseExists(to)
	if err != nil {
		log.WithError(err).Error("failed to determine to_db database existence")
		return err
	}

	if skipCreate {
		if !exists {
			log.WithError(err).Error("failed to migrate tables because destination database does not exist")
			return errors.New("failed to migrate tables because destination database does not exist")
		}
	} else {
		if exists {
			log.Debug("skipped table migration because destination database already exists")
			return nil
		}

		// Create the destination database
		if err = client.CreateDB(to, user); err != nil {
			return err
		}
	}

	err = client.migrateTables(to, from, tables)

	// If we created the destination database and we had a failure we will drop the db
	if err != nil && skipCreate {
		log.WithError(err).Error("failed to migrated tables")

		if errDB := client.DB.DropDatabase(to); errDB != nil {
			log.WithError(errDB).Error("failed to drop destination database after failed table migration")
			return errDB
		}

		return err
	}

	// Now we have to connect to the new database and fix the permissions by
	// applying the role ownership to the new database
	client.DB, err = pg.Connect(client.info, to)
	if err != nil {
		log.WithError(err).Error("failed to connect to database", log)
		return err
	}

	err = client.DB.Ping()
	if err != nil {
		log.WithError(err).Error("failed to ping to database")
		return err
	}

	if err = client.SetPublicSchemaRole(user); err != nil {
		return err
	}

	log.Info("migrated tables")
	return nil
}

// RenameDB takes an existing database name and a desired name and renames the
// database if it exists
func (client *Client) RenameDB(to, from string) error {
	var err error
	log := client.log().WithFields(logrus.Fields{
		"action":         "rename_db",
		"origin_db":      from,
		"destination_db": to,
	})

	exists, err := client.DB.DatabaseExists(from)
	if err != nil {
		log.WithError(err).Error("failed to determine database existence")
		return err
	}

	// Keep backwards compatibility here and don't raise an error if the
	// database doesn't exist
	if !exists {
		log.Debug("skipped rename because origin database does not exist")
		return nil
	}

	err = client.DB.RenameDatabase(from, to)
	if err != nil {
		if pqerr, ok := err.(*pq.Error); ok {
			switch pqerr.Code {
			// NOTE(ssd) 2018-09-21: We might also see
			// this in the case of concurrent updates but
			// for now we let this fail rather than warn
			// since if it was the result of a concurrent
			// rename then we should succeed on retry.
			//
			// case "42P04":
			// 	log.Warnf("Unable to rename as to_db already exists")
			// 	return
			case "3D000":
				log.Warnf("Unable to rename database because it does not exist")
				return nil
			}
		}

		log.WithError(err).Error("failed to rename database")
		return err
	}

	log.Info("renamed database")
	return nil
}

// SetPublicSchemaRole change the owner of all tables in the public and sqitch schemas
// to the given role name
func (client *Client) SetPublicSchemaRole(role string) error {
	var err error

	log := client.log().WithFields(logrus.Fields{
		"action": "apply_role",
		"role":   role,
	})

	err = client.grantAllOnSchema(role, "public")
	if err != nil {
		log.WithFields(logrus.Fields{
			"error":  err,
			"schema": "public",
		}).Error("failed to grant role on schema")

		return err
	}

	err = client.chownAllInSchema(role, "public")
	if err != nil {
		log.WithFields(logrus.Fields{
			"error":  err,
			"schema": "public",
		}).Error("failed to chown objects in schema")

		return err
	}

	sqitchExists, err := client.schemaExists("sqitch")
	if err != nil {
		log.WithFields(logrus.Fields{
			"error":  err,
			"schema": "sqitch",
		}).Error("failed to check if sqitch schema exists")

		return err
	}
	if sqitchExists {
		err = client.grantAllOnSchema(role, "sqitch")
		if err != nil {
			log.WithFields(logrus.Fields{
				"error":  err,
				"schema": "sqitch",
			}).Error("failed to grant role on schema")

			return err
		}

		err := client.chownAllInSchema(role, "sqitch")
		if err != nil {
			log.WithFields(logrus.Fields{
				"error":  err,
				"schema": "sqitch",
			}).Error("failed to chown objects in schema")

			return err
		}
	}

	return nil
}

// DeploySqitch takes a connection database name and sqitch directory and runs
// sqitch
func (client *Client) DeploySqitch(db, dir, user string) error {
	var err error

	log := client.log().WithFields(logrus.Fields{
		"action":     "deploy_sqitch",
		"db":         db,
		"sqitch_dir": dir,
		"user":       user,
	})

	_, err = exec.LookPath("sqitch")
	if err != nil {
		log.WithError(err).Error("failed to locate sqitch in PATH")
		return err
	}

	_, err = exec.LookPath("psql")
	if err != nil {
		log.WithError(err).Error("failed to locate psql in PATH")
		return err
	}

	connInfo, err := pg.SuperuserConnInfoFromPlatformConfig(client.platformConfig)
	if err != nil {
		return err
	}
	pgURI := connInfo.ConnURI(db)

	sqitchPGURI := strings.Replace(pgURI, "postgresql://", "db:pg://", 1)

	sqitchArgs := command.Args(
		"--quiet",
		"--top-dir", dir,
		"--engine", "pg",
		"deploy",
		sqitchPGURI,
	)

	environment := connInfo.PsqlCmdOptions()
	cmdArgs := []command.Opt{
		sqitchArgs,
		command.Envvar("TZ", "UTC"),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout),
	}

	cmdArgs = append(cmdArgs, environment...)

	err = command.Run("sqitch", cmdArgs...)
	if err != nil {
		log.WithError(err).Error("failed to deploy sqitch")
		return err
	}

	return nil
}

func (client *Client) log() *logrus.Entry {
	return logrus.WithFields(logrus.Fields{
		"user":        client.superuser,
		"db_database": client.dbDatabase,
		"conn_info":   client.info,
	})
}

func (client *Client) migrateTables(to, from string, tables []string) error {
	var err error

	log := client.log().WithFields(logrus.Fields{
		"action":  "migrate_tables",
		"from_db": from,
		"to_db":   to,
		"tables":  strings.Join(tables, ", "),
	})

	// Stream the SQL dump to the psql restore using a Pipe
	r, w := io.Pipe()

	waitRestore, err := client.startPGRestoreStream(to, r)
	if err != nil {
		return err
	}

	waitDump, err := client.startPGDumpStream(from, tables, w)
	if err != nil {
		return err
	}

	log.Debug("Waiting for dump to finish writing to pipe")
	if err = waitDump(); err != nil {
		log.WithError(err).Error("failed waiting for dump to write to pipe")
		return err
	}

	if err = w.Close(); err != nil {
		log.WithError(err).Error("failed to close write pipe")
		return err
	}

	if err = waitRestore(); err != nil {
		log.WithError(err).Error("failed waiting for restore from pipe")
		return err
	}

	log.Debug("tables migrated")

	return nil
}

func (client *Client) startPGDumpStream(db string, tables []string, w *io.PipeWriter) (func() error, error) {
	var err error

	args := []string{}
	args = append(args, client.info.ConnURI(db), "--no-owner")
	for _, table := range tables {
		args = append(args, "-t", table)
	}

	log := client.log().WithFields(logrus.Fields{
		"action": "pg_dump_to_pipe",
		"db":     db,
		"tables": strings.Join(tables, ", "),
	})

	cmdArgs := []command.Opt{
		command.Args(args...),
		command.Envvar("TZ", "UTC"),
		command.Stderr(os.Stderr),
		command.Stdout(w),
	}

	wait, err := command.Start("pg_dump", cmdArgs...)
	if err != nil {
		log.WithError(err).Error("failed to start pg_dump")
		return wait, err
	}

	return wait, nil
}

func (client *Client) startPGRestoreStream(db string, r *io.PipeReader) (func() error, error) {
	var err error

	log := client.log().WithFields(logrus.Fields{
		"action": "pg_restore_from_pipe",
		"db":     db,
	})

	cmdArgs := []command.Opt{
		command.Args(client.info.ConnURI(db)),
		command.Envvar("TZ", "UTC"),
		command.Envvar("ON_ERROR_STOP", "true"),
		command.Stdin(r),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout),
	}

	wait, err := command.Start("psql", cmdArgs...)
	if err != nil {
		log.WithError(err).Error("failed to start psql")
		return wait, err
	}

	return wait, nil
}

func (client *Client) grantAllOnSchema(role, schema string) error {
	quotedRoleName := pq.QuoteIdentifier(role)
	quotedSchemaName := pq.QuoteIdentifier(schema)
	query := fmt.Sprintf(grantAllOnSchemaQuery, quotedSchemaName, quotedRoleName)
	return client.DB.ExecStatement(query)
}

func renderChownAllInSchemaQuery(role, schema string) string {
	quotedSchemaName := pq.QuoteLiteral(schema)
	quotedOwner := pq.QuoteLiteral(role)
	return fmt.Sprintf(chownAllInSchemaQuery, quotedSchemaName, quotedOwner)
}
func (client *Client) chownAllInSchema(role, schema string) error {
	query := renderChownAllInSchemaQuery(role, schema)
	return client.DB.ExecStatement(query)
}

func (client *Client) schemaExists(schema string) (bool, error) {
	quotedSchema := pq.QuoteLiteral(schema)
	query := fmt.Sprintf(schemaExistsQuery, quotedSchema)
	return client.DB.BoolQuery(query)
}
