package schema

import (
	"database/sql"
	"net/url"
	"testing"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	// TODO: move code that needs this into lib/db/migrator
	"github.com/golang-migrate/migrate"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available

	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

const schemaDisplaySQL = `
  SELECT table_name, column_name, column_default, is_nullable, data_type
    FROM information_schema.columns
   WHERE table_schema = 'public'
ORDER BY table_name ASC, column_name ASC
;
`

const enumDisplaySQL = `
  SELECT t.typname, e.enumsortorder, e.enumlabel 
    FROM pg_type t, pg_enum e 
   WHERE t.oid = e.enumtypid
ORDER BY typname ASC, enumsortorder ASC;
`

type Postgres struct {
	DbMap        *gorp.DbMap
	dbConn       *sql.DB
	URI          string
	SchemaPath   string
	MaxIdleConns int
	MaxOpenConns int
}

func (db *Postgres) connect() error {
	dbConn, err := libdb.PGOpen(db.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", db.URI)
	}
	db.dbConn = dbConn

	if db.MaxIdleConns > 0 {
		dbConn.SetMaxIdleConns(db.MaxIdleConns)
	}
	if db.MaxOpenConns > 0 {
		dbConn.SetMaxOpenConns(db.MaxOpenConns)
	}
	// Configure the database mapping object
	db.DbMap = &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}}

	// Verify database
	err = db.ping()
	if err != nil {
		return errors.Wrapf(err, "Failed to ping database with uri: %s", db.URI)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (db *Postgres) ping() error {
	return db.dbConn.Ping()
}

func (db *Postgres) migrate() error {
	if err := migrator.Migrate(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}

	return nil
}

func (db *Postgres) clear() error {
	purl, err := addMigrationsTable(db.URI, "")

	m, err := migrate.New(addScheme(db.SchemaPath), purl)
	if err != nil {
		return errors.Wrapf(err, "migrator setup failed for %q", db.URI)
	}
	l := logger.NewLogrusStandardLogger()
	m.Log = migrationLog{Logger: l, verbose: true}
	if err := m.Drop(); err != nil {
		return errors.Wrapf(err, "drop database failed for %q", db.URI)
	}

	c := db.dbConn

	// The implementation of `Drop()` in the migrator only deletes tables and it
	// also ignores the schema_migrations table. We do additional cleanup
	// manually here.
	if _, err := c.Exec("DROP TYPE IF EXISTS rule_event CASCADE;"); err != nil {
		return errors.Wrapf(err, "enum cleanup failed on %q", db.URI)
	}

	if _, err := c.Exec("DROP TYPE IF EXISTS rule_action CASCADE;"); err != nil {
		return errors.Wrapf(err, "enum cleanup failed on %q", db.URI)
	}

	if _, err := c.Exec("DROP TABLE IF EXISTS schema_migrations CASCADE;"); err != nil {
		return errors.Wrapf(err, "schema_migrations cleanup failed on %q", db.URI)
	}

	return nil

}

func (db *Postgres) resetToBaseline() error {
	if err := db.clear(); err != nil {
		return err
	}

	if _, err := db.dbConn.Exec(PgDump1dot0Schema); err != nil {
		return errors.Wrapf(err, "loading pgdump failed for %q", db.URI)
	}

	return nil
}

// TODO: copied from lib/db/migrator. remove!
type migrationLog struct {
	logger.Logger
	verbose bool
}

// TODO: copied from lib/db/migrator. remove!
func (l migrationLog) Verbose() bool {
	return l.verbose
}

// TODO: copied from lib/db/migrator. remove!
func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}

// TODO: copied from lib/db/migrator. remove!
func addMigrationsTable(u, table string) (string, error) {
	pgURL, err := url.Parse(u)
	if err != nil {
		return "", err
	}
	if table != "" {
		q := pgURL.Query()
		q.Set("x-migrations-table", table)
		pgURL.RawQuery = q.Encode()
	}
	return pgURL.String(), nil
}

type PgColumnInfo struct {
	TableName     string  `db:"table_name"`
	ColumnName    string  `db:"column_name"`
	ColumnDefault *string `db:"column_default"`
	IsNullable    string  `db:"is_nullable"`
	DataType      string  `db:"data_type"`
}

type PgEnumInfo struct {
	TypeName      string `db:"typname"`
	EnumSortOrder int    `db:"enumsortorder"`
	EnumLabel     string `db:"enumlabel"`
}

func TestSchemaWhenUpgradingFromVersion1(t *testing.T) {
	p := Postgres{
		URI:        "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt",
		SchemaPath: "/src/components/notifications-service2/pkg/storage/postgres/schema/sql/",
	}
	err := p.connect()
	require.NoError(t, err)

	err = p.resetToBaseline()
	require.NoError(t, err)

	sqlFnNow := "now()"
	sqlValFalse := "false"

	// At this point in the test, we expect have the schema that the 1.0 version
	// of the service created. These assertions check that our test setup worked
	// correctly.
	expectedColumnsBaseline := []*PgColumnInfo{
		&PgColumnInfo{TableName: "migrations", ColumnName: "at", ColumnDefault: nil, IsNullable: "NO", DataType: "timestamp with time zone"},
		&PgColumnInfo{TableName: "migrations", ColumnName: "descr", ColumnDefault: nil, IsNullable: "YES", DataType: "text"},
		&PgColumnInfo{TableName: "migrations", ColumnName: "num", ColumnDefault: nil, IsNullable: "NO", DataType: "integer"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "at", ColumnDefault: &sqlFnNow, IsNullable: "NO", DataType: "timestamp with time zone"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "inbound_id", ColumnDefault: nil, IsNullable: "NO", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "action", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "critical_controls_only", ColumnDefault: &sqlValFalse, IsNullable: "YES", DataType: "boolean"},
		&PgColumnInfo{TableName: "rules", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "id", ColumnDefault: nil, IsNullable: "NO", DataType: "uuid"},
		&PgColumnInfo{TableName: "rules", ColumnName: "name", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
		&PgColumnInfo{TableName: "rules", ColumnName: "secret_id", ColumnDefault: nil, IsNullable: "YES", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "url", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
	}

	columns, err := p.DbMap.Select(&PgColumnInfo{}, schemaDisplaySQL)
	require.NoError(t, err)

	actualColumns := []*PgColumnInfo{}
	for _, c := range columns {
		column := c.(*PgColumnInfo)
		actualColumns = append(actualColumns, column)
	}

	assert.Equal(t, expectedColumnsBaseline, actualColumns)

	expectedEnums := []*PgEnumInfo{
		&PgEnumInfo{TypeName: "rule_action", EnumSortOrder: 1, EnumLabel: "SlackAlert"},
		&PgEnumInfo{TypeName: "rule_action", EnumSortOrder: 2, EnumLabel: "WebhookAlert"},
		&PgEnumInfo{TypeName: "rule_action", EnumSortOrder: 3, EnumLabel: "ServiceNowAlert"},
		&PgEnumInfo{TypeName: "rule_event", EnumSortOrder: 1, EnumLabel: "CCRSuccess"},
		&PgEnumInfo{TypeName: "rule_event", EnumSortOrder: 2, EnumLabel: "CCRFailure"},
		&PgEnumInfo{TypeName: "rule_event", EnumSortOrder: 3, EnumLabel: "ComplianceSuccess"},
		&PgEnumInfo{TypeName: "rule_event", EnumSortOrder: 4, EnumLabel: "ComplianceFailure"},
		&PgEnumInfo{TypeName: "rule_event", EnumSortOrder: 5, EnumLabel: "Assets"},
	}

	enums, err := p.DbMap.Select(&PgEnumInfo{}, enumDisplaySQL)
	require.NoError(t, err)

	actualEnums := []*PgEnumInfo{}

	for _, e := range enums {
		enum := e.(*PgEnumInfo)
		actualEnums = append(actualEnums, enum)
	}

	assert.Equal(t, expectedEnums, actualEnums)

	// Migrate the database from the 1.0 version to the 2.0 version
	err = p.migrate()
	require.NoError(t, err)

	// the migrator itself creates the schema_migrations table and our migration
	// #2 file removes the old migrations table, leaving us with these:
	expectedColumnsAfterUpgrade := []*PgColumnInfo{
		&PgColumnInfo{TableName: "processed_events", ColumnName: "at", ColumnDefault: &sqlFnNow, IsNullable: "NO", DataType: "timestamp with time zone"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "inbound_id", ColumnDefault: nil, IsNullable: "NO", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "action", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "critical_controls_only", ColumnDefault: &sqlValFalse, IsNullable: "YES", DataType: "boolean"},
		&PgColumnInfo{TableName: "rules", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "id", ColumnDefault: nil, IsNullable: "NO", DataType: "uuid"},
		&PgColumnInfo{TableName: "rules", ColumnName: "name", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
		&PgColumnInfo{TableName: "rules", ColumnName: "secret_id", ColumnDefault: nil, IsNullable: "YES", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "url", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
		&PgColumnInfo{TableName: "schema_migrations", ColumnName: "dirty", ColumnDefault: nil, IsNullable: "NO", DataType: "boolean"},
		&PgColumnInfo{TableName: "schema_migrations", ColumnName: "version", ColumnDefault: nil, IsNullable: "NO", DataType: "bigint"},
	}

	actualColumnsAfterUpgrade := []*PgColumnInfo{}
	columnsRes, err := p.DbMap.Select(&PgColumnInfo{}, schemaDisplaySQL)
	require.NoError(t, err)

	for _, c := range columnsRes {
		col := c.(*PgColumnInfo)
		actualColumnsAfterUpgrade = append(actualColumnsAfterUpgrade, col)
	}
	assert.Equal(t, expectedColumnsAfterUpgrade, actualColumnsAfterUpgrade)

	enumsAfter, err := p.DbMap.Select(&PgEnumInfo{}, enumDisplaySQL)
	require.NoError(t, err)

	actualEnumsAfterUpgrade := []*PgEnumInfo{}

	for _, e := range enumsAfter {
		enum := e.(*PgEnumInfo)
		actualEnumsAfterUpgrade = append(actualEnumsAfterUpgrade, enum)
	}

	// expectedEnums is the same as before--we expect them NOT to change.
	assert.Equal(t, expectedEnums, actualEnumsAfterUpgrade)
}

func TestSchemaWhenInstallingFresh(t *testing.T) {
	p := Postgres{
		URI:        "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt",
		SchemaPath: "/src/components/notifications-service2/pkg/storage/postgres/schema/sql/",
	}
	err := p.connect()
	require.NoError(t, err)

	err = p.clear()
	require.NoError(t, err)

	// migrate from empty db to 2.0 version directly
	err = p.migrate()
	require.NoError(t, err)

	sqlFnNow := "now()"
	sqlValFalse := "false"

	// the migrator itself creates the schema_migrations table and our migration
	// #2 file removes the old migrations table, leaving us with these:
	expectedColumnsAfterUpgrade := []*PgColumnInfo{
		&PgColumnInfo{TableName: "processed_events", ColumnName: "at", ColumnDefault: &sqlFnNow, IsNullable: "NO", DataType: "timestamp with time zone"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "processed_events", ColumnName: "inbound_id", ColumnDefault: nil, IsNullable: "NO", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "action", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "critical_controls_only", ColumnDefault: &sqlValFalse, IsNullable: "YES", DataType: "boolean"},
		&PgColumnInfo{TableName: "rules", ColumnName: "event", ColumnDefault: nil, IsNullable: "NO", DataType: "USER-DEFINED"},
		&PgColumnInfo{TableName: "rules", ColumnName: "id", ColumnDefault: nil, IsNullable: "NO", DataType: "uuid"},
		&PgColumnInfo{TableName: "rules", ColumnName: "name", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
		&PgColumnInfo{TableName: "rules", ColumnName: "secret_id", ColumnDefault: nil, IsNullable: "YES", DataType: "character varying"},
		&PgColumnInfo{TableName: "rules", ColumnName: "url", ColumnDefault: nil, IsNullable: "NO", DataType: "text"},
		&PgColumnInfo{TableName: "schema_migrations", ColumnName: "dirty", ColumnDefault: nil, IsNullable: "NO", DataType: "boolean"},
		&PgColumnInfo{TableName: "schema_migrations", ColumnName: "version", ColumnDefault: nil, IsNullable: "NO", DataType: "bigint"},
	}

	actualColumnsAfterUpgrade := []*PgColumnInfo{}
	columnsRes, err := p.DbMap.Select(&PgColumnInfo{}, schemaDisplaySQL)
	require.NoError(t, err)

	for _, c := range columnsRes {
		col := c.(*PgColumnInfo)
		actualColumnsAfterUpgrade = append(actualColumnsAfterUpgrade, col)
	}
	assert.Equal(t, expectedColumnsAfterUpgrade, actualColumnsAfterUpgrade)
}
