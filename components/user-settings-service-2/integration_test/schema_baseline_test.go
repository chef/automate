package integration

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/notifications-service2/pkg/storage/postgres"
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
	p := postgres.Postgres{
		URI:        "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt",
		SchemaPath: "/src/components/notifications-service2/pkg/storage/postgres/schema/sql/",
	}
	err := p.Connect()
	require.NoError(t, err)

	err = p.Clear()
	require.NoError(t, err)
	_, err = p.Exec(PgDump1dot0Schema)
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
	err = p.Migrate()
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
	p := postgres.Postgres{
		URI:        "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt",
		SchemaPath: "/src/components/notifications-service2/pkg/storage/postgres/schema/sql/",
	}
	err := p.Connect()
	require.NoError(t, err)

	err = p.Clear()
	require.NoError(t, err)

	// migrate from empty db to 2.0 version directly
	err = p.Migrate()
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

func TestSchemaIdempotence(t *testing.T) {
	p := postgres.Postgres{
		URI:        "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt",
		SchemaPath: "/src/components/notifications-service2/pkg/storage/postgres/schema/sql/",
	}
	err := p.Connect()
	require.NoError(t, err)
	err = p.DestructiveMigrateForTests()
	require.NoError(t, err)
}
