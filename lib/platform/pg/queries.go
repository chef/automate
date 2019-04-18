package pg

import (
	"fmt"
	"strings"
)

// Queries that don't support positional arguments and thus have
// provided constructors below.
const (
	createDBQuery          = `CREATE DATABASE %s`
	createDBWithOwnerQuery = `CREATE DATABASE %s OWNER %s`
	dropDBQuery            = `DROP DATABASE IF EXISTS %s`
	createRoleQuery        = `DO
$$
BEGIN
   IF NOT EXISTS (
      SELECT
      FROM   pg_catalog.pg_roles
      WHERE  rolname = %s)
   THEN
      CREATE ROLE %s LOGIN PASSWORD NULL;
   END IF;
END
$$
`
	renameDBQuery        = `ALTER DATABASE %s RENAME TO %s`
	alterDBOwner         = `ALTER DATABASE %s OWNER TO %s`
	removePWQuery        = `ALTER ROLE %s WITH PASSWORD NULL`
	grantAllQuery        = `GRANT ALL ON DATABASE %s TO %s`
	createExtensionQuery = `CREATE EXTENSION IF NOT EXISTS %s`
)

// Queries that support positional arguments and can thus be used
// without constructors.
const (
	DatabaseExistsQuery           = `SELECT EXISTS(SELECT 1 FROM pg_database WHERE datname = $1)`
	ConnectedUserIsSuperuserQuery = `SELECT rolsuper FROM pg_roles WHERE rolname = current_user`
)

func CreateDatabaseQuery(dbname string) string {
	quotedDBname := QuoteIdentifier(dbname)
	return fmt.Sprintf(createDBQuery, quotedDBname)
}

func CreateDatabaseWithOwnerQuery(dbname string, owner string) string {
	quotedDBname := QuoteIdentifier(dbname)
	quotedOwner := QuoteIdentifier(owner)
	return fmt.Sprintf(createDBWithOwnerQuery, quotedDBname, quotedOwner)
}

func AlterDatabaseOwner(dbname string, owner string) string {
	quotedDBname := QuoteIdentifier(dbname)
	quotedOwner := QuoteIdentifier(owner)
	return fmt.Sprintf(alterDBOwner, quotedDBname, quotedOwner)
}

func DropDatabaseQuery(dbname string) string {
	quotedDBName := QuoteIdentifier(dbname)
	return fmt.Sprintf(dropDBQuery, quotedDBName)
}

func CreateRoleQuery(name string) string {
	quotedRoleName := QuoteLiteral(name)
	quotedRoleIdent := QuoteIdentifier(name)
	return fmt.Sprintf(createRoleQuery, quotedRoleName, quotedRoleIdent)
}

func RemoveRolePasswordQuery(name string) string {
	quotedRoleIdent := QuoteIdentifier(name)
	return fmt.Sprintf(removePWQuery, quotedRoleIdent)
}

func RenameDatabaseQuery(old string, new string) string {
	quotedOld := QuoteIdentifier(old)
	quotedNew := QuoteIdentifier(new)
	return fmt.Sprintf(renameDBQuery, quotedOld, quotedNew)
}

func GrantAllQuery(dbName string, roleName string) string {
	quotedDBName := QuoteIdentifier(dbName)
	quotedRoleName := QuoteIdentifier(roleName)
	return fmt.Sprintf(grantAllQuery, quotedDBName, quotedRoleName)
}

func CreateExtensionQuery(extName string) string {
	quotedExtName := QuoteIdentifier(extName)
	return fmt.Sprintf(createExtensionQuery, quotedExtName)
}

// Taken from: https://github.com/lib/pq/pull/718
// which seems close enough to
//
// https://github.com/postgres/postgres/blob/322548a8abe225f2cfd6a48e07b99e2711d28ef7/src/backend/utils/adt/quote.c#L46-L70
//
// to be trustworthy.
//
// QuoteLiteral quotes a string literal to be used as part of an SQL statement.
// It's useful with SQL statements that don't support parametrization.
// For example:
//
// quoted := pq.QuoteLiteral("secret")
// err := db.Exec(fmt.Sprintf("CREATE USER foo PASSWORD %s", quoted))
//
// Any single quotes and backslashes in value will be escaped. If value contains
// at least one backslash, "E" prefix will be prepended.
func QuoteLiteral(value string) string {
	prefix := ""
	if strings.Contains(value, `\`) {
		prefix = "E"
	}
	value = strings.Replace(value, "'", "''", -1)
	value = strings.Replace(value, `\`, `\\`, -1)
	return prefix + "'" + value + "'"
}

// Quote a postgresql identifier. Identifiers are quoted with double
// quotes in postgresql.
//
// The postgresql function that does this
//     https://github.com/postgres/postgres/blob/master/src/backend/utils/adt/ruleutils.c#L10550
//
// keeps identifiers unquoted if they only contain safe
// characters. Here, we just always quote since we use this in places
// where quoted identifiers are always allowed.
func QuoteIdentifier(value string) string {
	value = strings.Replace(value, `"`, `""`, -1)
	return fmt.Sprintf(`"%s"`, value)
}
