package pg

import (
	"fmt"

	"github.com/lib/pq"
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
	quotedDBname := pq.QuoteIdentifier(dbname)
	return fmt.Sprintf(createDBQuery, quotedDBname)
}

func CreateDatabaseWithOwnerQuery(dbname string, owner string) string {
	quotedDBname := pq.QuoteIdentifier(dbname)
	quotedOwner := pq.QuoteIdentifier(owner)
	return fmt.Sprintf(createDBWithOwnerQuery, quotedDBname, quotedOwner)
}

func AlterDatabaseOwner(dbname string, owner string) string {
	quotedDBname := pq.QuoteIdentifier(dbname)
	quotedOwner := pq.QuoteIdentifier(owner)
	return fmt.Sprintf(alterDBOwner, quotedDBname, quotedOwner)
}

func DropDatabaseQuery(dbname string) string {
	quotedDBName := pq.QuoteIdentifier(dbname)
	return fmt.Sprintf(dropDBQuery, quotedDBName)
}

func CreateRoleQuery(name string) string {
	quotedRoleName := pq.QuoteLiteral(name)
	quotedRoleIdent := pq.QuoteIdentifier(name)
	return fmt.Sprintf(createRoleQuery, quotedRoleName, quotedRoleIdent)
}

func RemoveRolePasswordQuery(name string) string {
	quotedRoleIdent := pq.QuoteIdentifier(name)
	return fmt.Sprintf(removePWQuery, quotedRoleIdent)
}

func RenameDatabaseQuery(old string, new string) string {
	quotedOld := pq.QuoteIdentifier(old)
	quotedNew := pq.QuoteIdentifier(new)
	return fmt.Sprintf(renameDBQuery, quotedOld, quotedNew)
}

func GrantAllQuery(dbName string, roleName string) string {
	quotedDBName := pq.QuoteIdentifier(dbName)
	quotedRoleName := pq.QuoteIdentifier(roleName)
	return fmt.Sprintf(grantAllQuery, quotedDBName, quotedRoleName)
}

func CreateExtensionQuery(extName string) string {
	quotedExtName := pq.QuoteIdentifier(extName)
	return fmt.Sprintf(createExtensionQuery, quotedExtName)
}
