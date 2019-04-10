package a1upgrade

import (
	"fmt"
	"os"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

// UsersExportSQL is the statement used to export users data from A1. It returns
// one row of JSON, which we then feed into a file in  local-user-service's data
// directory.
const UsersExportSQL = `WITH t AS
(SELECT u.name, u.first_name, u.last_name, p.hashed_pass
 FROM users AS u NATURAL LEFT OUTER JOIN user_passwords AS p WHERE user_type='internal')
SELECT json_agg(t) FROM t`

// UserRolesExportSQL is the statement used to export user roles data from A1.
// It works analogous to UsersExportSQL above.
const UserRolesExportSQL = `WITH t AS
(SELECT u.name, array_agg(r.role) AS roles
 FROM users AS u
 JOIN enterprise_user_roles AS r
 ON r.user_id=u.id
 WHERE u.user_type='internal'
 GROUP BY u.name)
SELECT json_agg(t) FROM t`

// SAMLConfigSQL returns the existence of any data in the saml_config table
const SAMLConfigSQL = `SELECT EXISTS(SELECT 1 FROM saml_config)`

// PGDumpOutputDir is the path that the pg_dumpall output from A1
// will be written to. This is exposed as a package-level variable so
// that we can reference it from the pg_dumpall stub code.
var PGDumpOutputDir = "/var/opt/delivery/a1_pg_export"

// A1DeliveryDB is the delivery SQL database required to migrate from A1 to A2
// this database has multiple tables that are used by multiple parts of A1, the
// following once are Compliance-related tables.
var A1DeliveryDB = A1Database{
	name: "delivery",
	includedTables: []string{
		"s_tags",
		"s_secrets",
		"s_secrets_tags",
		"agents",
		"node_managers",
		"results",
		"profiles",
		"tags",
		"jobs",
		"jobs_nodes",
		"jobs_profiles",
		"jobs_tags",
		"nodes",
		"nodes_agents",
		"nodes_secrets",
		"nodes_tags",
	},
	required: true,
}

// CSDatabases are the SQL databases required to migrate the All-In-One Chef server
// to A2
var CSDatabases = []A1Database{
	{
		name: "opscode_chef",
		/*
			includedTables: []string{
				"checksums",
				"clients",
				"containers",
				"cookbook_artifact_version_checksums",
				"cookbook_artifact_versions",
				"cookbook_artifacts",
				"cookbook_version_checksums",
				"cookbook_versions",
				"cookbooks",
				"data_bag_items",
				"data_bags",
				"environments",
				"groups",
				"keys",
				"node_policy",
				"nodes",
				"opc_customers",
				"opc_users",
				"org_migration_state",
				"org_user_associations",
				"org_user_invites",
				"orgs",
				"policies",
				"policy_groups",
				"policy_revisions",
				"policy_revisions_policy_groups_association",
				"roles",
				"sandboxed_checksums",
				"users",
			},
		*/
	},
	{
		name: "bifrost",
		/*
			includedTables: []string{
				"actor_acl_actor",
				"actor_acl_group",
				"auth_actor",
				"auth_container",
				"auth_group",
				"auth_object",
				"container_acl_actor",
				"container_acl_group",
				"group_acl_actor",
				"group_acl_group",
				"group_actor_relations",
				"group_group_relations",
				"object_acl_actor",
				"object_acl_group",
			},
		*/
	},
	{
		name: "bookshelf",
		/*
			includedTables: []string{
				"buckets",
				"file_chunks",
				"file_data",
				"files",
			},
		*/
	},
	// pg_dumpall would have captured these tables.
	//
	// We are leaving them un-exported for now since we don't want
	// to handle anything that is out of date.
	//
	// {
	// 	name: "oc_id",
	// },
	// {
	// 	name: "opscode_pushy",
	// },
	// {
	// 	name: "opscode_reporting",
	// },
}

// A1Database represents a SQL database to migration
type A1Database struct {
	name           string
	includedTables []string
	excludedTables []string
	required       bool
}

func (db A1Database) toExporter(connInfo pg.ConnInfo, timeout time.Duration) pg.DatabaseExporter {
	return pg.DatabaseExporter{
		Name:           db.name,
		ExcludedTables: db.excludedTables,
		IncludedTables: db.includedTables,
		ConnInfo:       connInfo,
		DataDir:        PGDumpOutputDir,
		CmdExecutor:    command.NewExecExecutor(),
		Timeout:        timeout,
	}
}

// PsqlAlterUserWithPassword runs a SQL query to set a user's password.
func PsqlAlterUserWithPassword(user, password string) error {
	query := fmt.Sprintf(`ALTER USER "%s" WITH PASSWORD '%s'`, user, password)
	return defaultCommandExecutor.Run("/opt/delivery/embedded/bin/psql",
		command.Args("-c", query, "template1"),
		command.AsUser(user))
}

// BackupA1Postgres takes a backup of A1 postgres
func BackupA1Postgres(connInfo *pg.A1ConnInfo, dbs []A1Database, timeout time.Duration) error {
	_, err := connInfo.InitPgPassfile()
	if err != nil {
		return err
	}
	defer func() {
		err := connInfo.CleanupPgPassfile()
		if err != nil {
			logrus.WithError(err).Warn("Failed to cleanup pgpass file")
		}
	}()

	err = os.MkdirAll(PGDumpOutputDir, 0700)
	if err != nil {
		return errors.Wrap(err, "could not make output directory")
	}

	for _, db := range dbs {
		exporter := db.toExporter(connInfo, timeout)
		exists, err := exporter.Exists()
		if err != nil {
			return errors.Errorf("error looking for PostgreSQL database %q", db.name)
		}

		if !exists {
			if db.required {
				return errors.Errorf("required database %q does not exist", db.name)
			}
			logrus.Debugf("Not backing up database %q because it doesn't exist", db.name)
			continue
		}

		err = exporter.Export()
		if err != nil {
			return errors.Wrapf(err, "failed to export PostgreSQL database %q", db.name)
		}
	}

	return nil
}

// RestoreA1PostgresToA2 restores the A1 postgres dump to A2
func RestoreA1PostgresToA2(connInfo *pg.A2ConnInfo, dbs []A1Database, timeout time.Duration) error {
	for _, db := range dbs {
		exporter := db.toExporter(connInfo, timeout)
		err := exporter.Import(true)
		if err == pg.ErrNoExport {
			if db.required {
				return errors.Wrapf(err, "could not find export of required SQL database %q", db.name)
			}
			logrus.Debugf("skipping import of optional A1 database %q as no export of it exists", db.name)
			continue
		}

		if err != nil {
			return errors.Wrapf(err, "error importing database %q into Automate 2 PostgreSQL instance", db.name)
		}
	}

	return os.RemoveAll(PGDumpOutputDir)
}

// ExportUserData retrieves user data from the Automate 1 PostgreSQL
// database and exports it as JSON for the local-user-service to
// consume at runtime.
func ExportUserData(connInfo *pg.A1ConnInfo, timeout time.Duration) (string, error) {
	conn, err := pg.Connect(connInfo, "delivery")
	if err != nil {
		return "", errors.Wrap(err, "failed to connect to delivery database")
	}
	defer conn.Close()
	return conn.StringQuery(UsersExportSQL)
}

// ExportUserRolesData retrieves roles data from the Automate 1 PostgreSQL
// database and exports it as JSON for the authz-service to consume at runtime.
func ExportUserRolesData(connInfo *pg.A1ConnInfo, timeout time.Duration) (string, error) {
	conn, err := pg.Connect(connInfo, "delivery")
	if err != nil {
		return "", errors.Wrap(err, "failed to connect to delivery database")
	}
	defer conn.Close()
	return conn.StringQuery(UserRolesExportSQL)
}
