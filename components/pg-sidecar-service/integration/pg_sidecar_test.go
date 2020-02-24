package pgsidecar_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/gofrs/uuid"
	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/pg_sidecar"
	"github.com/chef/automate/components/pg-sidecar-service/pkg/client"
	"github.com/chef/automate/lib/platform/pg"
)

func TestPGSidecar(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(1)*time.Minute)
	defer cancel()

	pgs, err := client.NewClient(
		client.WithHost("localhost"),
		client.WithPort(10100),
		client.WithTLSCertPath("/hab/svc/pg-sidecar-service/config/service.crt"),
		client.WithTLSKeyPath("/hab/svc/pg-sidecar-service/config/service.key"),
		client.WithTLSRootCAPath("/hab/svc/pg-sidecar-service/config/root_ca.crt"),
	)
	require.NoError(t, err)

	pgh, err := newPgHelper()
	defer pgh.db.Close()

	t.Run("RenameDB when src DB exists", func(t *testing.T) {
		err = pgh.db.CreateDatabase("to_be_renamed")
		require.NoError(t, err)

		_, err = pgs.RenameDB(ctx, &api.RenameDBReq{
			FromDb: "to_be_renamed",
			ToDb:   "new_db",
		})
		require.NoError(t, err)

		exists, err := pgh.db.DatabaseExists("new_db")
		require.NoError(t, err)
		assert.True(t, exists)

		require.NoError(t, pgh.db.DropDatabase("new_db"))
	})

	t.Run("RenameDB when src DB does not exist", func(t *testing.T) {
		_, err = pgs.RenameDB(ctx, &api.RenameDBReq{
			FromDb: "does-not-exist",
			ToDb:   "will-not-exist",
		})
		require.NoError(t, err)
	})

	t.Run("RenameDB when target DB exists", func(t *testing.T) {
		err := pgh.db.CreateDatabase("to_be_renamed")
		require.NoError(t, err)
		err = pgh.db.CreateDatabase("new_db")
		require.NoError(t, err)

		_, err = pgs.RenameDB(ctx, &api.RenameDBReq{
			FromDb: "to_be_renamed",
			ToDb:   "new_db",
		})
		require.Error(t, err)

		exists, err := pgh.db.DatabaseExists("new_db")
		require.NoError(t, err)
		require.True(t, exists)

		exists, err = pgh.db.DatabaseExists("to_be_renamed")
		require.NoError(t, err)
		require.True(t, exists)

		err = pgh.db.DropDatabase("new_db")
		require.NoError(t, err)

		err = pgh.db.DropDatabase("to_be_renamed")
		require.NoError(t, err)
	})

	t.Run("CreateDB creates a db", func(t *testing.T) {
		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
		}()

		exists, err := pgh.db.DatabaseExists("test_database")
		require.NoError(t, err)
		assert.True(t, exists, "table should exist")

		exists, err = pgh.userExists("test_user")
		require.NoError(t, err)
		assert.True(t, exists, "user should exist")
	})

	t.Run("CreateDB when DB already exists", func(t *testing.T) {
		err := pgh.db.CreateDatabase("test_database")
		require.NoError(t, err)

		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		exists, err := pgh.db.DatabaseExists("test_database")
		require.NoError(t, err)
		assert.True(t, exists, "table should exist")

		exists, err = pgh.userExists("test_user")
		require.NoError(t, err)
		assert.True(t, exists, "user should exist")
	})

	t.Run("CreateDB when role already exists", func(t *testing.T) {
		err := pgh.db.CreateRole("test_user")
		require.NoError(t, err)

		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		exists, err := pgh.db.DatabaseExists("test_database")
		require.NoError(t, err)
		assert.True(t, exists, "table should exist")

		exists, err = pgh.userExists("test_user")
		require.NoError(t, err)
		assert.True(t, exists, "user should exist")
	})

	t.Run("MigrateTables src db missing don't fail", func(t *testing.T) {
		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "does-not-exist",
			ToDb:               "will-not-exist",
			Table:              []string{},
			ImportUser:         "new_db",
			FailIfSrcDbMissing: false,
			SkipDbCreate:       false,
		})

		require.NoError(t, err)
	})

	t.Run("MigrateTables fail when src db missing", func(t *testing.T) {
		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "does-not-exist",
			ToDb:               "will-not-exist",
			Table:              []string{},
			ImportUser:         "new_db",
			FailIfSrcDbMissing: true,
			SkipDbCreate:       false,
		})
		require.Error(t, err)

		// Make sure that we're adding the SrcDbMissing error code to the
		// error details
		st := status.Convert(err)
		var details *api.ErrorDetails
		for _, detail := range st.Details() {
			switch t := detail.(type) {
			case *api.ErrorDetails:
				details = t
			}
		}

		require.NotNil(t, details, "error details have no been annotated")
		require.Equal(t, api.ErrorDetails_SrcDbMissing, details.Code)
	})

	t.Run("MigrateTables empty table list migrates all tables", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.db.DropDatabase("new_db")
			pgh.dropRole("test_user")
			pgh.dropRole("new_db")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t1")
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t2")
		require.NoError(t, err)

		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "test_database",
			ToDb:               "new_db",
			Table:              []string{},
			ImportUser:         "new_db",
			FailIfSrcDbMissing: true,
			SkipDbCreate:       false,
		})
		require.NoError(t, err)

		ok, err := pgh.tableExist("new_db", "t1")
		require.NoError(t, err)
		require.True(t, ok)

		ok, err = pgh.tableExist("new_db", "t2")
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("MigrateTables with table list only migrates specified tables", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.db.DropDatabase("new_db")
			pgh.dropRole("test_user")
			pgh.dropRole("new_db")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t1")
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t2")
		require.NoError(t, err)

		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "test_database",
			ToDb:               "new_db",
			Table:              []string{"t2"},
			ImportUser:         "new_db",
			FailIfSrcDbMissing: true,
			SkipDbCreate:       false,
		})
		require.NoError(t, err)

		ok, err := pgh.tableExist("new_db", "t1")
		require.NoError(t, err)
		require.False(t, ok)

		ok, err = pgh.tableExist("new_db", "t2")
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("MigrateTables correctly sets ownership", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.db.DropDatabase("new_db")
			pgh.dropRole("test_user")
			pgh.dropRole("new_user")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t1")
		require.NoError(t, err)

		ok, err := pgh.tableOwnedBy("test_database", "t1", "automate")
		require.NoError(t, err)
		require.True(t, ok)

		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "test_database",
			ToDb:               "new_db",
			Table:              []string{},
			ImportUser:         "new_user",
			FailIfSrcDbMissing: true,
			SkipDbCreate:       false,
		})
		require.NoError(t, err)

		ok, err = pgh.tableExist("new_db", "t1")
		require.NoError(t, err)
		require.True(t, ok)

		ok, err = pgh.tableOwnedBy("new_db", "t1", "new_user")
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("MigrateTables target db does not exist and skip create", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		_, err = pgs.MigrateTables(ctx, &api.MigrateTablesReq{
			FromDb:             "test_database",
			ToDb:               "new_db",
			Table:              []string{},
			ImportUser:         "new_db",
			FailIfSrcDbMissing: true,
			SkipDbCreate:       true,
		})
		require.Error(t, err)
	})

	t.Run("CreateExtension creates extension", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
			pgh.dropExtension("uuid-ossp")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		_, err = pgs.CreateExtension(ctx, &api.CreateExtensionReq{
			Db:  "test_database",
			Ext: "uuid-ossp",
		})
		require.NoError(t, err)

		c, err := pg.Connect(pgh.conInfo, "test_database")
		defer c.Close()
		require.NoError(t, err)

		str, err := c.StringQuery("SELECT uuid_generate_v4()")
		require.NoError(t, err)

		_, err = uuid.FromString(str)
		require.NoError(t, err)
	})

	t.Run("CreateExtension when extension already exists", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
			pgh.dropExtension("uuid-ossp")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		_, err = pgs.CreateExtension(ctx, &api.CreateExtensionReq{
			Db:  "test_database",
			Ext: "uuid-ossp",
		})
		require.NoError(t, err)

		_, err = pgs.CreateExtension(ctx, &api.CreateExtensionReq{
			Db:  "postgres",
			Ext: "uuid-ossp",
		})
		require.NoError(t, err)

		_, err = pgs.CreateExtension(ctx, &api.CreateExtensionReq{
			Db:  "postgres",
			Ext: "uuid-ossp",
		})
		require.NoError(t, err)
	})

	t.Run("SetPublicSchemaRole updates ownership of tables in public schema", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
			pgh.dropRole("new_user")
		}()

		err := pgh.db.CreateRole("new_user")
		require.NoError(t, err)

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		err = pgh.createTable("test_database", "test_table")
		require.NoError(t, err)

		ok, err := pgh.tableOwnedBy("test_database", "test_table", "automate")
		require.NoError(t, err)
		require.True(t, ok)

		_, err = pgs.SetPublicSchemaRole(ctx, &api.SetPublicSchemaRoleReq{
			Db:   "test_database",
			Role: "test_user",
		})
		require.NoError(t, err)

		ok, err = pgh.tableOwnedBy("test_database", "test_table", "test_user")
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("AlterRole updates the role", func(t *testing.T) {
		defer func() {
			pgh.dropRole("test_user")
		}()

		err := pgh.db.CreateRole("test_user")
		require.NoError(t, err)

		ok, err := pgh.hasSuperuser("test_user")
		require.NoError(t, err)
		require.False(t, ok)

		_, err = pgs.AlterRole(ctx, &api.AlterRoleReq{
			Role: "test_user",
			With: &api.AlterRoleReq_Options{
				Superuser: true,
			},
		})
		require.NoError(t, err)

		ok, err = pgh.hasSuperuser("test_user")
		require.NoError(t, err)
		require.True(t, ok)
	})

	t.Run("DropTables with no tables given succeeds", func(t *testing.T) {
		_, err = pgs.DropTables(ctx, &api.DropTablesReq{
			Db:     "test_database",
			Tables: []string{},
		})
		require.NoError(t, err)
	})

	t.Run("DropTables with tables given drops tables", func(t *testing.T) {
		defer func() {
			pgh.db.DropDatabase("test_database")
			pgh.dropRole("test_user")
		}()

		_, err = pgs.CreateDB(ctx, &api.CreateDBReq{
			Db:   "test_database",
			User: "test_user",
		})
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t1")
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t2")
		require.NoError(t, err)

		err = pgh.createTable("test_database", "t3")
		require.NoError(t, err)

		_, err = pgs.DropTables(ctx, &api.DropTablesReq{
			Db:     "test_database",
			Tables: []string{"t1", "t3"},
		})
		require.NoError(t, err)

		ok, err := pgh.tableExist("test_database", "t1")
		require.NoError(t, err)
		require.False(t, ok)

		ok, err = pgh.tableExist("test_database", "t2")
		require.NoError(t, err)
		require.True(t, ok)

		ok, err = pgh.tableExist("test_database", "t3")
		require.NoError(t, err)
		require.False(t, ok)
	})
}

func newPgHelper() (*pgHelper, error) {
	var err error
	pgh := &pgHelper{
		conInfo: &pg.A2ConnInfo{
			User: "automate",
			Host: "localhost",
			Port: 5432,
			Certs: pg.TLSCertPaths{
				Cert:     "/hab/svc/automate-postgresql/config/server.crt",
				Key:      "/hab/svc/automate-postgresql/config/server.key",
				RootCert: "/hab/svc/automate-postgresql/config/root.crt",
			},
		},
	}

	pgh.db, err = pg.Connect(pgh.conInfo, "postgres")
	if err != nil {
		return nil, err
	}

	return pgh, nil
}

type pgHelper struct {
	conInfo *pg.A2ConnInfo
	db      pg.DB
}

func (h *pgHelper) userExists(username string) (bool, error) {
	return h.db.BoolQuery("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = $1)", username)
}

func (h *pgHelper) dropRole(name string) error {
	if err := h.db.ExecStatement(fmt.Sprintf("DROP OWNED BY \"%s\"", name)); err != nil {
		return err
	}
	return h.db.ExecStatement(fmt.Sprintf("DROP ROLE \"%s\"", name))
}

func (h *pgHelper) createTable(db, table string) error {
	c, err := pg.Connect(h.conInfo, db)
	defer c.Close()
	if err != nil {
		return err
	}

	return c.ExecStatement(fmt.Sprintf("CREATE TABLE IF NOT EXISTS %s (i char(1))", table))
}

func (h *pgHelper) tableExist(db, table string) (bool, error) {
	c, err := pg.Connect(h.conInfo, db)
	defer c.Close()
	if err != nil {
		return false, err
	}

	return c.BoolQuery(fmt.Sprintf("SELECT COUNT(to_regclass('%s')) = 1", table))
}

func (h *pgHelper) tableOwnedBy(db, table, role string) (bool, error) {
	c, err := pg.Connect(h.conInfo, db)
	defer c.Close()
	if err != nil {
		return false, err
	}

	tableOwnedByQuery := `
SELECT EXISTS
  ( SELECT *
    FROM pg_tables t
    WHERE t.tableowner = %s
    AND t.tablename = %s
  )
;`

	return c.BoolQuery(fmt.Sprintf(tableOwnedByQuery, pq.QuoteLiteral(role), pq.QuoteLiteral(table)))
}

func (h *pgHelper) hasSuperuser(role string) (bool, error) {
	return h.db.BoolQuery(fmt.Sprintf("SELECT r.rolsuper FROM pg_catalog.pg_roles r WHERE r.rolname = '%s'", role))
}

func (h *pgHelper) dropExtension(ext string) error {
	return h.db.ExecStatement(fmt.Sprintf("DROP EXTENSION \"%s\"", ext))
}
