package postgres

import (
	gosql "database/sql"
	"sort"
	"time"

	"github.com/go-gorp/gorp"
	"github.com/gofrs/uuid"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

type DBTrans struct {
	*gorp.Transaction
}

var validOrderFields = []string{"asc", "desc"}

func New(conf *config.Postgres) (*DB, error) {
	connectionString := conf.ConnectionString
	migrationsPath := conf.MigrationsPath

	db, err := InitDB(connectionString)
	if err != nil {
		return nil, err
	}

	err = runMigrations(connectionString, migrationsPath)
	if err != nil {
		return nil, err
	}

	return db, nil
}

// Transact wraps your calls in a transaction. If the call should fail with an error it will
// perform a rollback. Otherwise the transaction will be committed.
func Transact(db *DB, txFunc func(*DBTrans) error) error {
	trans, err := db.Begin()
	if err != nil {
		return errors.Wrap(err, "Unable to start transaction.")
	}
	tx := DBTrans{
		Transaction: trans,
	}
	defer func() {
		if err != nil {
			tx.Rollback() // nolint: errcheck
		} else {
			err = tx.Commit()
			if err != nil {
				tx.Rollback() // nolint: errcheck
				err = errors.Wrap(err, "Transaction failed and will be rolled back.")
			}
		}
	}()

	err = txFunc(&tx)
	return err
}

func InitDB(connectionString string) (*DB, error) {
	logrus.Debugf("Use PostgreSQL backend %s", connectionString)
	sql, err := gosql.Open("postgres", connectionString)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open database with uri: %s", connectionString)
	}

	// Check if the database exists
	err = sql.Ping()
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to ping database with uri: %s", connectionString)
	}

	db := &DB{
		DbMap: &gorp.DbMap{
			Db:      sql,
			Dialect: gorp.PostgresDialect{},
		},
	}

	initTables(db.DbMap)

	return db, nil
}

func runMigrations(connectionString string, migrationsPath string) error {
	if err := migrator.Migrate(connectionString, migrationsPath,
		logger.NewLogrusStandardLogger(), false,
	); err != nil {
		return errors.Wrapf(err, "Unable to complete database migrations")
	}
	return nil
}

func initTables(db *gorp.DbMap) {
	// tell gorp about the tables.
	// set auto-increment to false and tell it about our pk.
	db.AddTableWithName(server{}, "servers").SetKeys(false, "id")
	db.AddTableWithName(org{}, "orgs").SetKeys(false, "id")
}

func createUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

func valueOrDefaultStr(val string, def string) string {
	if val == "" {
		return def
	}
	return val
}
func valueOrDefaultInt(val int32, def int32) int32 {
	if val == 0 {
		return def
	}
	return val
}

func timeNowRef() time.Time {
	return time.Now().UTC().Round(time.Second)
}

func getMapKeys(mapy map[string]string) []string {
	keys := make([]string, 0, len(mapy))
	for k := range mapy {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
