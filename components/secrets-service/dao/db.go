package dao

import (
	gosql "database/sql"
	"sort"
	"time"

	"github.com/go-gorp/gorp"
	"github.com/gofrs/uuid"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	logs "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/secrets-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

var validOrderFields = []string{"asc", "desc"}

type DB struct {
	*gorp.DbMap
	SecretsKey string
}

type DBTrans struct {
	*gorp.Transaction
}

func New(conf *config.Postgres, secretsKey string) (*DB, error) {
	if len(secretsKey) != 32 {
		logs.Fatalf("Configured SECRETS_KEY must be 32 characters in length. This system is incorrectly configured with SECRETS_KEY of size: %d chars", len(secretsKey))
	}
	db, err := initDB(conf)
	if err != nil {
		return nil, err
	}

	return &DB{
		DbMap:      db,
		SecretsKey: secretsKey,
	}, nil
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

func initPostgresDB(conf *config.Postgres) (*gorp.DbMap, error) {
	pgURI := conf.ConnectionString

	logs.Debugf("Use PostgreSQL backend %s", pgURI)
	db, err := gosql.Open("postgres", pgURI)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open database with uri: %s", pgURI)
	}

	// Check if the database exists
	err = db.Ping()
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to ping database with uri: %s", pgURI)
	}

	// Run the migrations
	if err := migrator.Migrate(pgURI, conf.MigrationsPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return nil, errors.Wrapf(err, "Unable to complete database migrations")
	}

	return &gorp.DbMap{Db: db, Dialect: gorp.PostgresDialect{}}, nil
}

func initDB(conf *config.Postgres) (*gorp.DbMap, error) {
	dbmap, err := initPostgresDB(conf)
	if err != nil {
		return nil, err
	}
	initTables(dbmap)
	return dbmap, nil
}

func initTables(db *gorp.DbMap) {
	// tell gorp about the tables.
	// set auto-increment to false and tell it about our pk.
	logs.Infof("in initTables -- secrets-service db ")

	db.AddTableWithName(secretTag{}, "s_tags").SetKeys(false, "id")
	db.AddTableWithName(secret{}, "s_secrets").SetKeys(false, "id")
	db.AddTableWithName(SecretTag{}, "s_secrets_tags")
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
	keys := make([]string, 0)
	for k := range mapy {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
