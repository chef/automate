package dao

import (
	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	logs "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

type DBTrans struct {
	*gorp.Transaction
}

func New(conf *config.PostgresConfig) (*DB, error) {

	db, err := initDB(conf)
	if err != nil {
		return nil, err
	}

	return &DB{
		DbMap: db,
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

func initPostgresDB(conf *config.PostgresConfig) (*gorp.DbMap, error) {
	pgURI := conf.ConnectionString

	logs.Debugf("Use PostgreSQL backend %s", pgURI)
	db, err := db.PGOpen(pgURI)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open database with uri: %s", pgURI)
	}

	// Check if the database exists
	err = db.Ping()
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to ping database with uri: %s", pgURI)
	}

	// Run the migrations
	logs.Infof("Migration path %s", conf.MigrationsPath)
	if err := migrator.Migrate(pgURI, conf.MigrationsPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return nil, errors.Wrapf(err, "Unable to complete database migrations")
	}

	return &gorp.DbMap{Db: db, Dialect: gorp.PostgresDialect{}}, nil
}

func initDB(conf *config.PostgresConfig) (*gorp.DbMap, error) {
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
	logs.Infof("in initTables -- data-feed-service db ")

	db.AddTableWithName(Destination{}, "destinations").SetKeys(true, "id")
	logs.Infof("completed initTables -- data-feed-service db ")
}
